#include "typing.hpp"

#include <optional>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "span.hpp"
#include "types.hpp"

namespace yal {

struct Var {
    std::string name;
    TypeHandle  type;
    TypeHandle  inner_type;
    Span        where;
};

struct Context {
    auto new_child() -> Context { return {.env = {}, .parent = this}; }

    auto lookup(std::string const& name) -> Var const* {
        for (auto const& var : env) {
            if (var.name == name) return &var;
        }

        return parent ? parent->lookup(name) : nullptr;
    }

    void define(Var const& value) { env.push_back(value); }

    std::vector<Var> env;
    Context*         parent = nullptr;
};

struct Typing {
    auto add_types(Context& ctx, NodeHandle n) -> TypeHandle {
        auto node = ast->get_mut(n);
        er->report_note(node->span, "pass_add_types: {}", node->kind);

        switch (node->kind) {
            case NodeKind::Err: return node->set_type(ts->get_type_err());
            case NodeKind::Nil:
                er->report_bug(node->span,
                               "found nil AST node in typecheck: {}",
                               ast->fatten(n));
                return node->set_type(ts->get_type_err());

            case NodeKind::File: {
                auto c = ctx.new_child();

                for (auto child : ast->get_children(node)) {
                    add_types(c, child);
                }

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::Func: {
                auto c = ctx.new_child();
                auto f = node->as_func(*ast);

                auto name_node = ast->get(f.name);
                if (!name_node->is_id()) {
                    er->report_bug(
                        name_node->span,
                        "bound functions have not been implemented yet");
                    return node->set_type(ts->get_type_err());
                }

                std::vector<TypeHandle> args;
                for (auto& arg : f.args) args.push_back(add_types(c, arg));

                TypeHandle ret;
                if (ast->get(f.ret)->is_nil()) {
                    ret = ts->get_type_void();
                } else {
                    add_types(c, f.ret);
                    ret = eval_to_type(c, f.ret);
                }

                add_types(c, f.body);

                return node->set_type(ts->get_type_fn(args, ret));
            }

            case NodeKind::FuncArg: {
                auto name = node->value_string();

                auto type = add_types(ctx, node->first);
                if (auto t = ts->get(type); !t->is_type()) {
                    er->report_error(node->span,
                                     "can't use non-type {} as type", *t);
                    return node->set_type(ts->get_type_err());
                }

                auto real_type = eval_to_type(ctx, node->first);

                ctx.define({.name = name,
                            .type = real_type,
                            .inner_type = ts->get_type_err(),
                            .where = node->span});

                return node->set_type(real_type);
            }

            case NodeKind::FuncRetPack: {
                auto children = ast->get_children(node);

                std::vector<TypeHandle> types;
                for (auto& child : children) {
                    types.push_back(add_types(ctx, child));
                }

                return node->set_type(ts->get_type_pack(types));
            }

            case NodeKind::Block: {
                auto c = ctx.new_child();

                auto children = ast->get_children(node);
                for (auto& child : children) {
                    add_types(c, child);
                }

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::VarDecl:
            case NodeKind::DefDecl:
            case NodeKind::ExprStmt: break;

            case NodeKind::ReturnStmt: {
                if (!ast->get(node->first)->is_nil()) {
                    auto v = add_types(ctx, node->first);
                    er->report_note(node->span,
                                    "checking the return type has not been "
                                    "implemented ({})",
                                    ts->fatten(v));
                }

                // FIXME: check the return type

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::IfStmt:
            case NodeKind::IfStmtWithElse:
            case NodeKind::IfStmtWithDecl:
            case NodeKind::IfStmtWithDeclAndElse:
            case NodeKind::WhileStmt:
            case NodeKind::Assign:
            case NodeKind::Break:
            case NodeKind::Defer:
            case NodeKind::LogicOr:
            case NodeKind::LogicAnd:
            case NodeKind::BinOr:
            case NodeKind::BinXor:
            case NodeKind::BinAnd:
            case NodeKind::Equal:
            case NodeKind::NotEqual:
            case NodeKind::Greater:
            case NodeKind::GreaterEqual:
            case NodeKind::Smaller:
            case NodeKind::SmallerEqual:
            case NodeKind::ShftLeft:
            case NodeKind::ShftRight:
            case NodeKind::Add:
            case NodeKind::Sub:
            case NodeKind::Mul:
            case NodeKind::Div:
            case NodeKind::Mod:
            case NodeKind::Cast:
            case NodeKind::OrElse:
            case NodeKind::OrReturn:
            case NodeKind::AddrOf:
            case NodeKind::LogicNot:
            case NodeKind::BinNot:
            case NodeKind::Plus:
            case NodeKind::Neg:
            case NodeKind::Optional:
            case NodeKind::Ptr:
            case NodeKind::MultiPtr:
            case NodeKind::SlicePtr:
            case NodeKind::Array:
            case NodeKind::ArrayType:
            case NodeKind::ArrayAutoLen:
            case NodeKind::Deref:
            case NodeKind::Call:
            case NodeKind::Field: break;

            case NodeKind::ExprPack: {
                auto children = ast->get_children(node);

                std::vector<TypeHandle> types;
                for (auto& child : children) {
                    types.push_back(add_types(ctx, child));
                }

                return node->set_type(ts->get_type_pack(types));
            }

            case NodeKind::EnumLit:
            case NodeKind::IdPack: break;
            case NodeKind::Int: {
                // FIXME: use an untyped integer
                return node->set_type(ts->get_type_i32());
            }

            case NodeKind::Id: {
                auto v = ctx.lookup(node->value_string());
                if (!v) {
                    er->report_error(node->span, "undefined identifier: '{}'",
                                     node->value_string());
                    return node->set_type(ts->get_type_err());
                }

                return node->set_type(v->type);
            }

            case NodeKind::Str:
                //
                break;
        }

        throw std::runtime_error{"not implemented"};
    }

    auto eval_to_type(Context& ctx, NodeHandle h) const -> TypeHandle {
        return eval_to_type(ctx, *ast->get(h));
    }

    auto eval_to_type(Context& ctx, Node const& node) const -> TypeHandle {
        switch (node.kind) {
            case NodeKind::Id: {
                auto v = ctx.lookup(node.value_string());
                if (!v) {
                    er->report_error(node.span, "undefined identifier: '{}'",
                                     node.value_string());
                    return ts->get_type_err();
                }

                auto type = ts->get(v->type);
                if (!type->is_type()) {
                    er->report_error(node.span, "can't use non-type {} as type",
                                     *type);
                    return ts->get_type_err();
                }

                return v->inner_type;
            }

            case NodeKind::FuncRetPack: {
                auto children = ast->get_children(node);

                std::vector<TypeHandle> items;
                for (auto const& item : children) {
                    items.push_back(eval_to_type(ctx, item));
                }

                return ts->get_type_pack(items);
            }

            default:
                er->report_error(node.span, "can't evaluate {} to a type",
                                 node.kind);
                return ts->get_type_err();
        }
    }

    Ast*           ast;
    TypeStore*     ts;
    ErrorReporter* er;
};

auto pass_add_types(NodeHandle n, Ast& ast, TypeStore& ts, ErrorReporter& er)
    -> TypeHandle {
    auto t = Typing{.ast = &ast, .ts = &ts, .er = &er};
    auto ctx = Context{};

    ctx.define({.name = "i32",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_i32(),
                .where = {}});

    ctx.define({.name = "void",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_void(),
                .where = {}});

    return t.add_types(ctx, n);
}

}  // namespace yal
