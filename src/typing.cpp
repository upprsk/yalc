#include "typing.hpp"

#include <optional>
#include <ranges>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/ranges.h"
#include "span.hpp"
#include "types.hpp"

namespace yal {

struct Func {
    std::string name;
    TypeHandle  type;
    Span        ret_span;
};

struct Var {
    std::string name;
    TypeHandle  type;
    TypeHandle  inner_type;
    Span        where;
    bool        is_const;
};

struct Context {
    auto new_child() -> Context {
        return {
            .env = {},
            .parent = this,
            ._current_function = _current_function,
        };
    }

    auto new_child_for_func(Func const& f) -> Context {
        return {
            .env = {},
            .parent = this,
            ._current_function = &f,
        };
    }

    [[nodiscard]] auto current_func() const -> Func const* {
        if (!_current_function) throw std::runtime_error{"no current func"};

        return _current_function;
    }

    auto lookup(std::string const& name) -> Var const* {
        for (auto const& var : env | std::ranges::views::reverse) {
            if (var.name == name) return &var;
        }

        return parent ? parent->lookup(name) : nullptr;
    }

    void define(Var const& value) { env.push_back(value); }

    std::vector<Var> env;
    Context*         parent = nullptr;
    Func const*      _current_function;
};

struct Typing {
    auto add_types(Context& ctx, NodeHandle n) -> TypeHandle {
        auto node = ast->get_mut(n);
        // er->report_debug(node->span, "pass_add_types: {}", node->kind);

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

                auto const& name = name_node->value_string();

                std::vector<TypeHandle> args;
                for (auto& arg : f.args) args.push_back(add_types(c, arg));

                TypeHandle ret;
                if (ast->get(f.ret)->is_nil()) {
                    ret = ts->get_type_void();
                } else {
                    add_types(c, f.ret);
                    ret = eval_to_type(c, f.ret);
                }

                auto type = ts->get_type_fn(args, ret);
                auto bc =
                    c.new_child_for_func({.name = name,
                                          .type = type,
                                          .ret_span = ast->get(f.ret)->span});
                add_types(bc, f.body);

                ctx.define({.name = name,
                            .type = type,
                            .inner_type = ts->get_type_err(),
                            .where = node->span,
                            .is_const = true});

                return node->set_type(type);
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
                            .where = node->span,
                            .is_const = false});

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

            case NodeKind::VarDecl: {
                // in case we have an initializer, the type annotation is
                // optional
                auto decl = node->as_var_decl(*ast);

                // check for multiple returns
                std::vector<std::string_view> ids;
                if (ast->get(decl.ids)->is_id_pack()) {
                    for (auto const& id : ast->get_children(decl.ids))
                        ids.push_back(ast->get(id)->value_string());
                } else {
                    ids.push_back(ast->get(decl.ids)->value_string());
                }

                if (!ast->get(decl.init)->is_nil()) {
                    // in case we have a type, type and eval here
                    TypeHandle h;
                    if (!ast->get(decl.type)->is_nil()) {
                        auto c = ctx.new_child();
                        add_types(c, decl.type);

                        h = eval_to_type(c, decl.type);
                    }

                    // type the initializer and check
                    auto v = add_types(ctx, decl.init);
                    if (h.is_valid()) {
                        if (v != h) {
                            er->report_error(
                                node->span,
                                "type mismatch, declaration expects "
                                "{}, but initializer has type: {}",
                                ts->fatten(h), ts->fatten(v));
                        }
                    } else {
                        h = v;
                    }

                    if (ts->get(h)->is_pack()) {
                        auto recved = ts->get_children(h);
                        if (ids.size() != recved.size()) {
                            er->report_error(
                                node->span,
                                "return count mismatch, declaration has {} "
                                "receiver(s) but call returns {} values",
                                ids.size(), recved.size());

                            er->report_note(ast->get(decl.ids)->span,
                                            "got {} identifiers here",
                                            ids.size());

                            er->report_note(
                                ast->get(decl.init)->span, "call returns ({})",
                                fmt::join(
                                    std::ranges::views::transform(
                                        recved,
                                        [&](auto h) { return ts->fatten(h); }),
                                    ", "));
                        }

                        // NOTE: don't check that any of the types in the packs
                        // are zero-width
                    }

                    else if (ts->get(h)->size(*ts) == 0) {
                        er->report_error(node->span,
                                         "using zero-width value of type {} in "
                                         "variable declaration",
                                         ts->fatten(h));
                    }

                    for (auto const& name : ids) {
                        ctx.define({.name = std::string{name},
                                    .type = v,
                                    .inner_type = ts->get_type_err(),
                                    .where = node->span,
                                    .is_const = false});
                    }

                    return node->set_type(h);
                }

                throw std::runtime_error{"not implemented"};
            }

            case NodeKind::DefDecl: break;
            case NodeKind::ExprStmt: {
                auto expr = add_types(ctx, node->first);
                if (!ts->get(expr)->is_void()) {
                    er->report_error(node->span,
                                     "discarding expression result of type: {}",
                                     ts->fatten(expr));
                }

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::ReturnStmt: {
                auto type = ts->get_type_void();

                if (!ast->get(node->first)->is_nil()) {
                    type = add_types(ctx, node->first);
                }

                auto curr_func = ctx.current_func();
                auto func_type = ts->get(curr_func->type)->as_func(*ts);
                if (func_type.ret != type) {
                    er->report_error(node->span,
                                     "type mismatch, function expects {}, but "
                                     "return has type: {}",
                                     ts->fatten(func_type.ret),
                                     ts->fatten(type));
                    er->report_note(curr_func->ret_span,
                                    "return type declared here");
                }

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::IfStmt:
            case NodeKind::IfStmtWithElse:
            case NodeKind::IfStmtWithDecl:
            case NodeKind::IfStmtWithDeclAndElse:
            case NodeKind::WhileStmt: break;

            case NodeKind::Assign: {
                auto lhs = add_types(ctx, node->first);
                auto rhs = add_types(ctx, node->second);

                if (!ast->get(node->first)->is_lvalue()) {
                    er->report_error(node->span, "can't assign of non l-value");
                }

                if (lhs != rhs) {
                    er->report_error(node->span,
                                     "type mismatch, left side of assignment "
                                     "has type {} but right side has type {}",
                                     ts->fatten(lhs), ts->fatten(rhs));
                }

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::Break: break;

            case NodeKind::Defer:
                add_types(ctx, node->first);
                return node->set_type(ts->get_type_void());

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
            case NodeKind::ShftRight: break;

            case NodeKind::Add:
            case NodeKind::Sub:
            case NodeKind::Mul:
            case NodeKind::Div:
            case NodeKind::Mod: {
                auto lhs = add_types(ctx, node->first);
                auto rhs = add_types(ctx, node->second);

                if (lhs != rhs) {
                    er->report_error(
                        node->span, "incompatiple values in {}: {} and {}",
                        node->kind, ts->fatten(lhs), ts->fatten(rhs));
                }

                // NOTE: support floats and operator overloads
                if (!ts->get(lhs)->is_integral()) {
                    er->report_error(node->span,
                                     "value of type {} does not support {}",
                                     ts->fatten(lhs), node->kind);
                }

                return node->set_type(lhs);
            }

            case NodeKind::Cast:
            case NodeKind::OrElse:
            case NodeKind::OrReturn: break;

            case NodeKind::AddrOf: {
                auto ty = add_types(ctx, node->first);
                if (!ast->get(node->first)->is_lvalue()) {
                    er->report_error(node->span,
                                     "can't take address of non l-value");
                }

                auto flags = TypeFlags::None;
                if (ts->get(ty)->is_const()) {
                    flags = TypeFlags::IsConst;
                }

                return node->set_type(ts->get_type_ptr(ty, flags));
            }

            case NodeKind::LogicNot:
            case NodeKind::BinNot:
            case NodeKind::Plus:
            case NodeKind::Neg:
            case NodeKind::Optional: break;

            case NodeKind::Ptr: {
                auto ty = add_types(ctx, node->first);
                if (!ts->get(ty)->is_type()) {
                    er->report_error(
                        node->span, "can't create pointer type to non-type: {}",
                        ts->fatten(ty));
                }

                return node->set_type(ts->get_type_type());
            }

            case NodeKind::MultiPtr: {
                auto ty = add_types(ctx, node->first);
                if (!ts->get(ty)->is_type()) {
                    er->report_error(
                        node->span, "can't create pointer type to non-type: {}",
                        ts->fatten(ty));
                }

                return node->set_type(ts->get_type_type());
            }

            case NodeKind::SlicePtr:
            case NodeKind::Array:
            case NodeKind::ArrayType:
            case NodeKind::ArrayAutoLen: break;
            case NodeKind::Deref: {
                auto child = add_types(ctx, node->first);

                // TODO: handle optionals
                if (!ts->get(child)->is_ptr()) {
                    er->report_error(node->span,
                                     "can't dereference non-pointer type: {}",
                                     ts->fatten(child));
                    return node->set_type(child);
                }

                // it is a pointer, so unwrap it
                return node->set_type(ts->get(child)->first);
            }

            case NodeKind::Call: {
                auto call = node->as_call(*ast);

                auto callee = add_types(ctx, call.callee);

                // TODO: handle calling function pointers
                if (!ts->get(callee)->is_func()) {
                    er->report_error(node->span,
                                     "can't call non function of type: {}",
                                     ts->fatten(callee));

                    // NOTE: use the inferred expected type when it fails
                    return node->set_type(ts->get_type_err());
                }

                std::vector<TypeHandle> arg_types;
                for (auto& a : call.args)
                    arg_types.push_back(add_types(ctx, a));

                auto f = ts->get(callee)->as_func(*ts);
                if (f.args.size() != call.args.size()) {
                    er->report_error(node->span,
                                     "mismatched number of arguments for call, "
                                     "expected {} arguments but got {}",
                                     f.args.size(), call.args.size());

                    er->report_note(
                        ast->get(call.callee)->span, "callee expects ({})",
                        fmt::join(
                            std::ranges::views::transform(
                                f.args, [&](auto h) { return ts->fatten(h); }),
                            ", "));

                    er->report_note(
                        node->span, "but received ({})",
                        fmt::join(std::ranges::views::transform(
                                      arg_types,
                                      [&](auto h) { return ts->fatten(h); }),
                                  ", "));
                }

                auto count = std::min(arg_types.size(), f.args.size());
                for (size_t i = 0; i < count; i++) {
                    if (arg_types[i] != f.args[i]) {
                        er->report_error(
                            ast->get(call.args[i])->span,
                            "type mismatch, function expects {}, but got: {}",
                            ts->fatten(f.args[i]), ts->fatten(arg_types[i]));
                    }
                }

                return node->set_type(f.ret);
            }

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

        er->report_debug(node->span, "not_implemented: {}", node->kind);
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

            case NodeKind::Ptr: {
                auto child = eval_to_type(ctx, node.first);

                auto flags = TypeFlags::None;
                if (node.has_flag_ptr_is_const()) {
                    flags |= TypeFlags::IsConst;
                }

                return ts->get_type_ptr(child, flags);
            }

            case NodeKind::MultiPtr: {
                auto child = eval_to_type(ctx, node.first);

                auto flags = TypeFlags::None;
                if (node.has_flag_ptr_is_const()) {
                    flags |= TypeFlags::IsConst;
                }

                return ts->get_type_multi_ptr(child, flags);
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
                .where = {},
                .is_const = true});

    ctx.define({.name = "u32",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_u32(),
                .where = {},
                .is_const = true});

    ctx.define({.name = "i16",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_i16(),
                .where = {},
                .is_const = true});

    ctx.define({.name = "u16",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_u16(),
                .where = {},
                .is_const = true});

    ctx.define({.name = "i8",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_i8(),
                .where = {},
                .is_const = true});

    ctx.define({.name = "u8",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_u8(),
                .where = {},
                .is_const = true});

    ctx.define({.name = "usize",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_usize(),
                .where = {},
                .is_const = true});

    ctx.define({.name = "void",
                .type = ts.get_type_type(),
                .inner_type = ts.get_type_void(),
                .where = {},
                .is_const = true});

    return t.add_types(ctx, n);
}

}  // namespace yal
