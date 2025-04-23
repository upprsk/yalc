#include "sema.hpp"

#include <ranges>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"
#include "types.hpp"
#include "value.hpp"

namespace yal::sema {
using ast::Ast;
using ast::Node;
using ast::NodeKind;
namespace conv = ast::conv;

namespace rv = std::ranges::views;

struct Context {
    [[nodiscard]] constexpr auto child(types::Type* current_function) const
        -> Context {
        return {.current_function = current_function, .ts = ts, .er = er};
    }

    [[nodiscard]] constexpr auto child_lvalue() const -> Context {
        return {.current_function = current_function,
                .is_lvalue = true,
                .ts = ts,
                .er = er};
    }

    types::Type* current_function{};
    bool         is_lvalue{};

    types::TypeStore* ts{};
    ErrorReporter*    er{};
};

auto eval_node(Ast& ast, Node* node, Context& ctx) -> Value;
void visit_node(Ast& ast, Node* node, Context& ctx);

// ============================================================================

auto eval_node_func_params(Ast& ast, Node* node, Context& ctx) -> Value {
    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto data = conv::func_params(*node);

    std::vector<std::pair<Node*, types::Type*>> types;

    for (auto child : data.params) {
        auto param = conv::func_param(*child);

        if (!param.type) {
            types.emplace_back(child, nullptr);
            continue;
        }

        types::Type* ty = nullptr;

        auto v = eval_node(ast, param.type, ctx);
        ASSERT(v.type != nullptr);
        if (!v.type->is_type()) {
            er.report_error(
                child->get_loc(),
                "can not use value of type {} as type for parameter", *v.type);
            ty = ts.get_error();
        } else {
            ty = v.get_data_type();
        }

        types.emplace_back(child, ty);
    }

    types::Type* prev_ty = nullptr;
    for (auto& [node, ty] : types | rv::reverse) {
        if (ty == nullptr) {
            if (prev_ty == nullptr) {
                er.report_error(node->get_loc(), "no type found for parameter");

                ty = ts.get_error();
            } else {
                ty = prev_ty;
            }
        }

        auto d = node->get_decl();
        ASSERT(d != nullptr);
        ASSERT(d->get_type() == nullptr);

        d->value = {.type = ty};

        prev_ty = ty;
    }

    return {.type = ts.get_type(),
            .data = ts.new_pack(types |
                                rv::transform([](auto p) { return p.second; }) |
                                std::ranges::to<std::vector>())};
}

auto eval_node_func_ret_pack(Ast& ast, Node* node, Context& ctx) -> Value {
    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto data = conv::func_ret_pack(*node);

    std::vector<std::pair<Node*, types::Type*>> types;

    for (auto child : data.ret) {
        if (child->is_oneof(ast::NodeKind::NamedRet)) {
            auto param = conv::named_ret(*child);

            if (!param.type) {
                types.emplace_back(child, nullptr);
                continue;
            }

            types::Type* ty = nullptr;

            auto v = eval_node(ast, param.type, ctx);
            ASSERT(v.type != nullptr);
            if (!v.type->is_type()) {
                er.report_error(
                    child->get_loc(),
                    "can not use value of type {} as type for named return",
                    *v.type);
                ty = ts.get_error();
            } else {
                ty = v.get_data_type();
            }

            types.emplace_back(child, ty);
            continue;
        }

        types::Type* ty = nullptr;

        auto v = eval_node(ast, child, ctx);
        ASSERT(v.type != nullptr);
        if (!v.type->is_type()) {
            er.report_error(child->get_loc(),
                            "can not use value of type {} as type for return",
                            *v.type);
            ty = ts.get_error();
        } else {
            ty = v.get_data_type();
        }

        types.emplace_back(child, ty);
    }

    types::Type* prev_ty = nullptr;
    for (auto& [node, ty] : types | rv::reverse) {
        if (ty == nullptr) {
            if (prev_ty == nullptr) {
                er.report_error(node->get_loc(), "no type found for return");

                ty = ts.get_error();
            } else {
                ty = prev_ty;
            }
        }

        if (node->is_oneof(ast::NodeKind::NamedRet)) {
            auto d = node->get_decl();
            ASSERT(d != nullptr);
            ASSERT(d->get_type() == nullptr);

            d->value = {.type = ty};
        }

        prev_ty = ty;
    }

    return {.type = ts.get_type(),
            .data = ts.new_pack(types |
                                rv::transform([](auto p) { return p.second; }) |
                                std::ranges::to<std::vector>())};
}

auto eval_node(Ast& ast, Node* node, Context& ctx) -> Value {
    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    if (node->is_oneof(ast::NodeKind::MultiPtr, ast::NodeKind::MultiPtrConst)) {
        auto data = conv::mptr(*node);
        auto inner = eval_node(ast, data.inner, ctx);
        ASSERT(inner.type != nullptr);

        types::Type* inner_ty = nullptr;
        if (!inner.type->is_type()) {
            er.report_error(
                node->get_loc(),
                "can not use value of type {} as type in multi-pointer",
                *inner.type);
            inner_ty = ts.get_error();
        } else {
            inner_ty = inner.get_data_type();
        }

        auto ty = ts.new_mptr(inner_ty, data.is_const);
        return {.type = ts.get_type(), .data = ty};
    }

    if (node->is_oneof(ast::NodeKind::FuncParams)) {
        return eval_node_func_params(ast, node, ctx);
    }

    if (node->is_oneof(ast::NodeKind::FuncRetPack)) {
        return eval_node_func_ret_pack(ast, node, ctx);
    }

    if (node->is_oneof(NodeKind::Str)) {
        return {.type = ts.get_strview(), .data = node->get_data_str()};
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        if (!data.to) {
            er.report_debug(node->get_loc(), "[eval] no decl found in {:?}",
                            data.name);
            return {.type = ts.get_error()};
        }

        auto ty = data.to->get_type();
        if (!ty) {
            ASSERT(data.to->node != nullptr);

            er.report_debug(node->get_loc(), "no type found in {:?}",
                            data.name);
            er.report_note(data.to->node->get_loc(), "defined here ({})",
                           data.to->full_name);
            return {.type = ts.get_error()};
        }

        return data.to->value;
    }

    PANIC("cant eval node of type", node->get_kind());
}

// ============================================================================

void visit_func_decl(Ast& ast, Node* node, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };

    auto& ts = *ctx.ts;
    auto& er = *ctx.er;
    auto  data = conv::func_decl(*node);

    visit(ctx, data.decorators);
    visit(ctx, data.gargs);
    visit(ctx, data.args);
    visit(ctx, data.ret);

    // TODO: Evaluate decorators
    if (data.get_gargs().params.size() != 0) {
        er.report_bug(data.gargs->get_loc(),
                      "Generic args have not been implemented");
    }

    auto params = eval_node(ast, data.args, ctx);
    ASSERT(params.type != nullptr);
    ASSERT(params.type->is_type());

    Value ret;
    if (data.ret)
        ret = eval_node(ast, data.ret, ctx);
    else
        ret = {.type = ts.get_type(),
               .data = ts.new_pack(std::array{ts.get_void()})};

    ASSERT(ret.type != nullptr);
    ASSERT(ret.type->is_type());

    auto ty =
        ts.new_func(params.get_data_type(), ret.get_data_type(),
                    node->get_kind() == ast::NodeKind::FuncDeclWithCVarArgs);
    node->set_type(ty);

    auto d = node->get_decl();
    ASSERT(d != nullptr);
    ASSERT(d->get_type() == nullptr);

    // TODO: save the function as value. Somehow
    d->value = {.type = ty};

    auto sctx = ctx.child(ty);
    visit(sctx, data.body);
}

void visit_var_decl(Ast& ast, Node* node, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };

    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto data = conv::var_decl(*node);
    visit(ctx, data.types);
    visit(ctx, data.inits);

    // we have explicit types
    if (data.types && data.inits) {
        auto ids = data.get_ids().ids;
        auto types = data.get_types().items;
        auto inits = data.get_inits().items;

        if (ids.size() != types.size()) {
            er.report_error(data.types->get_loc(),
                            "wrong number of types for declaration, "
                            "expected {} but got {}",
                            ids.size(), types.size());
            er.report_note(data.ids->get_loc(), "{} identifiers declared here",
                           ids.size());
        }

        if (ids.size() != inits.size()) {
            er.report_error(data.inits->get_loc(),
                            "wrong number of initializers for declaration, "
                            "expected {} but got {}",
                            ids.size(), inits.size());
            er.report_note(data.ids->get_loc(), "{} identifiers declared here",
                           ids.size());
        }

        for (auto const& [id, ty, init] : rv::zip(ids, types, inits)) {
            auto name = conv::id(*id);

            visit(ctx, ty);
            visit(ctx, init);
            if (name.name == "_") continue;

            auto inity = init->get_type();
            ASSERT(inity != nullptr);

            auto v = eval_node(ast, ty, ctx);
            ASSERT(v.type != nullptr);

            types::Type* type = nullptr;

            if (!v.type->is_type()) {
                er.report_error(
                    ty->get_loc(),
                    "can not use value of type {} as type for declaration",
                    *v.type);
                type = inity;  // NOTE: maybe set to error
            } else {
                type = v.get_data_type();
            }

            auto r = ts.coerce(type, inity);
            if (!r) {
                er.report_error(init->get_loc(),
                                "can not coerce value of type {} to "
                                "declaration of type {}",
                                *inity, *type);
                er.report_note(ty->get_loc(), "required {} from here", *type);
                r = inity;  // NOTE: maybe set to error
            }

            id->set_type(r);

            if (name.to) {
                ASSERT(name.to->get_type() == nullptr);
                name.to->value = {.type = r};
            } else {
                er.report_debug(id->get_loc(), "id has no decl: {:?}",
                                name.name);
            }
        }
    }

    // just types, not inits
    else if (data.types) {
        auto ids = data.get_ids();
        auto types = data.get_types();

        if (ids.ids.size() != types.items.size()) {
            er.report_error(data.types->get_loc(),
                            "wrong number of types for declaration, "
                            "expected {} but got {}",
                            ids.ids.size(), types.items.size());
            er.report_note(data.ids->get_loc(), "{} identifiers declared here",
                           ids.ids.size());
        }

        for (auto const& [id, ty] : rv::zip(ids.ids, types.items)) {
            auto name = conv::id(*id);

            if (name.name == "_") continue;

            auto v = eval_node(ast, ty, ctx);
            ASSERT(v.type != nullptr);

            types::Type* type = nullptr;

            if (!v.type->is_type()) {
                er.report_error(
                    ty->get_loc(),
                    "can not use value of type {} as type for declaration",
                    *v.type);
                type = ts.get_error();  // NOTE: maybe set to error
            } else {
                type = v.get_data_type();
            }

            id->set_type(type);

            if (name.to) {
                ASSERT(name.to->get_type() == nullptr);
                name.to->value = {.type = type};
            } else {
                er.report_debug(id->get_loc(), "id has no decl: {:?}",
                                name.name);
            }
        }
    }

    // no explicit types but got inits
    else if (data.inits) {
        auto ids = data.get_ids();
        auto inits = data.get_inits();

        if (ids.ids.size() != inits.items.size()) {
            er.report_error(data.inits->get_loc(),
                            "wrong number of initializers for declaration, "
                            "expected {} but got {}",
                            ids.ids.size(), inits.items.size());
            er.report_note(data.ids->get_loc(), "{} identifiers declared here",
                           ids.ids.size());
        }

        for (auto const& [id, init] : rv::zip(ids.ids, inits.items)) {
            auto name = conv::id(*id);

            visit(ctx, init);
            if (name.name == "_") continue;

            auto inity = init->get_type();
            ASSERT(inity != nullptr);

            id->set_type(inity);

            if (name.to) {
                ASSERT(name.to->get_type() == nullptr);
                name.to->value = {.type = inity};
            } else {
                er.report_debug(id->get_loc(), "id has no decl: {:?}",
                                name.name);
            }
        }
    }

    // nothing given
    else {
        er.report_error(node->get_loc(), "no types in declaration");

        auto ids = data.get_ids();
        for (auto const& id : ids.ids) {
            auto name = conv::id(*id);
            id->set_type(ts.get_error());

            if (name.to) {
                ASSERT(name.to->get_type() == nullptr);
                name.to->value = {.type = ts.get_error()};
            } else {
                er.report_debug(id->get_loc(), "id has no decl: {:?}",
                                name.name);
            }
        }
    }
}

void visit_node(Ast& ast, Node* node, Context& ctx) {
    if (node == nullptr) return;

    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };
    auto visit_span = [&](Context& ctx, std::span<Node* const> nodes) {
        for (auto node : nodes) visit_node(ast, node, ctx);
    };

    auto visit_children = [&](Context& ctx, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, Context& ctx) {
                visit_node(ast, node, ctx);
            },
            ctx);
    };

    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    if (node->is_oneof(NodeKind::Module, NodeKind::SourceFile,
                       NodeKind::ModuleDecl, NodeKind::Decorators,
                       NodeKind::Decorator, NodeKind::FuncParams,
                       NodeKind::FuncRetPack, NodeKind::Block)) {
        node->set_type(ts.get_void());
        visit_children(ctx, node);
        return;
    }

    if (node->is_oneof(NodeKind::FuncDecl, NodeKind::FuncDeclWithCVarArgs)) {
        visit_func_decl(ast, node, ctx);
        return;
    }

    if (node->is_oneof(NodeKind::DecoratorParam)) {
        auto data = conv::decorator_param(*node);
        visit(ctx, data.value);

        auto ty = data.value ? data.value->get_type() : ts.get_void();
        node->set_type(ty);
        return;
    }

    if (node->is_oneof(ast::NodeKind::FuncParam)) {
        auto data = conv::func_param(*node);
        visit(ctx, data.type);

        auto ty = data.type->get_type();
        ASSERT(ty != nullptr);
        if (!ty->is_type()) {
            er.report_error(
                node->get_loc(),
                "can not use value of type {} as type for parameter", *ty);
            node->set_type(ts.get_error());
            return;
        }

        auto e = eval_node(ast, data.type, ctx);
        ASSERT(e.type != nullptr);
        ASSERT(e.type->is_type());

        node->set_type(e.get_data_type());
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);

        std::vector<types::Type*> types;
        for (auto child : data.items) {
            visit(ctx, child);
            if (child->get_type() == nullptr) {
                types.push_back(ts.get_error());
            } else {
                types.push_back(child->get_type());
            }
        }

        node->set_type(ts.new_pack(types));
        return;
    }

    if (node->is_oneof(ast::NodeKind::AddrOf)) {
        auto data = conv::unary(*node);
        visit(ctx, data.child);

        auto cty = data.child->get_type();
        ASSERT(cty != nullptr);

        // FIXME: when to make const?
        node->set_type(ts.new_ptr(cty, true));
        return;
    }

    if (node->is_oneof(ast::NodeKind::Str)) {
        node->set_type(ts.get_strview());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        node->set_type(ts.get_i32());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        if (!data.to) return;

        auto ty = data.to->get_type();
        if (!ty) {
            ASSERT(data.to->node != nullptr);

            er.report_debug(node->get_loc(), "no type found in {:?}",
                            data.name);
            er.report_note(data.to->node->get_loc(), "defined here ({})",
                           data.to->full_name);
        }

        node->set_type(data.to->get_type());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Ptr, ast::NodeKind::PtrConst,
                       ast::NodeKind::MultiPtr, ast::NodeKind::MultiPtrConst)) {
        visit_children(ctx, node);
        node->set_type(ts.get_type());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Call)) {
        auto data = conv::call(*node);
        visit(ctx, data.callee);

        auto cty = data.callee->get_type();
        ASSERT(cty != nullptr);

        if (!cty->is_func()) {
            er.report_error(node->get_loc(),
                            "can not call non-function value of type {}", *cty);
            visit_span(ctx, data.args);
            return;
        }

        auto fn = cty->as_func();
        if (fn.is_var_args) {
            er.report_bug(node->get_loc(), "varargs have not been implemented");
        }

        if (fn.get_params().size() != data.args.size()) {
            er.report_error(
                node->get_loc(),
                "wrong number of arguments in call. Expected {} but got {}",
                fn.get_params().size(), data.args.size());
        }

        for (auto const& [i, param, arg] :
             rv::zip(rv::iota(1), fn.get_params(), data.args)) {
            visit(ctx, arg);
            ASSERT(arg->get_type() != nullptr);

            arg->set_type(arg->get_type());

            auto r = ts.coerce(param, arg->get_type());
            if (!r) {
                er.report_error(arg->get_loc(),
                                "can not coerce argument of type {} to {}",
                                *arg->get_type(), *param);
                arg->set_type(ts.get_error());
                continue;
            }

            auto c = ast.new_coerce(arg->get_loc(), arg, r);
            node->set_child(i, c);
        }

        node->set_type(fn.ret);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Field)) {
        auto data = conv::field(*node);
        visit(ctx, data.receiver);

        auto rty = data.receiver->get_type();
        ASSERT(rty != nullptr);

        if (rty->is_strview()) {
            if (data.name == "ptr") {
                node->set_type(ts.new_mptr(ts.get_u8(), true));
                return;
            }

            if (data.name == "len") {
                node->set_type(ts.get_usize());
                return;
            }

            er.report_error(node->get_loc(),
                            "type {} does not have a field named {:?}", *rty,
                            data.name);
            node->set_type(ts.get_error());
            return;
        }

        er.report_error(node->get_loc(), "type {} does not have fields", *rty);
        node->set_type(ts.get_error());
        return;
    }

    if (node->is_oneof(ast::NodeKind::VarDecl)) {
        visit_var_decl(ast, node, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprStmt)) {
        auto data = conv::unary(*node);
        visit(ctx, data.child);

        ASSERT(data.child->get_type() != nullptr);
        if (!data.child->get_type()->is_void()) {
            er.report_warn(node->get_loc(),
                           "discard of non-void result of type {}",
                           *data.child->get_type());
        }

        node->set_type(ts.get_void());
        return;
    }

    if (node->is_oneof(ast::NodeKind::ReturnStmt)) {
        auto data = conv::unary(*node);
        visit(ctx, data.child);

        node->set_type(ts.get_void());

        if (ctx.current_function == nullptr) {
            er.report_error(node->get_loc(),
                            "can not use return statement outside of function");
            return;
        }

        auto rty = data.child->get_type();
        ASSERT(rty != nullptr);

        auto expected_rty = ctx.current_function->as_func().ret;
        ASSERT(expected_rty != nullptr);

        auto r = ts.coerce(expected_rty, rty);
        if (!r) {
            er.report_error(
                node->get_loc(),
                "can not coerce value of type {} to return of type {}", *rty,
                *expected_rty);
            return;
        }

        return;
    }

    er.report_warn(node->get_loc(), "node not sema analized ({})",
                   node->get_kind());
    visit_children(ctx, node);
}

void sema_ast(Ast& ast, Node* root, ErrorReporter& er, types::TypeStore& ts,
              Options const& opt) {
    auto ctx = Context{.ts = &ts, .er = &er};
    visit_node(ast, root, ctx);
}

}  // namespace yal::sema
