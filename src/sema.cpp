#include "sema.hpp"

#include <ranges>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/color.h"
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
auto eval_node_to_type(Ast& ast, Node* node, Context& ctx) -> types::Type*;
void visit_node(Ast& ast, Node* node, Context& ctx);

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
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

        auto ty = eval_node_to_type(ast, param.type, ctx);
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

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
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

            auto ty = eval_node_to_type(ast, param.type, ctx);
            types.emplace_back(child, ty);
            continue;
        }

        auto ty = eval_node_to_type(ast, child, ctx);
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
        auto inner_ty = eval_node_to_type(ast, data.inner, ctx);
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

auto eval_node_to_type(Ast& ast, Node* node, Context& ctx) -> types::Type* {
    ASSERT(node != nullptr);

    auto v = eval_node(ast, node, ctx);
    ASSERT(v.type != nullptr);

    if (!v.type->is_type()) {
        ctx.er->report_error(node->get_loc(),
                             "can not use value of type {} as type", *v.type);
        return ctx.ts->get_error();
    }

    return v.get_data_type();
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_decorators(Ast& ast, Node* decorators, Decl* decl, Context& ctx) {
    auto& er = *ctx.er;

    for (auto decorator : conv::decorators(*decorators).items) {
        auto dec = conv::decorator(*decorator);

        // handle extern decorator
        if (dec.name == "extern") {
            decl->flags.set_extern();

            // no parameters, just mark as extern and set extern name to
            // local_name
            if (dec.params.size() == 0) {
                decl->link_name = decl->local_name;
            }

            // with paramters, validate
            else if (dec.params.size() == 1) {
                auto param = conv::decorator_param(*dec.params[0]);
                if (param.key == "link_name") {
                    auto v = eval_node(ast, param.value, ctx);
                    ASSERT(v.type != nullptr);

                    if (v.type->is_strview()) {
                        decl->link_name = v.get_data_strv();
                    } else {
                        er.report_error(
                            param.value->get_loc(),
                            "expected string view for parameter, got {}",
                            *v.type);
                    }
                }

                // unknown parameter, error
                else {
                    er.report_error(dec.params[0]->get_loc(),
                                    "unknown parameter for `extern`: {:?}",
                                    param.key);
                }
            }

            // too many parameters, error
            else {
                er.report_error(decorator->get_loc(),
                                "expected none or a single named parameter for "
                                "`extern`, got {} parameters",
                                dec.params.size());
            }
        }

        // export the current function, very similar to `extern`
        else if (dec.name == "export") {
            // no parameters, just mark extern name to local_name
            if (dec.params.size() == 0) {
                decl->link_name = decl->local_name;
            }

            // with paramters, validate
            else if (dec.params.size() == 1) {
                auto param = conv::decorator_param(*dec.params[0]);
                if (param.key == "link_name") {
                    auto v = eval_node(ast, param.value, ctx);
                    ASSERT(v.type != nullptr);

                    if (v.type->is_strview()) {
                        decl->link_name = v.get_data_strv();
                    } else {
                        er.report_error(
                            param.value->get_loc(),
                            "expected string view for parameter, got {}",
                            *v.type);
                    }
                }

                // unknown parameter, error
                else {
                    er.report_error(dec.params[0]->get_loc(),
                                    "unknown parameter for `export`: {:?}",
                                    param.key);
                }
            }

            // too many parameters, error
            else {
                er.report_error(decorator->get_loc(),
                                "expected none or a single named parameter for "
                                "`export`, got {} parameters",
                                dec.params.size());
            }
        }

        // this has no meaning here?
        else if (dec.name == "private") {
            PANIC("still dont know what to do here");
        }

        // we don' support custom decorators yet
        else {
            er.report_error(decorator->get_loc(),
                            "custom decorators not implemented (got {:?})",
                            dec.name);
        }
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_func_decl(Ast& ast, Node* node, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };

    auto& ts = *ctx.ts;
    auto& er = *ctx.er;
    auto  data = conv::func_decl(*node);
    auto  decl = node->get_decl();
    ASSERT(decl != nullptr);

    visit(ctx, data.decorators);
    visit(ctx, data.gargs);
    visit(ctx, data.args);
    visit(ctx, data.ret);

    visit_decorators(ast, data.decorators, decl, ctx);

    // extern should not have body
    if (decl->flags.has_extern() && data.body != nullptr) {
        er.report_error(node->get_loc(),
                        "extern function should not have a body");
    }

    // no-extern should have body
    if (!decl->flags.has_extern() && data.body == nullptr) {
        er.report_error(node->get_loc(), "missing body for function");
    }

    if (data.get_gargs().params.size() != 0) {
        er.report_bug(data.gargs->get_loc(),
                      "Generic args have not been implemented");
    }

    auto params = eval_node_to_type(ast, data.args, ctx);

    types::Type* ret = nullptr;
    if (data.ret)
        ret = eval_node_to_type(ast, data.ret, ctx);
    else
        ret = ts.new_pack(std::array{ts.get_void()});

    auto ty = ts.new_func(
        params, ret, node->get_kind() == ast::NodeKind::FuncDeclWithCVarArgs);
    node->set_type(ty);

    auto d = node->get_decl();
    ASSERT(d != nullptr);
    ASSERT(d->get_type() == nullptr);

    // TODO: save the function as value. Somehow
    d->value = {.type = ty};

    auto sctx = ctx.child(ty);
    visit(sctx, data.body);
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void fixup_untyped_integer_chain(types::TypeStore& ts, Node* chain,
                                 types::Type* target) {
    ASSERT(chain != nullptr);
    ASSERT(chain->get_type() != nullptr);
    // ASSERT(chain->get_type()->is_untyped_int(), *chain->get_type());

    if (chain->is_oneof(ast::NodeKind::Int)) {
        chain->set_type(target);
        return;
    }

    if (chain->is_oneof(ast::NodeKind::Add)) {
        chain->set_type(target);
        for (auto c : chain->get_children())
            fixup_untyped_integer_chain(ts, c, target);
        return;
    }

    if (chain->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*chain);
        ASSERT(data.items.size() == target->inner.size());
        ASSERT(target->kind == types::TypeKind::Pack);
        ASSERT(!target->contains_untyped_int());

        chain->set_type(target);
        for (auto [c, inner] : rv::zip(data.items, target->inner))
            fixup_untyped_integer_chain(ts, c, inner);
        return;
    }

    // fmt::println(stderr, "{}: got end of int updating chain on {}",
    //              fmt::styled("ERROR", fmt::fg(fmt::color::red)),
    //              chain->get_kind());
}

// ============================================================================

auto count_init_exprs(conv::ExprPack const& pack) -> size_t {
    size_t expr_count{};
    for (auto init : pack.items) {
        auto ty = init->get_type();
        ASSERT(ty != nullptr);

        if (ty->is_pack()) {
            // multiple returns of some function
            expr_count += ty->inner.size();
        } else {
            expr_count++;
        }
    }

    return expr_count;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_decl_with_types_and_inits(Ast& ast, Node* ids_node, Node* types_node,
                                     Node* inits_node, Context& ctx) {
    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto types = conv::expr_pack(*types_node).items;
    auto inits = conv::expr_pack(*inits_node).items;

    auto expr_count = count_init_exprs(conv::expr_pack(*inits_node));

    if (ids.size() != expr_count) {
        er.report_error(inits_node->get_loc(),
                        "wrong number of initializers for declaration, "
                        "expected {} but got {}",
                        ids.size(), expr_count);
        er.report_note(ids_node->get_loc(), "{} identifiers declared here ",
                       ids.size());
    }

    if (ids.size() != types.size()) {
        er.report_error(types_node->get_loc(),
                        "wrong number of types for declaration, "
                        "expected {} but got {}",
                        ids.size(), types.size());
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    for (size_t name_idx{}, ty_idx{};
         auto [init_idx, init] : rv::enumerate(inits)) {
        if (name_idx >= ids.size() || ty_idx >= types.size()) break;

        auto init_type = init->get_type();
        ASSERT(init_type != nullptr);

        // result of calling some function
        if (init_type->is_pack()) {
            std::vector<types::Type*> coercions;
            auto                      requires_coercion = false;

            for (auto i : init_type->inner) {
                if (name_idx >= ids.size() || ty_idx >= types.size()) break;

                if (conv::id(*ids[name_idx]).is_discard()) {
                    name_idx++;
                    ty_idx++;
                    continue;
                }

                auto type = eval_node_to_type(ast, types[ty_idx], ctx);
                auto r = ts.coerce(type, i);
                if (!r) {
                    er.report_error(init->get_loc(),
                                    "can not coerce value of type {} to "
                                    "declaration of type {}",
                                    *i, *type);
                    er.report_note(types[ty_idx]->get_loc(),
                                   "required {} from here", *type);
                    r = ts.get_error();
                }

                // a function should not be able to return this
                ASSERT(!i->is_untyped_int());

                ids[name_idx]->set_type(r);
                coercions.push_back(r);
                if (*r != *i) {
                    requires_coercion = true;
                }

                auto name = conv::id(*ids[name_idx]);
                if (name.to) {
                    ASSERT(name.to->get_type() == nullptr);
                    name.to->value = {.type = r};
                } else {
                    er.report_debug(ids[name_idx]->get_loc(),
                                    "id has no decl: {:?}", name.name);
                }

                name_idx++;
                ty_idx++;
            }

            if (requires_coercion)
                inits[init_idx] = ast.new_coerce(init->get_loc(), init,
                                                 ts.new_pack(coercions));
        }

        // a simple initializer expression
        else {
            if (conv::id(*ids[name_idx]).is_discard()) {
                name_idx++;
                ty_idx++;
                continue;
            }

            auto type = eval_node_to_type(ast, types[ty_idx], ctx);
            auto r = ts.coerce(type, init_type);
            if (!r) {
                er.report_error(init->get_loc(),
                                "can not coerce value of type {} to "
                                "declaration of type {}",
                                *init_type, *type);
                er.report_note(types[ty_idx]->get_loc(),
                               "required {} from here", *type);
                r = ts.get_error();
            }

            if (init_type->is_untyped_int()) {
                ASSERT(!r->is_untyped_int());

                // fixup untyped integers
                fixup_untyped_integer_chain(ts, init, r);
            }

            ids[name_idx]->set_type(r);
            if (*r != *init->get_type())
                inits[init_idx] = ast.new_coerce(init->get_loc(), init, r);

            auto name = conv::id(*ids[name_idx]);
            if (name.to) {
                ASSERT(name.to->get_type() == nullptr);
                name.to->value = {.type = r};
            } else {
                er.report_debug(ids[name_idx]->get_loc(),
                                "id has no decl: {:?}", name.name);
            }

            name_idx++;
            ty_idx++;
        }
    }
}

void visit_decl_with_types(Ast& ast, Node* ids_node, Node* types_node,
                           Context& ctx) {
    auto& er = *ctx.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto types = conv::expr_pack(*types_node).items;

    if (ids.size() != types.size()) {
        er.report_error(types_node->get_loc(),
                        "wrong number of types for declaration, "
                        "expected {} but got {}",
                        ids.size(), types.size());
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    for (auto const& [id, ty] : rv::zip(ids, types)) {
        auto name = conv::id(*id);
        if (name.is_discard()) continue;

        auto type = eval_node_to_type(ast, ty, ctx);
        id->set_type(type);

        if (name.to) {
            ASSERT(name.to->get_type() == nullptr);
            name.to->value = {.type = type};
        } else {
            er.report_debug(id->get_loc(), "id has no decl: {:?}", name.name);
        }
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_decl_with_inits(Ast& ast, Node* ids_node, Node* inits_node,
                           Context& ctx) {
    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto inits = conv::expr_pack(*inits_node).items;

    auto expr_count = count_init_exprs(conv::expr_pack(*inits_node));

    if (ids.size() != expr_count) {
        er.report_error(inits_node->get_loc(),
                        "wrong number of initializers for declaration, "
                        "expected {} but got {}",
                        ids.size(), expr_count);
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    for (size_t name_idx{}; auto [init_idx, init] : rv::enumerate(inits)) {
        if (name_idx >= ids.size()) break;

        auto init_type = init->get_type();
        ASSERT(init_type != nullptr);

        // result of calling some function
        if (init_type->is_pack()) {
            for (auto i : init_type->inner) {
                if (conv::id(*ids[name_idx]).is_discard()) {
                    name_idx++;
                    continue;
                }

                ids[name_idx]->set_type(i);

                auto name = conv::id(*ids[name_idx]);
                if (name.to) {
                    ASSERT(name.to->get_type() == nullptr);
                    name.to->value = {.type = i};
                } else {
                    er.report_debug(ids[name_idx]->get_loc(),
                                    "id has no decl: {:?}", name.name);
                }

                name_idx++;
            }
        }

        // a simple initializer expression
        else {
            if (conv::id(*ids[name_idx]).is_discard()) {
                name_idx++;
                continue;
            }

            if (init_type->is_untyped_int()) {
                fixup_untyped_integer_chain(ts, init, ts.get_i32());
                init_type = init->get_type();
            }

            ids[name_idx]->set_type(init_type);

            auto name = conv::id(*ids[name_idx]);
            if (name.to) {
                ASSERT(name.to->get_type() == nullptr);
                name.to->value = {.type = init_type};
            } else {
                er.report_debug(ids[name_idx]->get_loc(),
                                "id has no decl: {:?}", name.name);
            }

            name_idx++;
        }
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_var_decl(Ast& ast, Node* node, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };

    auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto data = conv::var_decl(*node);
    visit(ctx, data.types);
    visit(ctx, data.inits);

    // we have explicit types
    if (data.types && data.inits) {
        visit_decl_with_types_and_inits(ast, data.ids, data.types, data.inits,
                                        ctx);
    }

    // just types, not inits
    else if (data.types) {
        visit_decl_with_types(ast, data.ids, data.types, ctx);
    }

    // no explicit types but got inits
    else if (data.inits) {
        visit_decl_with_inits(ast, data.ids, data.inits, ctx);
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

    // fixup the type of the init expr pack
    if (data.inits) {
        auto inits = conv::expr_pack(*data.inits).items;

        std::vector<types::Type*> types;
        for (auto n : inits) {
            ASSERT(n != nullptr);
            ASSERT(n->get_type() != nullptr);
            types.push_back(n->get_type());
        }

        data.inits->set_type(ts.new_pack(types));
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
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
        if (data.type == nullptr) {
            er.report_error(node->get_loc(), "missing type for parameter");
            return;
        }

        auto ty = data.type->get_type();
        ASSERT(ty != nullptr);
        if (!ty->is_type()) {
            er.report_error(
                node->get_loc(),
                "can not use value of type {} as type for parameter", *ty);
            node->set_type(ts.get_error());
            return;
        }

        auto type = eval_node_to_type(ast, data.type, ctx);
        node->set_type(type);
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

    if (node->is_oneof(ast::NodeKind::Equal, ast::NodeKind::Less,
                       ast::NodeKind::Greater, ast::NodeKind::LessEqual,
                       ast::NodeKind::GreaterEqual)) {
        auto data = conv::binary(*node);
        visit(ctx, data.lhs);
        visit(ctx, data.rhs);

        auto lhs = data.lhs->get_type();
        ASSERT(lhs != nullptr);
        auto rhs = data.rhs->get_type();
        ASSERT(rhs != nullptr);

        node->set_type(ts.get_bool());

        // first try to coerce the rhs to lhs to have a single type on both
        // sides
        auto r = ts.coerce(lhs, rhs);
        if (!r) {
            er.report_error(
                node->get_loc(),
                "can not coerce argument of type {} to {} in comparison", *lhs,
                *rhs);
        } else {
            // coerce rhs to the same type as lhs
            if (*r != *rhs)
                node->set_child(
                    1, ast.new_coerce(data.rhs->get_loc(), data.rhs, r));
        }

        if (lhs->is_untyped_int() && !rhs->is_untyped_int()) {
            fixup_untyped_integer_chain(ts, data.lhs, rhs);
        }

        if (rhs->is_untyped_int() && !lhs->is_untyped_int()) {
            fixup_untyped_integer_chain(ts, data.rhs, lhs);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Add, ast::NodeKind::Sub,
                       ast::NodeKind::Mul, ast::NodeKind::Div,
                       ast::NodeKind::Mod)) {
        auto data = conv::binary(*node);
        visit(ctx, data.lhs);
        visit(ctx, data.rhs);

        auto lhs = data.lhs->get_type();
        ASSERT(lhs != nullptr);
        auto rhs = data.rhs->get_type();
        ASSERT(rhs != nullptr);

        auto r = ts.coerce(lhs, rhs);
        if (!r) {
            er.report_error(
                node->get_loc(),
                "can not coerce argument of type {} to {} in operation", *rhs,
                *lhs);
            er.report_note(data.lhs->get_loc(), "this has type {}", *lhs);

            r = ts.get_error();
        }

        if (!r->is_integral() && !r->is_err()) {
            er.report_error(node->get_loc(), "type {} does not support {}", *r,
                            node->get_kind());
            r = ts.get_error();
        }

        if (lhs->is_untyped_int() && !rhs->is_untyped_int()) {
            fixup_untyped_integer_chain(ts, data.lhs, rhs);
        }

        if (rhs->is_untyped_int() && !lhs->is_untyped_int()) {
            fixup_untyped_integer_chain(ts, data.rhs, lhs);
        }

        // coerce rhs to the same type as lhs
        if (*r != *data.rhs->get_type())
            node->set_child(1,
                            ast.new_coerce(data.rhs->get_loc(), data.rhs, r));

        node->set_type(r);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Str)) {
        node->set_type(ts.get_strview());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        node->set_type(ts.get_untyped_int());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        if (data.is_discard() && ctx.is_lvalue) {
            node->set_type(ts.get_void());
            return;
        }

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

            if (arg->get_type()->is_untyped_int()) {
                ASSERT(!r->is_untyped_int());
                fixup_untyped_integer_chain(ts, arg, r);
            }

            if (*r != *arg->get_type()) {
                auto c = ast.new_coerce(arg->get_loc(), arg, r);
                node->set_child(i, c);
            }
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

    if (node->is_oneof(ast::NodeKind::Assign)) {
        auto data = conv::assign(*node);

        auto sctx = ctx.child_lvalue();
        visit(sctx, data.lhs);
        visit(ctx, data.rhs);
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

    if (node->is_oneof(ast::NodeKind::WhileStmt)) {
        auto data = conv::while_stmt(*node);
        visit(ctx, data.cond);

        auto cty = data.cond->get_type();
        ASSERT(cty != nullptr);

        if (!cty->is_bool()) {
            er.report_error(data.cond->get_loc(),
                            "wrong type for condition: {}, expected boolean",
                            *cty);
        }

        visit(ctx, data.body);

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

        // FIXME: this is not very nice
        ASSERT(rty->is_pack());
        ASSERT(r->is_pack());
        if (rty->contains_untyped_int()) {
            ASSERT(!r->contains_untyped_int());
            fixup_untyped_integer_chain(ts, data.child, r);
        }

        if (*r != *data.child->get_type()) {
            node->set_child(
                0, ast.new_coerce(data.child->get_loc(), data.child, r));
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Coerce)) {
        visit_children(ctx, node);
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
