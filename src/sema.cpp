#include "sema.hpp"

#include <ranges>
#include <utility>
#include <vector>

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

struct State {
    // NOLINTNEXTLINE(readability-redundant-member-init)
    std::vector<Node*> pending_functions{};

    types::TypeStore* ts;
    ErrorReporter*    er;
    Options const*    opt;
};

struct Context {
    [[nodiscard]] constexpr auto with_module(Node* current_module) const
        -> Context {
        return {
            .current_module = current_module,
            .current_loop = current_loop,
            .current_function = current_function,
        };
    }

    [[nodiscard]] constexpr auto child(Node* current_loop) const -> Context {
        return {
            .current_module = current_module,
            .current_loop = current_loop,
            .current_function = current_function,
        };
    }

    [[nodiscard]] constexpr auto child(types::Type* current_function) const
        -> Context {
        return {
            .current_module = current_module,
            .current_loop = current_loop,
            .current_function = current_function,
        };
    }

    Node*        current_module{};
    Node*        current_loop{};
    types::Type* current_function{};
};

auto eval_node(Ast& ast, Node* node, State& state, Context& ctx) -> Value;
auto eval_node_to_type(Ast& ast, Node* node, State& state, Context& ctx)
    -> types::Type*;
void visit_node(Ast& ast, Node* node, State& state, Context& ctx);

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto eval_node_func_params(Ast& ast, Node* node, State& state, Context& ctx)
    -> Value {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto data = conv::func_params(*node);

    std::vector<std::pair<Node*, types::Type*>> types;

    for (auto child : data.params) {
        auto param = conv::func_param(*child);

        if (!param.type) {
            types.emplace_back(child, nullptr);
            continue;
        }

        auto ty = eval_node_to_type(ast, param.type, state, ctx);
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
auto eval_node_func_ret_pack(Ast& ast, Node* node, State& state, Context& ctx)
    -> Value {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto data = conv::func_ret_pack(*node);

    std::vector<std::pair<Node*, types::Type*>> types;

    for (auto child : data.ret) {
        if (child->is_oneof(ast::NodeKind::NamedRet)) {
            auto param = conv::named_ret(*child);

            if (!param.type) {
                types.emplace_back(child, nullptr);
                continue;
            }

            auto ty = eval_node_to_type(ast, param.type, state, ctx);
            types.emplace_back(child, ty);
            continue;
        }

        auto ty = eval_node_to_type(ast, child, state, ctx);
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

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto eval_node(Ast& ast, Node* node, State& state, Context& ctx) -> Value {
    auto& ts = *state.ts;
    auto& er = *state.er;

    if (node->is_oneof(ast::NodeKind::Ptr, ast::NodeKind::PtrConst)) {
        auto data = conv::ptr(*node);
        auto inner_ty = eval_node_to_type(ast, data.inner, state, ctx);
        auto ty = ts.new_ptr(inner_ty, data.is_const);
        return {.type = ts.get_type(), .data = ty};
    }

    if (node->is_oneof(ast::NodeKind::MultiPtr, ast::NodeKind::MultiPtrConst)) {
        auto data = conv::mptr(*node);
        auto inner_ty = eval_node_to_type(ast, data.inner, state, ctx);
        auto ty = ts.new_mptr(inner_ty, data.is_const);
        return {.type = ts.get_type(), .data = ty};
    }

    if (node->is_oneof(ast::NodeKind::StructType)) {
        auto data = conv::struct_type(*node);

        std::vector<types::Type*> fields;
        for (auto field : data.fields) {
            auto v = eval_node(ast, field, state, ctx);
            if (!v.type->is_type()) {
                er.report_warn(field->get_loc(),
                               "can not use non-type in field");
                continue;
            }

            auto ty = v.get_data_type();
            ASSERT(ty->kind == types::TypeKind::StructField);
            fields.push_back(ty);
        }

        return {.type = ts.get_type(), .data = ts.new_struct(fields)};
    }

    if (node->is_oneof(ast::NodeKind::StructField)) {
        auto data = conv::struct_field(*node);

        if (data.init) {
            auto v = eval_node(ast, data.init, state, ctx);
            er.report_bug(
                data.init->get_loc(),
                "default initializer for fields has not been implemented");
            er.report_note(data.init->get_loc(), "got value: {}", v);
        }

        auto type = eval_node(ast, data.type, state, ctx);
        if (!type.type->is_type()) {
            er.report_error(data.type->get_loc(),
                            "can not use non-type {} as type for field",
                            *type.type);
            return {.type = ts.get_error()};
        }

        return {.type = ts.get_type(),
                .data = ts.new_struct_field(data.name, type.get_data_type())};
    }

    if (node->is_oneof(ast::NodeKind::FuncParams)) {
        return eval_node_func_params(ast, node, state, ctx);
    }

    if (node->is_oneof(ast::NodeKind::FuncRetPack)) {
        return eval_node_func_ret_pack(ast, node, state, ctx);
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

auto eval_node_to_type(Ast& ast, Node* node, State& state, Context& ctx)
    -> types::Type* {
    ASSERT(node != nullptr);

    auto& ts = *state.ts;
    auto& er = *state.er;

    auto v = eval_node(ast, node, state, ctx);
    ASSERT(v.type != nullptr);

    if (!v.type->is_type()) {
        er.report_error(node->get_loc(), "can not use value of type {} as type",
                        *v.type);
        return ts.get_error();
    }

    if (!v.has_data()) {
        er.report_bug(node->get_loc(), "no data in compile-time value: {}", v);
        return ts.get_error();
    }

    return v.get_data_type();
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_decorators(Ast& ast, Node* decorators, Decl* decl, State& state,
                      Context& ctx) {
    auto& er = *state.er;

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
                    auto v = eval_node(ast, param.value, state, ctx);
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
                decl->flags.set_export();
                decl->link_name = decl->local_name;
            }

            // with paramters, validate
            else if (dec.params.size() == 1) {
                auto param = conv::decorator_param(*dec.params[0]);
                if (param.key == "link_name") {
                    auto v = eval_node(ast, param.value, state, ctx);
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
void visit_func_decl(Ast& ast, Node* node, State& state, Context& ctx) {
    auto visit = [&](State& state, Context& ctx, Node* node) {
        visit_node(ast, node, state, ctx);
    };

    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::func_decl(*node);
    auto  decl = node->get_decl();
    ASSERT(decl != nullptr);

    visit(state, ctx, data.decorators);
    visit(state, ctx, data.gargs);
    visit(state, ctx, data.args);
    visit(state, ctx, data.ret);

    visit_decorators(ast, data.decorators, decl, state, ctx);

    // handle main function, it needs to be handled specially. It should be
    // exported even without `@export`
    if (ctx.current_module && decl->local_name == "main" &&
        conv::flat_module(*ctx.current_module).name == "main") {
        decl->flags.set_export();
        decl->link_name = decl->local_name;
    }

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

    auto params = eval_node_to_type(ast, data.args, state, ctx);

    types::Type* ret = nullptr;
    if (data.ret)
        ret = eval_node_to_type(ast, data.ret, state, ctx);
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

    // if the function is namespaced, add it to the type
    auto ids = data.get_name().ids;
    if (ids.size() > 1) {
        if (ids.size() != 2) {
            er.report_bug(data.name->get_loc(),
                          "arbitrary nesting of namespacing on types has not "
                          "been implemented");
        }

        else {
            auto ty_node = ids[0];
            auto ty_decl = ty_node->get_decl();
            ASSERT(ty_decl != nullptr);
            ASSERT(ty_decl->value.type->is_type());

            auto ty = ty_decl->value.get_data_type();
            ts.add_function_to_type(*ty, conv::id(*ids[1]).name, d);
        }
    }

    // we only process the body later, so that all functions have already been
    // processed by the time we reach their bodies.
    state.pending_functions.push_back(node);

    // auto sctx = ctx.child(ty);
    // visit(sctx, data.body);
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void fixup_untyped_chain(types::TypeStore& ts, Node* chain,
                         types::Type* target) {
    ASSERT(chain != nullptr);
    ASSERT(chain->get_type() != nullptr);
    // ASSERT(chain->get_type()->is_untyped_int(), *chain->get_type());

    if (chain->is_oneof(ast::NodeKind::Int)) {
        chain->set_type(target);
        return;
    }

    if (chain->is_oneof(ast::NodeKind::Add, ast::NodeKind::Sub,
                        ast::NodeKind::Mul, ast::NodeKind::Div,
                        ast::NodeKind::Mod)) {
        chain->set_type(target);
        for (auto c : chain->get_children()) fixup_untyped_chain(ts, c, target);
        return;
    }

    if (chain->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*chain);
        ASSERT(data.items.size() == target->inner.size());
        ASSERT(target->kind == types::TypeKind::Pack);
        ASSERT(!target->contains_untyped());

        chain->set_type(target);
        for (auto [c, inner] : rv::zip(data.items, target->inner))
            fixup_untyped_chain(ts, c, inner);
        return;
    }

    if (chain->is_oneof(ast::NodeKind::Lit)) {
        auto unw = target;
        while (unw->is_distinct()) unw = unw->inner[0];

        ASSERT(unw->kind == types::TypeKind::Struct);
        auto data = conv::lit(*chain);

        std::unordered_map<std::string_view, types::Type*> expected_items;
        for (auto exp : unw->inner) expected_items[exp->id] = exp;

        for (auto item : data.items) {
            if (item->is_oneof(ast::NodeKind::LitParam)) {
                auto data = conv::lit_param(*item);
                auto it = expected_items.at(data.key);
                fixup_untyped_chain(ts, data.init, it->inner[0]);
                item->set_type(it);
            } else {
                PANIC("not implemented (and also not possible for now)");
            }
        }

        chain->set_type(target);
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
void set_type_of_id(Node* id_node, types::Type* type, State& state) {
    ASSERT(type != nullptr);
    ASSERT(id_node != nullptr);

    auto& er = *state.er;

    ASSERT(id_node->get_type() == nullptr);
    id_node->set_type(type);

    auto name = conv::id(*id_node);
    if (name.to) {
        ASSERT(name.to->get_type() == nullptr);
        name.to->value = {.type = type};
    } else {
        er.report_bug(id_node->get_loc(), "id has no decl: {:?}", name.name);
    }
}

auto coerce_and_check(types::Type* target, types::Type* source,
                      Node* target_node, Node* source_node, State& state,
                      Context& ctx) -> types::Type* {
    (void)ctx;

    auto& ts = *state.ts;
    auto& er = *state.er;

    auto r = ts.coerce(target, source);
    if (!r) {
        er.report_error(source_node->get_loc(),
                        "can not coerce value of type {} to type {}", *source,
                        *target);
        er.report_note(target_node->get_loc(), "required {} from here",
                       *target);
        r = ts.get_error();
    }

    return r;
}

auto coerce_check_and_fixup_source_ints(Ast& ast, types::Type* target,
                                        types::Type* source, Node* target_node,
                                        Node* source_node, State& state,
                                        Context& ctx)
    -> std::pair<types::Type*, Node*> {
    auto r =
        coerce_and_check(target, source, target_node, source_node, state, ctx);

    if (source->contains_untyped() && !r->is_err()) {
        ASSERT(!r->is_untyped_int());

        // fixup untyped integers
        fixup_untyped_chain(*state.ts, source_node, r);
    }

    if (!r->is_err() && *r != *source_node->get_type()) {
        auto c = ast.new_coerce(source_node->get_loc(), source_node, r);
        return std::make_pair(r, c);
    }

    return std::make_pair(r, nullptr);
}

auto coerce_check_and_fixup_simetric_ints(Ast& ast, types::Type* target,
                                          types::Type* source,
                                          Node* target_node, Node* source_node,
                                          State& state, Context& ctx)
    -> std::pair<types::Type*, Node*> {
    auto r =
        coerce_and_check(target, source, target_node, source_node, state, ctx);

    if (target->contains_untyped() && !source->contains_untyped()) {
        fixup_untyped_chain(*state.ts, target_node, r);
    }

    if (source->contains_untyped() && !target->contains_untyped()) {
        fixup_untyped_chain(*state.ts, source_node, r);
    }

    if (!r->is_err() && !r->is_untyped_int() &&
        *r != *source_node->get_type()) {
        auto c = ast.new_coerce(source_node->get_loc(), source_node, r);
        return std::make_pair(r, c);
    }

    return std::make_pair(r, nullptr);
}

auto coerce_check_and_fixup_simetric_ints_with_fallback(
    Ast& ast, types::Type* target, types::Type* source, types::Type* fallback,
    Node* target_node, Node* source_node, State& state, Context& ctx)
    -> std::pair<types::Type*, Node*> {
    auto r =
        coerce_and_check(target, source, target_node, source_node, state, ctx);

    if (target->contains_untyped() && !source->contains_untyped()) {
        fixup_untyped_chain(*state.ts, target_node, r);
    }

    if (source->contains_untyped() && !target->contains_untyped()) {
        fixup_untyped_chain(*state.ts, source_node, r);
    }

    if (source->contains_untyped() && target->contains_untyped()) {
        fixup_untyped_chain(*state.ts, target_node, fallback);
        fixup_untyped_chain(*state.ts, source_node, fallback);
    }

    if (!r->is_err() && !r->is_untyped_int() &&
        *r != *source_node->get_type()) {
        auto c = ast.new_coerce(source_node->get_loc(), source_node, r);
        return std::make_pair(r, c);
    }

    return std::make_pair(r, nullptr);
}

// ----------------------------------------------------------------------------

void fixup_expr_pack(types::TypeStore& ts, Node* pack) {
    if (pack == nullptr) return;

    auto inits = conv::expr_pack(*pack).items;

    std::vector<types::Type*> types;
    for (auto n : inits) {
        ASSERT(n != nullptr);
        ASSERT(n->get_type() != nullptr);
        types.push_back(n->get_type());
    }

    pack->set_type(ts.new_pack(types));
}

void fixup_expr_pack_lvalue(types::TypeStore& ts, Node* pack) {
    if (pack == nullptr) return;

    auto inits = conv::expr_pack(*pack).items;

    std::vector<types::Type*> types;
    for (auto n : inits) {
        ASSERT(n != nullptr);
        if (conv::is_discard_id(*n)) {
            types.push_back(ts.get_void());
            continue;
        }

        ASSERT(n->get_type() != nullptr);
        types.push_back(n->get_type());
    }

    pack->set_type(ts.new_pack(types));
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_decl_with_types_and_inits(Ast& ast, Node* ids_node, Node* types_node,
                                     Node* inits_node, State& state,
                                     Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto types = conv::expr_pack(*types_node).items;
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

                auto type = eval_node_to_type(ast, types[ty_idx], state, ctx);
                auto r =
                    coerce_and_check(type, i, types[ty_idx], init, state, ctx);

                // a function should not be able to return this
                ASSERT(!i->is_untyped_int());

                coercions.push_back(r);
                if (*r != *i) {
                    requires_coercion = true;
                }

                set_type_of_id(ids[name_idx], r, state);

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
                if (init_type->contains_untyped()) {
                    // fixup untyped integers
                    fixup_untyped_chain(*state.ts, init, ts.get_default_int());
                }

                name_idx++;
                ty_idx++;
                continue;
            }

            auto type = eval_node_to_type(ast, types[ty_idx], state, ctx);
            auto [r, coerce] = coerce_check_and_fixup_source_ints(
                ast, type, init_type, types[ty_idx], init, state, ctx);
            if (coerce) inits[init_idx] = coerce;

            set_type_of_id(ids[name_idx], r, state);

            name_idx++;
            ty_idx++;
        }
    }
}

void visit_decl_with_types(Ast& ast, Node* ids_node, Node* types_node,
                           State& state, Context& ctx) {
    auto& er = *state.er;

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

        auto type = eval_node_to_type(ast, ty, state, ctx);
        set_type_of_id(id, type, state);
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_decl_with_inits(Ast& ast, Node* ids_node, Node* inits_node,
                           State& state, Context& ctx) {
    (void)ast;
    (void)ctx;

    auto& ts = *state.ts;
    auto& er = *state.er;

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

                set_type_of_id(ids[name_idx], i, state);

                name_idx++;
            }
        }

        // a simple initializer expression
        else {
            if (conv::id(*ids[name_idx]).is_discard()) {
                if (init_type->contains_untyped()) {
                    // fixup untyped integers
                    fixup_untyped_chain(ts, init, ts.get_default_int());
                }

                name_idx++;
                continue;
            }

            if (init_type->contains_untyped()) {
                fixup_untyped_chain(ts, init, ts.get_default_int());
                init_type = init->get_type();
            }

            set_type_of_id(ids[name_idx], init_type, state);

            name_idx++;
        }
    }
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void eval_defined_values(Ast& ast, Node* decl, std::span<Node*> ids,
                         std::span<Node*> inits, State& state, Context& ctx) {
    auto& er = *state.er;
    auto& ts = *state.ts;
    if (ids.size() != inits.size()) {
        er.report_bug(decl->get_loc(),
                      "using multiple returns in def has not been implemented");
        return;
    }

    for (auto [id, init] : rv::zip(ids, inits)) {
        auto v = eval_node(ast, init, state, ctx);
        auto d = id->get_decl();
        ASSERT(d != nullptr);
        ASSERT(d->value.has_data() == false);
        ASSERT(d->value.type != nullptr);
        ASSERT(*d->value.type == *v.type);

        // wen we eval and define a type, create a distinct copy
        if (v.type->is_type()) {
            v.data = ts.new_distinct_of(d->full_name, v.get_data_type());
        }

        d->value = v;
    }
}

void visit_top_def_decl(Ast& ast, Node* node, State& state, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) {
        visit_node(ast, node, state, ctx);
    };

    auto& ts = *state.ts;
    auto& er = *state.er;

    auto top_data = conv::top_def_decl(*node);

    if (!top_data.get_decorators().items.empty()) {
        er.report_bug(top_data.decorators->get_loc(),
                      "decorators on `def` not implemented");
    }

    auto data = top_data.get_decl();
    visit(ctx, data.types);
    visit(ctx, data.inits);

    node->set_type(ts.get_void());

    // we have explicit types
    if (data.types && data.inits) {
        visit_decl_with_types_and_inits(ast, data.ids, data.types, data.inits,
                                        state, ctx);
        eval_defined_values(ast, top_data.decl, data.get_ids().ids,
                            data.get_inits().items, state, ctx);
    }

    // just types, not inits
    else if (data.types) {
        er.report_error(top_data.decl->get_loc(),
                        "declaration is missing initializers");
        er.report_note(
            top_data.decl->get_loc(),
            "initializers may be ommited on variables, but not constants");
    }

    // no explicit types but got inits
    else if (data.inits) {
        visit_decl_with_inits(ast, data.ids, data.inits, state, ctx);
        eval_defined_values(ast, top_data.decl, data.get_ids().ids,
                            data.get_inits().items, state, ctx);
    }

    // nothing given
    else {
        er.report_error(node->get_loc(), "no types in declaration");

        auto ids = data.get_ids();
        for (auto const& id : ids.ids) {
            set_type_of_id(id, ts.get_error(), state);
        }
    }

    // fixup the type of the init expr pack
    fixup_expr_pack(ts, data.inits);
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_var_decl(Ast& ast, Node* node, State& state, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) {
        visit_node(ast, node, state, ctx);
    };

    auto& ts = *state.ts;
    auto& er = *state.er;

    auto data = conv::var_decl(*node);
    visit(ctx, data.types);
    visit(ctx, data.inits);

    node->set_type(ts.get_void());

    // we have explicit types
    if (data.types && data.inits) {
        visit_decl_with_types_and_inits(ast, data.ids, data.types, data.inits,
                                        state, ctx);
    }

    // just types, not inits
    else if (data.types) {
        visit_decl_with_types(ast, data.ids, data.types, state, ctx);
    }

    // no explicit types but got inits
    else if (data.inits) {
        visit_decl_with_inits(ast, data.ids, data.inits, state, ctx);
    }

    // nothing given
    else {
        er.report_error(node->get_loc(), "no types in declaration");

        auto ids = data.get_ids();
        for (auto const& id : ids.ids) {
            set_type_of_id(id, ts.get_error(), state);
        }
    }

    // fixup the type of the init expr pack
    fixup_expr_pack(ts, data.inits);
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_assign(Ast& ast, Node* node, State& state, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) {
        visit_node(ast, node, state, ctx);
    };

    auto& ts = *state.ts;
    auto& er = *state.er;

    auto data = conv::assign(*node);
    auto lhs = data.get_lhs().items;
    auto rhs = data.get_rhs().items;

    node->set_type(ts.get_void());

    // lhs will be visited later
    visit(ctx, data.rhs);

    auto expr_count = count_init_exprs(data.get_rhs());

    if (lhs.size() != expr_count) {
        er.report_error(
            data.rhs->get_loc(),
            "wrong number of values for assigment, expected {} but got {}",
            lhs.size(), expr_count);
        er.report_note(data.lhs->get_loc(), "{} required from here",
                       lhs.size());
    }

    for (size_t lhs_idx{}; auto [rhs_idx, rhs_item] : rv::enumerate(rhs)) {
        if (lhs_idx >= lhs.size()) break;

        auto rhs_type = rhs_item->get_type();
        ASSERT(rhs_type != nullptr);

        // result of calling some function
        if (rhs_type->is_pack()) {
            std::vector<types::Type*> coercions;
            auto                      requires_coercion = false;

            // TODO: when a function call is assigned to _ (is discarded) and is
            // the only item in the assigment (to avoid ambiguity), allow using
            // a single _ despite the number of return values of the function.

            for (auto ty : rhs_type->inner) {
                if (lhs_idx >= lhs.size()) break;

                if (conv::is_discard_id(*lhs[lhs_idx])) {
                    coercions.push_back(ts.get_void());
                    lhs_idx++;
                    continue;
                }

                visit(ctx, lhs[lhs_idx]);

                auto r = coerce_and_check(lhs[lhs_idx]->get_type(), ty,
                                          lhs[lhs_idx], rhs_item, state, ctx);

                // a function should not be able to return this
                ASSERT(!ty->is_untyped_int());

                coercions.push_back(r);
                if (*r != *ty) {
                    requires_coercion = true;
                }

                lhs_idx++;
            }

            if (requires_coercion)
                rhs[rhs_idx] = ast.new_coerce(rhs_item->get_loc(), rhs_item,
                                              ts.new_pack(coercions));
        }

        // a simple initializer expression
        else {
            if (conv::is_discard_id(*lhs[lhs_idx])) {
                // need to fixup
                if (rhs_type->contains_untyped()) {
                    fixup_untyped_chain(ts, rhs_item, ts.get_default_int());
                }

                lhs_idx++;
                continue;
            }

            visit(ctx, lhs[lhs_idx]);

            auto [r, coerce] = coerce_check_and_fixup_source_ints(
                ast, lhs[lhs_idx]->get_type(), rhs_item->get_type(),
                lhs[lhs_idx], rhs_item, state, ctx);
            if (coerce) rhs[rhs_idx] = coerce;

            lhs_idx++;
        }
    }

    // fixup the type of the init expr pack
    fixup_expr_pack_lvalue(ts, data.lhs);
    fixup_expr_pack(ts, data.rhs);
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_node(Ast& ast, Node* node, State& state, Context& ctx) {
    if (node == nullptr) return;

    auto visit = [&](Context& ctx, Node* node) {
        visit_node(ast, node, state, ctx);
    };
    auto visit_span = [&](Context& ctx, std::span<Node* const> nodes) {
        for (auto node : nodes) visit_node(ast, node, state, ctx);
    };

    auto visit_children = [&](Context& ctx, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, State& state, Context& ctx) {
                visit_node(ast, node, state, ctx);
            },
            state, ctx);
    };

    auto& ts = *state.ts;
    auto& er = *state.er;

    // These should not reach sema
    if (node->is_oneof(ast::NodeKind::Module, ast::NodeKind::SourceFile,
                       ast::NodeKind::ModuleDecl)) {
        UNREACHABLE("found in sema node that should be removed by name-res",
                    node->get_kind());
    }

    if (node->is_oneof(ast::NodeKind::FlatModule)) {
        node->set_type(ts.get_void());

        auto mctx = ctx.with_module(node);

        visit_children(mctx, node);

        for (auto const& node : state.pending_functions) {
            auto data = conv::func_decl(*node);
            auto sctx = mctx.child(node->get_type());
            visit(sctx, data.body);
        }

        state.pending_functions.clear();
        return;
    }

    if (node->is_oneof(NodeKind::Decorators, NodeKind::Decorator,
                       NodeKind::FuncParams, NodeKind::FuncRetPack,
                       NodeKind::Block)) {
        node->set_type(ts.get_void());
        visit_children(ctx, node);
        return;
    }

    if (node->is_oneof(NodeKind::FuncDecl, NodeKind::FuncDeclWithCVarArgs)) {
        visit_func_decl(ast, node, state, ctx);
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

        auto type = eval_node_to_type(ast, data.type, state, ctx);
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

    if (node->is_oneof(ast::NodeKind::Deref)) {
        auto data = conv::unary(*node);
        visit(ctx, data.child);

        auto cty = data.child->get_type();
        ASSERT(cty != nullptr);

        if (cty->is_pack() && cty->inner.size() == 1) {
            cty = cty->inner[0];
        }

        if (!cty->is_ptr()) {
            er.report_error(node->get_loc(),
                            "can not dereference non-pointer type {}", *cty);
            node->set_type(ts.get_error());
            return;
        }

        node->set_type(cty->inner[0]);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Equal, ast::NodeKind::NotEqual,
                       ast::NodeKind::Less, ast::NodeKind::Greater,
                       ast::NodeKind::LessEqual, ast::NodeKind::GreaterEqual)) {
        auto data = conv::binary(*node);
        visit(ctx, data.lhs);
        visit(ctx, data.rhs);

        auto lhs = data.lhs->get_type();
        ASSERT(lhs != nullptr);
        auto rhs = data.rhs->get_type();
        ASSERT(rhs != nullptr);

        node->set_type(ts.get_bool());

        auto [r, coerce] = coerce_check_and_fixup_simetric_ints_with_fallback(
            ast, lhs, rhs, ts.get_default_int(), data.lhs, data.rhs, state,
            ctx);
        if (coerce) node->set_child(1, coerce);

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

        auto [r, coerce] = coerce_check_and_fixup_simetric_ints(
            ast, lhs, rhs, data.lhs, data.rhs, state, ctx);
        if (coerce) node->set_child(1, coerce);

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
        // if (data.is_discard() && ctx.is_lvalue) {
        //     node->set_type(ts.get_void());
        //     return;
        // }

        if (!data.to) {
            node->set_type(ts.get_error());
            return;
        }

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

    if (node->is_oneof(ast::NodeKind::StructType)) {
        visit_children(ctx, node);
        node->set_type(ts.get_type());
        return;
    }

    if (node->is_oneof(ast::NodeKind::StructField)) {
        auto data = conv::struct_field(*node);
        visit(ctx, data.type);
        visit(ctx, data.init);

        node->set_type(ts.get_type());

        auto type = data.type->get_type();
        ASSERT(type != nullptr);

        if (data.init) {
            auto init = data.init->get_type();
            ASSERT(init != nullptr);

            auto [_, coerce] = coerce_check_and_fixup_source_ints(
                ast, type, init, data.type, data.init, state, ctx);
            if (coerce) node->set_child(1, coerce);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Lit)) {
        auto data = conv::lit(*node);

        std::vector<types::Type*> types;
        for (auto it : data.items) {
            visit(ctx, it);

            ASSERT(it->get_type() != nullptr);
            types.push_back(it->get_type());
        }

        node->set_type(ts.new_lit(types));
        return;
    }

    if (node->is_oneof(ast::NodeKind::LitParam)) {
        auto data = conv::lit_param(*node);
        visit(ctx, data.init);

        auto type = data.init->get_type();
        ASSERT(type != nullptr);

        node->set_type(ts.new_lit_field(data.key, type));
        return;
    }

    if (node->is_oneof(ast::NodeKind::Call)) {
        auto data = conv::call(*node);
        visit(ctx, data.callee);

        auto cty = data.callee->get_type();
        ASSERT(cty != nullptr);

        if (!cty->is_func()) {
            if (!cty->is_err()) {
                er.report_error(node->get_loc(),
                                "can not call non-function value of type {}",
                                *cty);
            }

            visit_span(ctx, data.args);
            node->set_type(ts.get_error());
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

            auto [r, coerce] = coerce_check_and_fixup_source_ints(
                ast, param, arg->get_type(), data.callee, arg, state, ctx);
            if (coerce) node->set_child(i, coerce);
        }

        node->set_type(fn.ret);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Field)) {
        auto data = conv::field(*node);
        visit(ctx, data.receiver);

        auto rty = data.receiver->get_type();
        ASSERT(rty != nullptr);

        auto unw = rty->unwrapped();

        if (unw->is_strview()) {
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

        // struct fields
        if (unw->is_struct()) {
            auto fields = unw->as_struct_get_fields();
            auto it = fields.find(data.name);
            if (it == fields.end()) {
                er.report_error(node->get_loc(),
                                "type {} does not have a field named {:?}",
                                *rty, data.name);
                node->set_type(ts.get_error());
                return;
            }

            node->set_type(it->second);
            return;
        }

        // methods
        auto d = ts.get_function_from_type(*rty, data.name);
        if (d == nullptr) {
            er.report_error(node->get_loc(),
                            "type {} does not have field named {:?}", *rty,
                            data.name);
            node->set_type(ts.get_error());
            return;
        }

        node->set_type(ts.new_bound_from(d->value.type));

        ASSERT(node->get_decl() == nullptr);
        node->set_decl(d);

        return;
    }

    if (node->is_oneof(ast::NodeKind::Cast)) {
        auto data = conv::binary(*node);
        visit(ctx, data.lhs);
        visit(ctx, data.rhs);

        auto src = data.lhs->get_type();
        ASSERT(src != nullptr);

        auto target = eval_node_to_type(ast, data.rhs, state, ctx);
        auto r = ts.cast(target, src);
        if (!r) {
            er.report_error(node->get_loc(),
                            "can not cast value of type {} to type {}", *src,
                            *target);
            er.report_note(data.lhs->get_loc(), "this has type {}", *src);
            r = ts.get_error();
        }

        node->set_type(r);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        visit_top_def_decl(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::VarDecl)) {
        visit_var_decl(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Assign)) {
        visit_assign(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprStmt)) {
        auto data = conv::unary(*node);
        visit(ctx, data.child);

        ASSERT(data.child->get_type() != nullptr);
        if (!data.child->get_type()->is_void() &&
            !data.child->get_type()->is_err()) {
            er.report_warn(node->get_loc(),
                           "discard of non-void result of type {}",
                           *data.child->get_type());
        }

        node->set_type(ts.get_void());
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeferStmt)) {
        auto data = conv::defer_stmt(*node);
        visit(ctx, data.stmt);

        node->set_type(ts.get_void());
        return;
    }

    if (node->is_oneof(ast::NodeKind::IfStmt)) {
        auto data = conv::if_stmt(*node);
        visit(ctx, data.cond);

        auto cty = data.cond->get_type();
        ASSERT(cty != nullptr);

        if (!cty->is_bool()) {
            er.report_error(data.cond->get_loc(),
                            "wrong type for condition: {}, expected boolean",
                            *cty);
        }

        visit(ctx, data.wt);
        visit(ctx, data.wf);

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

        auto sctx = ctx.child(node);
        visit(sctx, data.body);

        node->set_type(ts.get_void());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Break)) {
        node->set_type(ts.get_void());

        if (ctx.current_loop == nullptr) {
            er.report_error(node->get_loc(),
                            "can not use break outside of loop");
            return;
        }

        // er.report_debug(node->get_loc(), "got break");
        // er.report_debug(ctx.current_loop->get_loc(), "for this loop");
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

        if (data.child == nullptr) {
            auto expected_rty = ctx.current_function->as_func().ret;
            ASSERT(expected_rty != nullptr);

            if (expected_rty->inner.size() == 1 &&
                expected_rty->inner[0]->is_void()) {
                return;
            }

            er.report_error(node->get_loc(),
                            "function expects a return value but got none");
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

        ASSERT(rty->is_pack());
        ASSERT(r->is_pack());
        if (rty->contains_untyped()) {
            ASSERT(!r->contains_untyped());
            fixup_untyped_chain(ts, data.child, r);
        }

        if (*r != *data.child->get_type()) {
            node->set_child(
                0, ast.new_coerce(data.child->get_loc(), data.child, r));
        }

        return;
    }

    if (state.opt->verbose_sema) {
        er.report_warn(node->get_loc(), "node not sema analized ({})",
                       node->get_kind());
    }

    visit_children(ctx, node);
}

void sema_ast(Ast& ast, Node* root, ErrorReporter& er, types::TypeStore& ts,
              Options const& opt) {
    auto state = State{.ts = &ts, .er = &er, .opt = &opt};
    auto ctx = Context{};
    visit_node(ast, root, state, ctx);
}

}  // namespace yal::sema
