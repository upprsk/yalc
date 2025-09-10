#include "sema.hpp"

#include <fmt/ranges.h>

#include <algorithm>
#include <string_view>

#include "error_reporter.hpp"
#include "location.hpp"
#include "node.hpp"
#include "symbol.hpp"
#include "types.hpp"

namespace yal::sema {

// ----------------------------------------------------------------------------

struct Scope {
    Scope*               parent = nullptr;
    Location             loc;
    std::vector<Symbol*> symbols;

    ast::Decl*   current_decl;
    SymbolStore* symbol_store;

    constexpr auto define(Symbol* sym) -> Symbol* {
        symbols.push_back(sym);
        return sym;
    }

    constexpr auto define(std::string_view name, Location name_loc, Value value,
                          bool is_local, bool is_const) -> Symbol* {
        auto sym =
            symbol_store->new_sym(name, name_loc, value, is_local, is_const);
        symbols.push_back(sym);
        return sym;
    }

    constexpr auto lookup(std::string_view name) -> Symbol* {
        if (auto sym = lookup_here(name)) return sym;
        return parent ? parent->lookup(name) : nullptr;
    }

    constexpr auto lookup_here(std::string_view name) -> Symbol* {
        auto it = std::ranges::find_last_if(
            symbols, [&](Symbol const* sym) { return sym->name == name; });

        if (it.begin() == symbols.end()) return nullptr;
        return *it.begin();
    }

    constexpr auto make_child(Location loc) -> Scope {
        return {.parent = this,
                .loc = loc,
                .symbols = {},
                .current_decl = current_decl,
                .symbol_store = symbol_store};
    }
};

struct State {
    ErrorReporter& er;
    ty::TypeStore& ts;
    Options const& opts;
};

// ----------------------------------------------------------------------------

auto get_default_int() -> ty::Type {
    // FIXME: use s32 or s64 depending on the platform

    return ty::make_int(8, true);  // s64
}

// ----------------------------------------------------------------------------

struct CoercionResult {
    ty::Type type;
    bool     source_requires_fixup = false;
    bool     requires_a_cast = false;
};

struct CastResult {
    ty::Type type;
    bool     source_requires_fixup = false;
    bool     redundant_cast = false;
};

struct UnifyResult {
    ty::Type type;
    bool     lhs_requires_fixup = false;
    bool     rhs_requires_fixup = false;
};

struct CoercionOpts {
    Location loc;
    Location source_loc;
    Location target_loc;
};

struct UnifyOpts {
    Location loc;
    Location lhs_loc;
    Location rhs_loc;

    ty::Type expected_type;
};

auto coerce_type(State& s, ty::Type source, ty::Type target,
                 CoercionOpts const& opts) -> CoercionResult {
    // comptime_int -> int OK
    // TODO: comptime_int -> float OK

    // don't deal with errors here, to avoid too many extra error messages when
    // something goes wrong
    if (source.is_err()) return {.type = target};

    if (source.is_comptime_int() && target.is_int()) {
        return {.type = target, .source_requires_fixup = true};
    }

    // If the kinds are different, then coercion is not possible
    if (source.kind != target.kind) {
        s.er.report_error(opts.loc, "can not coerce from {} to {}", source,
                          target);
        s.er.report_note(opts.source_loc, "this has type {}", source);
        s.er.report_note(opts.target_loc, "but expected type {} from here",
                         target);

        // TODO: could detect that some form of cast is possible:
        // - floats <-> ints
        // - arrays <-> slices
        return {.type = target};
    }

    // If the definitions are distinct, then coercion is not possible (requires
    // a cast)
    if (source.sym != target.sym) {
        s.er.report_error(
            opts.loc, "can not coerce from {} to {}, these are distinct types",
            source, target);
        s.er.report_note(opts.source_loc, "this has type {}", source);
        s.er.report_note(opts.target_loc, "but expected type {} from here",
                         target);

        return {.type = target, .requires_a_cast = true};
    }

    // both are integers
    if (target.is_int()) {
        auto const& source_int = source.as.integer;
        auto const& target_int = target.as.integer;

        if (source_int.byte_size != target_int.byte_size) {
            s.er.report_error(
                opts.loc,
                "can not coerce from {} to {}, integers differ in size", source,
                target);
            s.er.report_note(opts.source_loc, "this has type {}", source);
            s.er.report_note(opts.target_loc, "but expected type {} from here",
                             target);

            return {.type = target, .requires_a_cast = true};
        }

        if (source_int.is_signed != target_int.is_signed) {
            s.er.report_error(
                opts.loc,
                "can not coerce from {} to {}, integers differ in signness",
                source, target);
            s.er.report_note(opts.source_loc, "this has type {}", source);
            s.er.report_note(opts.target_loc, "but expected type {} from here",
                             target);

            return {.type = target, .requires_a_cast = true};
        }

        // should both actually be the same, nice!
        return {.type = target};
    }

    PANIC("coerction: type combination not implemented", source, target);
}

auto unify_types(State& s, ty::Type lhs, ty::Type rhs, UnifyOpts const& opts)
    -> UnifyResult {
    // don't deal with errors here, to avoid too many extra error messages when
    // something goes wrong
    if (lhs.is_err()) return {.type = rhs};
    if (rhs.is_err()) return {.type = lhs};

    if (lhs.is_comptime_int() && rhs.is_int()) {
        return {.type = rhs, .lhs_requires_fixup = true};
    }

    if (lhs.is_int() && rhs.is_comptime_int()) {
        return {.type = lhs, .rhs_requires_fixup = true};
    }

    if (lhs.kind != rhs.kind) {
        s.er.report_error(opts.loc, "incompatible types: {} and {}", lhs, rhs);
        s.er.report_note(opts.lhs_loc, "this has type {}", lhs);
        s.er.report_note(opts.rhs_loc, "this has type {}", rhs);

        // TODO: could detect that some form of cast is possible:
        // - floats <-> ints
        // - arrays <-> slices
        return {.type =
                    opts.expected_type.is_valid() ? opts.expected_type : lhs};
    }

    // If the definitions are distinct, then coercion is not possible (requires
    // a cast)
    if (lhs.sym != rhs.sym) {
        s.er.report_error(
            opts.loc, "incompatible types: {} and {}, these are distinct types",
            lhs, rhs);
        s.er.report_note(opts.lhs_loc, "this has type {}", lhs);
        s.er.report_note(opts.rhs_loc, "this has type {}", rhs);

        return {.type =
                    opts.expected_type.is_valid() ? opts.expected_type : lhs};
    }

    // both are integers
    if (lhs.is_int()) {
        auto const& source_int = lhs.as.integer;
        auto const& target_int = rhs.as.integer;

        if (source_int.byte_size != target_int.byte_size) {
            s.er.report_error(
                opts.loc,
                "incompatible types: {} and {}, integers differ in size", lhs,
                rhs);
            s.er.report_note(opts.lhs_loc, "this has type {}", lhs);
            s.er.report_note(opts.rhs_loc, "this has type {}", rhs);

            return {.type = opts.expected_type.is_valid() ? opts.expected_type
                                                          : lhs};
        }

        if (source_int.is_signed != target_int.is_signed) {
            s.er.report_error(
                opts.loc,
                "incompatible types: {} and {}, integers differ in signness",
                lhs, rhs);
            s.er.report_note(opts.lhs_loc, "this has type {}", lhs);
            s.er.report_note(opts.rhs_loc, "this has type {}", rhs);

            return {.type = opts.expected_type.is_valid() ? opts.expected_type
                                                          : lhs};
        }

        // should both actually be the same, nice!
        return {.type = lhs};
    }

    s.er.report_error(opts.loc, "incompatible types: {} and {}", lhs, rhs);
    s.er.report_note(opts.lhs_loc, "this has type {}", lhs);
    s.er.report_note(opts.rhs_loc, "this has type {}", rhs);
    return {.type = opts.expected_type.is_valid() ? opts.expected_type : lhs};
}

auto cast_type(State& s, ty::Type source, ty::Type target,
               CoercionOpts const& opts) -> CastResult {
    // don't deal with errors here, to avoid too many extra error messages when
    // something goes wrong
    if (source.is_err() || target.is_err()) return {.type = target};

    if (source.is_comptime_int() && target.is_int()) {
        return {.type = target, .source_requires_fixup = true};
    }

    // both are integers, can cast without much problem
    if (source.is_int() && target.is_int()) {
        auto is_redundant_cast =
            source.sym == target.sym &&
            source.as.integer.byte_size == target.as.integer.byte_size &&
            source.as.integer.is_signed == target.as.integer.is_signed;

        if (is_redundant_cast) {
            s.er.report_warn(opts.loc, "redundant cast from {} to {}", source,
                             target);
        }

        return {.type = target, .redundant_cast = is_redundant_cast};
    }

    s.er.report_error(opts.loc, "can not cast from {} to {}", source, target);
    return {.type = target};
}

auto type_supports_operator(ty::Type type, ast::ExprKind op) -> bool {
    switch (op) {
        case ast::ExprKind::Err: return true;  // to avoid bogus messages

        case ast::ExprKind::Neg:
        case ast::ExprKind::Add:
        case ast::ExprKind::Sub:
        case ast::ExprKind::Mul:
        case ast::ExprKind::Div:
        case ast::ExprKind::Mod: return type.is_int() || type.is_comptime_int();

        default: PANIC("invalid op received", op);
    }
}

auto operator_to_sym(ast::ExprKind op) -> std::string_view {
    std::string_view name = "?";
    switch (op) {
        case ast::ExprKind::Err: name = "Err"; break;

        case ast::ExprKind::Neg: name = "negate"; break;
        case ast::ExprKind::Add: name = "+"; break;
        case ast::ExprKind::Sub: name = "-"; break;
        case ast::ExprKind::Mul: name = "*"; break;
        case ast::ExprKind::Div: name = "/"; break;
        case ast::ExprKind::Mod: name = "%"; break;

        default: PANIC("invalid op received", op);
    }

    return name;
}

auto remove_ptr(ty::Type ty) -> ty::Type {
    if (ty.is_ptr()) return ty.as.ptr->inner;

    // this is not technically correct, but should give better error messages
    if (ty.is_multi_ptr()) return ty.as.ptr->inner;
    if (ty.is_slice()) return ty.as.ptr->inner;

    return {};  // error type
}

// ============================================================================

auto eval_expr_to_type(State& s, Scope& scope, ast::Expr* expr) -> ty::Type;

auto eval_expr(State& s, Scope& scope, ast::Expr* expr) -> Value {
    if (!expr) {
        s.er.report_error(scope.loc,
                          "in this scope: can not evaluate missing expression");

        return {};  // return error value
    }

    switch (expr->kind) {
        case ast::ExprKind::Err:

        case ast::ExprKind::Neg:
        case ast::ExprKind::Add:
        case ast::ExprKind::Sub:
        case ast::ExprKind::Mul:
        case ast::ExprKind::Div:
        case ast::ExprKind::Mod:
        case ast::ExprKind::Deref:
        case ast::ExprKind::Ref: break;

        case ast::ExprKind::Cast: break;

        case ast::ExprKind::Field:
        case ast::ExprKind::Call: break;

        case ast::ExprKind::Ptr: {
            auto& ptr = expr->as_ptr();

            auto inner = eval_expr_to_type(s, scope, ptr.inner);
            auto type = s.ts.type_ptr(inner, ptr.is_const);
            return {.type = ty::make_type(), .as = {.type = type}};
        } break;

        case ast::ExprKind::MultiPtr: {
            auto& ptr = expr->as_ptr();

            auto inner = eval_expr_to_type(s, scope, ptr.inner);
            auto type = s.ts.type_multi_ptr(inner, ptr.is_const);
            return {.type = ty::make_type(), .as = {.type = type}};
        } break;

        case ast::ExprKind::Slice: {
            auto& ptr = expr->as_ptr();

            auto inner = eval_expr_to_type(s, scope, ptr.inner);
            auto type = s.ts.type_slice(inner, ptr.is_const);
            return {.type = ty::make_type(), .as = {.type = type}};
        } break;

        case ast::ExprKind::Array: break;

        case ast::ExprKind::Id: {
            auto& id = expr->as_id();
            if (!id.sym) return {};  // return error value
            return id.sym->value;
        } break;

        case ast::ExprKind::Kw: break;

        case ast::ExprKind::Int: {
            auto& integer = expr->as_int();

            return {
                .type = expr->type,
                .as = {.integer = {.value = integer.value, .has_value = true}}};
        } break;

        case ast::ExprKind::String: break;
    }

    PANIC("EVAL: not implemented", *expr);
}

auto eval_expr_to_type(State& s, Scope& scope, ast::Expr* expr) -> ty::Type {
    auto v = eval_expr(s, scope, expr);
    if (v.type.kind != ty::TypeKind::Type) {
        if (v.type.kind != ty::TypeKind::Err) {
            auto loc = expr ? expr->loc : scope.loc;
            s.er.report_error(loc, "can not use value of type {} as type",
                              v.type);
        }

        return {};  // return error type
    }

    return v.as.type;
}

// ============================================================================

void fixup_comptime_integers_in_expr(State& s, ast::Expr* expr,
                                     ty::Type target_type) {
    ASSERT(expr->type.is_comptime_int());
    ASSERT(target_type.is_int());

    switch (expr->kind) {
        case ast::ExprKind::Err: break;

        case ast::ExprKind::Neg:
            PANIC("FIXUP comptime_int: not implemented", *expr, target_type);
            break;

        case ast::ExprKind::Add:
        case ast::ExprKind::Sub:
        case ast::ExprKind::Mul:
        case ast::ExprKind::Div:
        case ast::ExprKind::Mod:
            fixup_comptime_integers_in_expr(s, expr->as_arith().lhs,
                                            target_type);
            fixup_comptime_integers_in_expr(s, expr->as_arith().rhs,
                                            target_type);
            break;

        case ast::ExprKind::Deref:
        case ast::ExprKind::Ref: break;

        case ast::ExprKind::Cast:
        case ast::ExprKind::Field:
        case ast::ExprKind::Call:
        case ast::ExprKind::Ptr:
        case ast::ExprKind::MultiPtr:
        case ast::ExprKind::Slice:
        case ast::ExprKind::Array:
            PANIC("FIXUP comptime_int: not implemented", *expr, target_type);
            break;

        case ast::ExprKind::Id: expr->type = target_type; break;

        case ast::ExprKind::Kw: break;

        case ast::ExprKind::Int: expr->type = target_type; break;

        case ast::ExprKind::String: break;
    }
}

// ============================================================================

void sema_expr(State& s, Scope& scope, ast::Expr* expr, ty::Type expected_type);

// ----------------------------------------------------------------------------

struct RvalueInfo {
    bool is_rvalue = false;
    bool is_const = false;
};

auto calc_rvalue_info(ast::Expr* expr) -> RvalueInfo {
    if (!expr)
        // not rvalue
        return {};

    switch (expr->kind) {
        case ast::ExprKind::Err:
        case ast::ExprKind::Neg:
        case ast::ExprKind::Add:
        case ast::ExprKind::Sub:
        case ast::ExprKind::Mul:
        case ast::ExprKind::Div:
        case ast::ExprKind::Mod:
        case ast::ExprKind::Cast:
            // not rvalue
            return {};

        case ast::ExprKind::Field:
            // is an rvalue depending on obj
            // FIXME: check when this is ok, for now it is not an rvalue
            return {};

        case ast::ExprKind::Call:
            // not an rvalue
            return {};

        case ast::ExprKind::Deref:
        case ast::ExprKind::Ref:
            // not an rvalue
            return {};

        case ast::ExprKind::Ptr:
        case ast::ExprKind::MultiPtr:
        case ast::ExprKind::Slice:
            // not an rvalue
            return {};

        case ast::ExprKind::Array:
            // FIXME: this would be an rvalue
            return {};

        case ast::ExprKind::Id: {
            auto& id = expr->as_id();
            ASSERT(id.sym != nullptr);

            // this is an rvalue, and may or may not be a constant depending on
            // the symbol
            return {.is_rvalue = true, .is_const = id.sym->is_const};
        }

        case ast::ExprKind::Kw:
            // not an rvalue
            return {};

        case ast::ExprKind::Int:
            // not an rvalue
            return {};

        case ast::ExprKind::String:
            // NOTE: is this an rvalue? can we do something here? For now it is
            // not an rvalue
            return {};
    }

    UNREACHABLE(expr);
}

// ----------------------------------------------------------------------------

void sema_expr_arith(State& s, Scope& scope, ast::ArithExpr& expr,
                     ty::Type expected_type) {
    sema_expr(s, scope, expr.lhs, expected_type);
    sema_expr(s, scope, expr.rhs, expected_type);

    if (expr.lhs && expr.rhs) {
        auto result = unify_types(s, expr.lhs->type, expr.rhs->type,
                                  {.loc = expr.loc,
                                   .lhs_loc = expr.lhs->loc,
                                   .rhs_loc = expr.rhs->loc,
                                   .expected_type = expected_type});
        if (result.lhs_requires_fixup) {
            fixup_comptime_integers_in_expr(s, expr.lhs, result.type);
        }
        if (result.rhs_requires_fixup) {
            fixup_comptime_integers_in_expr(s, expr.rhs, result.type);
        }

        // FIXME: handle when an implicit conversion happens, as that
        // requires an additonal AST node.

        if (s.opts.verbose_coercions) {
            s.er.report_debug(expr.loc,
                              "{} <-> {} result={} (lhs_requires_fixup={}, "
                              "rhs_requires_fixup={})",
                              expr.lhs->type, expr.rhs->type, result.type,
                              result.lhs_requires_fixup ? "yes" : "no",
                              result.rhs_requires_fixup ? "yes" : "no");
        }

        expr.type = result.type;
    }

    // check that the operator is supported by the type
    if (expr.type.is_valid()) {
        if (!type_supports_operator(expr.type, expr.kind)) {
            s.er.report_error(expr.loc,
                              "operator {} can not be used with type {}",
                              operator_to_sym(expr.kind), expr.type);
        }
    }
}

void sema_expr_cast(State& s, Scope& scope, ast::CastExpr& expr,
                    ty::Type expected_type) {
    if (!expr.type_is_infer()) {
        sema_expr(s, scope, expr.type_expr, ty::make_type());
    }

    auto type = ty::Type{};

    if (!expr.type_is_infer()) {
        type = eval_expr_to_type(s, scope, expr.type_expr);
    } else {
        type = expected_type;
        if (!type.is_valid()) {
            s.er.report_error(expr.loc, "could not infer type for cast");
        }
    }

    sema_expr(s, scope, expr.child, type);

    // now we need to cast it
    if (expr.type_expr && expr.child) {
        auto result = cast_type(s, expr.child->type, type,
                                {.loc = expr.loc,
                                 .source_loc = expr.child->loc,
                                 .target_loc = expr.type_expr->loc});

        if (result.source_requires_fixup) {
            fixup_comptime_integers_in_expr(s, expr.child, result.type);
        }

        // FIXME: handle when an implicit conversion happens, as that
        // requires an additonal AST node.

        if (s.opts.verbose_coercions) {
            s.er.report_debug(expr.loc,
                              "{} -> {} result={} (source_requires_fixup={})",
                              expr.child->type, type, result.type,
                              result.source_requires_fixup ? "yes" : "no");
        }

        type = result.type;
    }

    expr.type = type;
}

void sema_expr_deref(State& s, Scope& scope, ast::ArithExpr& expr,
                     ty::Type expected_type) {
    // FIXME: we need a way to make the expected_type for the child to work,
    // i.e. make it into a pointer?
    expected_type = {};
    sema_expr(s, scope, expr.lhs, expected_type);
    if (expr.lhs) {
        if (expr.lhs->type.is_ptr()) {
            expr.type = expr.lhs->type.as.ptr->inner;
        } else {
            s.er.report_error(
                expr.lhs->loc,
                "can not dereference value of non-pointer type {}",
                expr.lhs->type);
        }
    }
}

void sema_expr_ref(State& s, Scope& scope, ast::ArithExpr& expr,
                   ty::Type expected_type) {
    sema_expr(s, scope, expr.lhs, remove_ptr(expected_type));

    // NOTE: there might be some bugs sneaking in here
    auto info = calc_rvalue_info(expr.lhs);
    if (!info.is_rvalue) {
        s.er.report_error(expr.loc, "can not take address of lvalue");
    }

    if (expr.lhs) {
        if (expr.lhs->type.is_comptime_int()) {
            // can not take address of comptime_int!
            s.er.report_error(expr.lhs->loc,
                              "can not take address of comptime_int");
            // :)
            expr.lhs->type = get_default_int();
        }

        expr.type = s.ts.type_ptr(expr.lhs->type, info.is_const);
    }
}

void sema_expr(State& s, Scope& scope, ast::Expr* expr,
               ty::Type expected_type) {
    if (!expr) return;

    switch (expr->kind) {
        case ast::ExprKind::Err: break;

        case ast::ExprKind::Neg:

        case ast::ExprKind::Add:
        case ast::ExprKind::Sub:
        case ast::ExprKind::Mul:
        case ast::ExprKind::Div:
        case ast::ExprKind::Mod:
            sema_expr_arith(s, scope, expr->as_arith(), expected_type);
            break;

        case ast::ExprKind::Cast:
            sema_expr_cast(s, scope, expr->as_cast(), expected_type);
            break;

        case ast::ExprKind::Field:
        case ast::ExprKind::Call: PANIC("SEMA EXPR: not implemented", *expr);

        case ast::ExprKind::Deref:
            sema_expr_deref(s, scope, expr->as_arith(), expected_type);
            break;
        case ast::ExprKind::Ref:
            sema_expr_ref(s, scope, expr->as_arith(), expected_type);
            break;

        case ast::ExprKind::Ptr:
        case ast::ExprKind::MultiPtr:
        case ast::ExprKind::Slice: {
            auto& ptr = expr->as_ptr();

            sema_expr(s, scope, ptr.inner, ty::make_type());
            if (ptr.inner && !ptr.inner->type.is_type()) {
                s.er.report_error(ptr.inner->loc,
                                  "can not use value of type {} as type",
                                  ptr.inner->type);
            }

            ptr.type = ty::make_type();
        } break;

        case ast::ExprKind::Array: PANIC("SEMA EXPR: not implemented", *expr);

        case ast::ExprKind::Id: {
            auto& id = expr->as_id();
            if (auto sym = scope.lookup(id.value)) {
                id.sym = sym;
                id.type = sym->value.type;
            } else {
                s.er.report_error(expr->loc, "undefined identifier {:?}",
                                  id.value);
            }
        } break;

        case ast::ExprKind::Kw: PANIC("SEMA EXPR: not implemented", *expr);

        case ast::ExprKind::Int: {
            auto& integer = expr->as_int();

            if (expected_type.is_int()) {
                integer.type = expected_type;
                // FIXME: check that the literal fits in the expected type
            } else {
                integer.type = ty::make_comptime_int();
            }
        } break;

        case ast::ExprKind::String: PANIC("SEMA EXPR: not implemented", *expr);
    }
}

// ============================================================================

struct VarDesc {
    ast::Expr* type_expr;
    ast::Expr* init;
    Location   loc;

    bool should_fixup;
};

auto sema_some_var(State& s, Scope& scope, VarDesc const& var) -> ty::Type {
    // var x;              // type=null and init=null -> error
    // var x: type;        // init=null -> ok
    // var x = init;       // type=null -> ok
    // var x: type = init; // ok

    sema_expr(s, scope, var.type_expr, ty::make_type());

    auto expected_type = ty::Type{};
    if (var.type_expr) {
        expected_type = eval_expr_to_type(s, scope, var.type_expr);
    }

    sema_expr(s, scope, var.init, expected_type);

    if (var.type_expr == nullptr && var.init == nullptr) {
        s.er.report_error(var.loc,
                          "variable declaration requires at least type or "
                          "initializer, got none");
    }

    else if (var.type_expr != nullptr && var.init == nullptr) {
        // only got the type, so there is nothing to do here.
    }

    else if (var.type_expr == nullptr && var.init != nullptr) {
        expected_type = var.init->type;

        // in case the type of the expression is comptime_int, then we need to
        // move it to the default integer type
        if (expected_type.is_comptime_int() && var.should_fixup) {
            expected_type = get_default_int();
            fixup_comptime_integers_in_expr(s, var.init, expected_type);
        }
    }

    else {
        // we may have failed to get the type from type_expr, in such case we
        // don't do anything here
        if (expected_type.is_valid()) {
            auto result = coerce_type(s, var.init->type, expected_type,
                                      {.loc = var.loc,
                                       .source_loc = var.type_expr->loc,
                                       .target_loc = var.init->loc});
            if (result.source_requires_fixup) {
                fixup_comptime_integers_in_expr(s, var.init, result.type);
            }

            // FIXME: handle when an implicit conversion happens, as that
            // requires an additonal AST node.

            if (s.opts.verbose_coercions) {
                s.er.report_debug(var.loc,
                                  "{} -> {} result={} (requires_a_cast={}, "
                                  "source_requires_fixup={})",
                                  var.init->type, expected_type, result.type,
                                  result.requires_a_cast ? "yes" : "no",
                                  result.source_requires_fixup ? "yes" : "no");
            }

            expected_type = result.type;
        }
    }

    return expected_type;
}

// ============================================================================

void sema_func_params(State& s, Scope& scope, ast::FuncDecl& decl) {
    for (auto& p : decl.params) {
        auto param_scope = scope.make_child(p.loc);
        sema_expr(s, param_scope, p.type_expr, ty::make_type());

        if (p.type_expr) {
            p.type = eval_expr_to_type(s, param_scope, p.type_expr);
        }

        p.sym =
            scope.define(p.name, p.loc, {.type = p.type, .as = {}}, true, true);
    }
}

void sema_func_rets(State& s, Scope& scope, ast::FuncDecl& decl) {
    for (auto& r : decl.rets) {
        auto ret_scope = scope.make_child(r.loc);
        sema_expr(s, ret_scope, r.type_expr, ty::make_type());

        if (r.type_expr) {
            r.type = eval_expr_to_type(s, ret_scope, r.type_expr);
        }
    }
}

void sema_fixup_func_params(State& s, ast::FuncDecl& decl) {
    ty::Type* latest_type = nullptr;
    for (auto& p : std::views::reverse(decl.params)) {
        if (p.type_expr == nullptr) {
            if (latest_type == nullptr) {
                s.er.report_error(
                    p.loc, "missing type for function parameter {:?}", p.name);
                continue;
            }

            p.type = *latest_type;
            p.sym->value.type = p.type;
        } else {
            latest_type = &p.type;
        }
    }
}

void sema_fixup_func_rets(State& s, ast::FuncDecl& decl) {
    ty::Type* latest_type = nullptr;
    for (auto& r : std::views::reverse(decl.rets)) {
        if (r.type_expr == nullptr) {
            if (latest_type == nullptr) {
                s.er.report_error(r.loc, "missing type for function return");
                continue;
            }

            r.type = *latest_type;
        } else {
            latest_type = &r.type;
        }
    }
}

auto create_func_type(State& s, ast::FuncDecl& decl) -> ty::Type {
    std::vector<ty::Type> param_types;
    std::vector<ty::Type> ret_types;

    param_types.reserve(decl.params.size());
    ret_types.reserve(decl.rets.size());

    for (auto const& p : decl.params) param_types.push_back(p.type);
    for (auto const& r : decl.rets) ret_types.push_back(r.type);

    return s.ts.type_func(param_types, ret_types);
}

void sema_func_decl_header(State& s, Scope& parent_scope, ast::FuncDecl& decl) {
    ASSERT(decl.attached_type == "", decl.name,
           "attached types have not been implemented yet");

    decl.sym = parent_scope.define(decl.name, decl.name_loc, {}, false, true);

    auto scope = parent_scope.make_child(decl.name_loc);
    scope.current_decl = &decl;

    ASSERT(decl.attributes.size() == 0, decl.name,
           "attributes have not been implemented yet");

    sema_func_params(s, scope, decl);
    sema_func_rets(s, scope, decl);

    sema_fixup_func_params(s, decl);
    sema_fixup_func_rets(s, decl);

    auto type = create_func_type(s, decl);
    decl.sym->value = {.type = type, .as = {.func_decl = &decl}};
}

void sema_var_decl_header(State& s, Scope& parent_scope, ast::VarDecl& decl) {
    auto scope = parent_scope.make_child(decl.name_loc);
    scope.current_decl = &decl;

    ASSERT(decl.attributes.size() == 0, decl.name,
           "attributes have not been implemented yet");

    auto expected_type = sema_some_var(s, scope,
                                       {.type_expr = decl.type_expr,
                                        .init = decl.init,
                                        .loc = decl.loc,
                                        .should_fixup = true});

    decl.sym =
        parent_scope.define(decl.name, decl.name_loc,
                            {.type = expected_type, .as = {}}, false, false);
}

void sema_def_decl_header(State& s, Scope& parent_scope, ast::VarDecl& decl) {
    // alread define the thing, as it may be needed recursivelly
    decl.sym = parent_scope.define(decl.name, decl.name_loc, {}, false, true);

    auto scope = parent_scope.make_child(decl.name_loc);
    scope.current_decl = &decl;

    ASSERT(decl.attributes.size() == 0, decl.name,
           "attributes have not been implemented yet");

    auto expected_type = sema_some_var(s, scope,
                                       {.type_expr = decl.type_expr,
                                        .init = decl.init,
                                        .loc = decl.loc,
                                        .should_fixup = false});

    if (decl.init == nullptr) {
        s.er.report_error(decl.loc, "constants must have an initializer");
    }

    auto value = eval_expr(s, scope, decl.init);
    // FIXME: handle coercing value to expected_type

    decl.sym->value = value;
}

// ----------------------------------------------------------------------------

void sema_decl_header(State& s, Scope& scope, ast::Decl* decl) {
    if (!decl) return;

    switch (decl->kind) {
        case ast::DeclKind::Err: break;

        case ast::DeclKind::Import:
            PANIC("SEMA header: imports not implemented", *decl);

        case ast::DeclKind::Func:
            sema_func_decl_header(s, scope, decl->as_func());
            break;

        case ast::DeclKind::Var:
            sema_var_decl_header(s, scope, decl->as_var());
            break;

        case ast::DeclKind::Def:
            sema_def_decl_header(s, scope, decl->as_def());
            break;

        case ast::DeclKind::MultiVar:
        case ast::DeclKind::MultiDef:
            PANIC("SEMA header: var and def not implemented", *decl);
    }
}

// ============================================================================

auto get_current_function_type(Scope& scope) -> ty::TypeFunc* {
    if (scope.current_decl->kind != ast::DeclKind::Func) return nullptr;

    auto sym = scope.current_decl->as_func().sym;
    ASSERT(sym != nullptr, *scope.current_decl);
    ASSERT(sym->value.type.is_func(), *scope.current_decl);

    return sym->value.type.as.func;
}

void sema_stmt_return(State& s, Scope& scope, ast::ReturnStmt& stmt) {
    ASSERT(scope.current_decl != nullptr, stmt);

    auto                          expected_returns_loc = stmt.loc;
    std::span<ty::Type const>     expected_returns;
    std::span<ast::FuncRet const> expected_return_rename_later;

    auto current_function = get_current_function_type(scope);
    if (current_function == nullptr) {
        s.er.report_error(stmt.loc, "use of return outside of function");
    } else {
        expected_returns = current_function->rets;
        expected_returns_loc = scope.current_decl->as_func().rets_loc;
        expected_return_rename_later = scope.current_decl->as_func().rets;

        if (stmt.children.size() != expected_returns.size()) {
            s.er.report_error(
                stmt.children_loc,
                "incorrect number of return values, expected {} but got {}",
                expected_returns.size(), stmt.children.size());

            s.er.report_note(expected_returns_loc,
                             "{} return values declared here",
                             expected_returns.size());
        }
    }

    for (auto const& [idx, r] : std::views::enumerate(stmt.children)) {
        auto expected_type = ty::Type{};
        if (static_cast<size_t>(idx) < expected_returns.size())
            expected_type = expected_returns[idx];

        sema_expr(s, scope, r, expected_type);
    }

    // now check that all types do actually match

    auto len = std::min({stmt.children.size(), expected_returns.size(),
                         expected_return_rename_later.size()});
    for (size_t i = 0; i < len; ++i) {
        auto const& r = stmt.children[i];
        auto const& r_type = expected_returns[i];
        auto const& r_decl = expected_return_rename_later[i];

        auto result = coerce_type(
            s, r->type, r_type,
            {.loc = r->loc, .source_loc = r->loc, .target_loc = r_decl.loc});
        if (result.source_requires_fixup) {
            fixup_comptime_integers_in_expr(s, r, result.type);
        }

        // FIXME: handle when an implicit conversion happens, as that requires
        // an additonal AST node.

        if (s.opts.verbose_coercions) {
            s.er.report_debug(r->loc,
                              "{} -> {} result={} (requires_a_cast={}, "
                              "source_requires_fixup={})",
                              r->type, r_type, result.type,
                              result.requires_a_cast ? "yes" : "no",
                              result.source_requires_fixup ? "yes" : "no");
        }
    }
}

void sema_stmt_var(State& s, Scope& scope, ast::VarStmt& stmt) {
    auto expected_type = sema_some_var(s, scope,
                                       {.type_expr = stmt.type_expr,
                                        .init = stmt.init,
                                        .loc = stmt.loc,
                                        .should_fixup = true});

    stmt.sym = scope.define(stmt.name, stmt.name_loc,
                            {.type = expected_type, .as = {}}, true, false);
}

void sema_stmt(State& s, Scope& scope, ast::Stmt* stmt) {
    if (!stmt) return;

    switch (stmt->kind) {
        case ast::StmtKind::Err: break;

        case ast::StmtKind::Block: {
            auto& block = stmt->as_block();
            auto  block_scope = scope.make_child(stmt->loc);

            for (auto const& child : block.children) {
                sema_stmt(s, block_scope, child);
            }
        } break;

        case ast::StmtKind::Return:
            sema_stmt_return(s, scope, stmt->as_return());
            break;

        case ast::StmtKind::Expr: PANIC("SEMA: not implemented", *stmt);

        case ast::StmtKind::Var: sema_stmt_var(s, scope, stmt->as_var()); break;

        case ast::StmtKind::Def:
        case ast::StmtKind::MultiVar:
        case ast::StmtKind::MultiDef:
        case ast::StmtKind::Assign:
        case ast::StmtKind::MultiAssign: PANIC("SEMA: not implemented", *stmt);
    }
}

// ============================================================================

void sema_func_decl(State& s, Scope& parent_scope, ast::FuncDecl& decl) {
    auto scope = parent_scope.make_child(decl.name_loc);
    scope.current_decl = &decl;

    for (auto& p : decl.params) scope.define(p.sym);

    if (decl.body) {
        sema_stmt(s, scope, decl.body);
    } else {
        // FIXME: add @extern support
        s.er.report_error(decl.loc, "missing function body, missing @extern?");
    }
}

// ----------------------------------------------------------------------------

void sema_decl(State& s, Scope& scope, ast::Decl* decl) {
    if (!decl) return;

    switch (decl->kind) {
        case ast::DeclKind::Err: break;

        case ast::DeclKind::Import:
            PANIC("SEMA: imports not implemented", *decl);

        case ast::DeclKind::Func:
            sema_func_decl(s, scope, decl->as_func());
            break;

        case ast::DeclKind::Var:
            // everything already done in sema_decl_header
            break;

        case ast::DeclKind::Def:
            // everything already done in sema_decl_header
            break;

        case ast::DeclKind::MultiVar:
        case ast::DeclKind::MultiDef:
            PANIC("SEMA: var and def not implemented", *decl);
    }
}

// ----------------------------------------------------------------------------

void define_builtin_types(ty::TypeStore& /* ts */, Scope& builtin_scope) {
    auto s8_type = ty::make_int(1, true);
    auto u8_type = ty::make_int(1, false);
    auto s16_type = ty::make_int(2, true);
    auto u16_type = ty::make_int(2, false);
    auto s32_type = ty::make_int(4, true);
    auto u32_type = ty::make_int(4, false);
    auto s64_type = ty::make_int(8, true);
    auto u64_type = ty::make_int(8, false);

    auto mkty = [](ty::Type t) {
        return Value{.type = ty::make_type(), .as = {.type = t}};
    };

    builtin_scope.define("s8", {}, mkty(s8_type), false, true);
    builtin_scope.define("u8", {}, mkty(u8_type), false, true);
    builtin_scope.define("s16", {}, mkty(s16_type), false, true);
    builtin_scope.define("u16", {}, mkty(u16_type), false, true);
    builtin_scope.define("s32", {}, mkty(s32_type), false, true);
    builtin_scope.define("u32", {}, mkty(u32_type), false, true);
    builtin_scope.define("s64", {}, mkty(s64_type), false, true);
    builtin_scope.define("u64", {}, mkty(u64_type), false, true);
}

void perform_sema(ErrorReporter& er, ast::FlatModule const& module,
                  Options const& opts) {
    ty::TypeStore type_store;
    auto          s = State{.er = er, .ts = type_store, .opts = opts};

    auto symbol_store = SymbolStore{};
    auto builtin_scope = Scope{.loc = {},
                               .symbols = {},
                               .current_decl = nullptr,
                               .symbol_store = &symbol_store};

    define_builtin_types(s.ts, builtin_scope);

    auto scope = builtin_scope.make_child({});

    // do sema on all of the things at global scope
    for (auto const& decl : module.declarations) {
        sema_decl_header(s, scope, decl);
    }

    // do inner analisys
    for (auto const& decl : module.declarations) {
        sema_decl(s, scope, decl);
    }

    fmt::println("{}", module);
}

}  // namespace yal::sema
