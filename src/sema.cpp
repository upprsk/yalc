#include "sema.hpp"

#include <fmt/ranges.h>

#include <algorithm>
#include <ranges>
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

auto types_equal(ty::Type lhs, ty::Type rhs) -> bool {
    if (lhs.kind != rhs.kind) return false;
    if (lhs.is_int())
        return lhs.as.integer.byte_size == rhs.as.integer.byte_size &&
               lhs.as.integer.is_signed == rhs.as.integer.is_signed;
    if (lhs.is_ptr() || lhs.is_multi_ptr() || lhs.is_slice())
        return lhs.flags.is_const() == rhs.flags.is_const() &&
               types_equal(lhs.as.ptr->inner, rhs.as.ptr->inner);

    // FIXME: add other types

    return false;
}

auto coerce_type(State& s, ty::Type source, ty::Type target,
                 CoercionOpts const& opts) -> CoercionResult {
    // comptime_int -> int OK
    // TODO: comptime_int -> float OK

    // don't deal with errors here, to avoid too many extra error messages when
    // something goes wrong
    if (source.is_err() || target.is_err()) return {.type = target};

    if (source.is_pending_cast()) {
        return {.type = target, .source_requires_fixup = true};
    }

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

    // both are comptime_int
    if (target.is_comptime_int()) return {.type = target};

    // both are bool
    if (target.is_bool()) return {.type = target};

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

    if (target.is_ptr()) {
        auto const& source_ptr = source.as.ptr;
        auto const& target_ptr = target.as.ptr;

        if (!types_equal(source_ptr->inner, target_ptr->inner)) {
            s.er.report_error(opts.loc, "can not coerce from {} to {}", source,
                              target);
            s.er.report_note(opts.source_loc, "this has type {}", source);
            s.er.report_note(opts.target_loc, "but expected type {} from here",
                             target);

            return {.type = target};
        }

        // var -> var: OK
        // var -> const: OK
        // const -> var: bad
        // const -> const: OK

        if (source.flags.is_const() && !target.flags.is_const()) {
            s.er.report_error(opts.loc, "can not coerce from {} to {}", source,
                              target);
            s.er.report_note(opts.source_loc, "this has type {}", source);
            s.er.report_note(opts.target_loc, "but expected type {} from here",
                             target);

            return {.type = target};
        }

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

    if (lhs.is_pending_cast()) {
        return {.type = rhs, .lhs_requires_fixup = true};
    }

    if (rhs.is_pending_cast()) {
        return {.type = lhs, .rhs_requires_fixup = true};
    }

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

    // both are comptime_int
    if (lhs.is_comptime_int()) return {.type = lhs};

    // both are bool
    if (lhs.is_bool()) return {.type = lhs};

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

    if (lhs.is_ptr()) {
        auto const& source_ptr = rhs.as.ptr;
        auto const& target_ptr = lhs.as.ptr;

        if (!types_equal(source_ptr->inner, target_ptr->inner)) {
            s.er.report_error(opts.loc, "incompatible types: {} and {}", rhs,
                              lhs);
            s.er.report_note(opts.lhs_loc, "this has type {}", rhs);
            s.er.report_note(opts.rhs_loc, "this has type {}", lhs);

            return {.type = lhs};
        }

        // var -> var: OK
        // var -> const: OK
        // const -> var: bad
        // const -> const: OK

        if (rhs.flags.is_const() && !lhs.flags.is_const()) {
            s.er.report_error(opts.loc, "incompatible types: {} and {}", rhs,
                              lhs);
            s.er.report_note(opts.lhs_loc, "this has type {}", rhs);
            s.er.report_note(opts.rhs_loc, "this has type {}", lhs);

            return {.type = lhs};
        }

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
    if (source.is_err() || target.is_err() || target.is_pending_cast())
        return {.type = target};

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

    // both are pointers, may be able to cast
    if (source.is_ptr() && target.is_ptr()) {
        // source is const but target is, not allowed
        if (source.flags.is_const() && !target.flags.is_const()) {
            s.er.report_error(
                opts.loc,
                "can not cast away constness of pointer. From {} to {}", source,
                target);
        }

        // NOTE: do we want some variant of the static_cast vs reinterpret_cast
        // thing? For now our cast is C-style, does everything.

        auto is_redundant_cast =
            source.sym == target.sym &&
            source.flags.is_const() == target.flags.is_const() &&
            types_equal(source.as.ptr->inner, target.as.ptr->inner);

        if (is_redundant_cast) {
            s.er.report_warn(opts.loc, "redundant cast from {} to {}", source,
                             target);
        }

        return {.type = target, .redundant_cast = is_redundant_cast};
    }

    if (source.is_bool() && target.is_bool()) {
        auto is_redundant_cast = source.sym == target.sym;

        if (is_redundant_cast) {
            s.er.report_warn(opts.loc, "redundant cast from {} to {}", source,
                             target);
        }

        return {.type = target, .redundant_cast = is_redundant_cast};
    }

    if (source.is_bool() && target.is_int()) {
        return {.type = target};
    }

    if (source.is_int() && target.is_bool()) {
        // This will do the C thing where 0 is false and everything else is true
        return {.type = target};
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

        case ast::ExprKind::Not:
        case ast::ExprKind::Equal:
        case ast::ExprKind::NotEqual:
        case ast::ExprKind::Less:
        case ast::ExprKind::Greater:
        case ast::ExprKind::LessEqual:
        case ast::ExprKind::GreaterEqual:
            return type.is_int() || type.is_comptime_int() || type.is_bool();

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
        case ast::ExprKind::Not:
        case ast::ExprKind::Equal:
        case ast::ExprKind::NotEqual:
        case ast::ExprKind::Less:
        case ast::ExprKind::Greater:
        case ast::ExprKind::LessEqual:
        case ast::ExprKind::GreaterEqual:
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

auto cast_expr_apply(State& s, ast::CastExpr& expr, ty::Type type) -> ty::Type;

void fixup_types_in_expr(State& s, ast::Expr* expr, ty::Type target_type) {
    if (s.opts.verbose_coercions) {
        s.er.report_debug(expr->loc, "fixup: {} -> {}", expr->type,
                          target_type);
    }

    // in case of an error, just abort
    if (expr->type.is_err() || target_type.is_err() ||
        target_type.is_comptime_int())
        return;

    ASSERT(expr->type.is_comptime_int() || expr->type.is_pending_cast(),
           expr->type);
    ASSERT(target_type.is_int(), target_type);

    switch (expr->kind) {
        case ast::ExprKind::Err: break;

        case ast::ExprKind::Neg:
            fixup_types_in_expr(s, expr->as_arith().lhs, target_type);

            // FIXME: this does not look right, should probably do a full
            // re-check of the expr
            expr->type = target_type;
            break;

        case ast::ExprKind::Add:
        case ast::ExprKind::Sub:
        case ast::ExprKind::Mul:
        case ast::ExprKind::Div:
        case ast::ExprKind::Mod:
        case ast::ExprKind::Not:
        case ast::ExprKind::Equal:
        case ast::ExprKind::NotEqual:
        case ast::ExprKind::Less:
        case ast::ExprKind::Greater:
        case ast::ExprKind::LessEqual:
        case ast::ExprKind::GreaterEqual:
            fixup_types_in_expr(s, expr->as_arith().lhs, target_type);
            fixup_types_in_expr(s, expr->as_arith().rhs, target_type);

            // FIXME: this does not look right, should probably do a full
            // re-check of the expr
            expr->type = target_type;
            break;

        case ast::ExprKind::Deref:
        case ast::ExprKind::Ref: break;

        case ast::ExprKind::Cast: {
            auto& cast = expr->as_cast();
            ASSERT(cast.type_is_infer());

            expr->type = cast_expr_apply(s, cast, target_type);
        } break;

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

void sema_stmt(State& s, Scope& scope, ast::Stmt* stmt);
void sema_expr(State& s, Scope& scope, ast::Expr* expr, ty::Type expected_type);

// ----------------------------------------------------------------------------

struct LvalueInfo {
    bool is_lvalue = false;
    bool is_const = false;
};

auto calc_rvalue_info(ast::Expr* expr) -> LvalueInfo {
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
        case ast::ExprKind::Not:
        case ast::ExprKind::Equal:
        case ast::ExprKind::NotEqual:
        case ast::ExprKind::Less:
        case ast::ExprKind::Greater:
        case ast::ExprKind::LessEqual:
        case ast::ExprKind::GreaterEqual:
            // not rvalue
            return {};

        case ast::ExprKind::Field:
            // is an rvalue depending on obj
            // FIXME: check when this is ok, for now it is not an rvalue
            return {};

        case ast::ExprKind::Call:
            // not an rvalue
            return {};

        case ast::ExprKind::Deref: {
            auto& deref = expr->as_arith();
            auto  info = calc_rvalue_info(deref.lhs);

            auto is_const =
                deref.lhs ? deref.lhs->type.flags.is_const() : false;
            return {.is_lvalue = info.is_lvalue, .is_const = is_const};
        }

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

            // in case there is no sym, return that it is an lvalue to reduce
            // bogus error messages
            if (id.sym == nullptr) return {.is_lvalue = true};

            // this is an rvalue, and may or may not be a constant depending on
            // the symbol
            return {.is_lvalue = true, .is_const = id.sym->is_const};
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
            fixup_types_in_expr(s, expr.lhs, result.type);
        }
        if (result.rhs_requires_fixup) {
            fixup_types_in_expr(s, expr.rhs, result.type);
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

    else if (expr.lhs) {
        expr.type = expr.lhs->type;
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

void sema_expr_comp(State& s, Scope& scope, ast::ArithExpr& expr,
                    ty::Type expected_type) {
    sema_expr(s, scope, expr.lhs, expected_type);
    sema_expr(s, scope, expr.rhs, expected_type);

    expr.type = ty::make_bool();

    if (expr.lhs && expr.rhs) {
        auto result = unify_types(s, expr.lhs->type, expr.rhs->type,
                                  {.loc = expr.loc,
                                   .lhs_loc = expr.lhs->loc,
                                   .rhs_loc = expr.rhs->loc,
                                   .expected_type = expected_type});
        if (result.lhs_requires_fixup) {
            fixup_types_in_expr(s, expr.lhs, result.type);
        }
        if (result.rhs_requires_fixup) {
            fixup_types_in_expr(s, expr.rhs, result.type);
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

        // check that the operator is supported by the type
        if (result.type.is_valid()) {
            if (!type_supports_operator(result.type, expr.kind)) {
                s.er.report_error(expr.loc,
                                  "operator {} can not be used with type {}",
                                  operator_to_sym(expr.kind), result.type);
            }
        }
    }

    else if (expr.lhs) {
        // check that the operator is supported by the type
        if (expr.lhs->type.is_valid()) {
            if (!type_supports_operator(expr.lhs->type, expr.kind)) {
                s.er.report_error(expr.loc,
                                  "operator {} can not be used with type {}",
                                  operator_to_sym(expr.kind), expr.lhs->type);
            }
        }
    }
}

auto cast_expr_apply(State& s, ast::CastExpr& expr, ty::Type type) -> ty::Type {
    auto result = cast_type(s, expr.child->type, type,
                            {.loc = expr.loc,
                             .source_loc = expr.child->loc,
                             .target_loc = expr.type_expr->loc});

    if (result.source_requires_fixup) {
        fixup_types_in_expr(s, expr.child, result.type);
    }

    // FIXME: handle when an implicit conversion happens, as that
    // requires an additonal AST node.

    if (s.opts.verbose_coercions) {
        s.er.report_debug(expr.loc,
                          "{} -> {} result={} (source_requires_fixup={})",
                          expr.child->type, type, result.type,
                          result.source_requires_fixup ? "yes" : "no");
    }

    return result.type;
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
        if (expected_type.is_valid()) {
            type = expected_type;
        } else {
            type = ty::make_pending_cast();
        }
    }

    sema_expr(s, scope, expr.child, type);

    // now we need to cast it
    if (expr.type_expr && expr.child) {
        type = cast_expr_apply(s, expr, type);
    }

    expr.type = type;
}

void sema_expr_call(State& s, Scope& scope, ast::CallExpr& expr,
                    ty::Type expected_type) {
    sema_expr(s, scope, expr.callee, {});

    std::span<ty::Type const> expected_args;
    std::span<ty::Type const> expected_rets;

    auto is_err = false;

    if (expr.callee->type.is_func()) {
        expected_args = expr.callee->type.as.func->params;
        expected_rets = expr.callee->type.as.func->rets;

        if (expr.args.size() != expected_args.size()) {
            s.er.report_error(expr.args_loc,
                              "incorrect number of arguments for function, "
                              "expected {} but got {}",
                              expected_args.size(), expr.args.size());
        }
    } else {
        if (!expr.callee->type.is_err()) {
            s.er.report_error(expr.callee->loc,
                              "can not call value of non-function type {}",
                              expr.callee->type);
        }

        is_err = true;
    }

    for (auto const& [idx, arg] : std::views::enumerate(expr.args)) {
        auto expected_type = ty::Type{};
        if (static_cast<size_t>(idx) < expected_args.size())
            expected_type = expected_args[idx];

        sema_expr(s, scope, arg, expected_type);

        auto result = coerce_type(s, arg->type, expected_type,
                                  {.loc = arg->loc,
                                   .source_loc = arg->loc,
                                   .target_loc = expr.callee->loc});
        if (result.source_requires_fixup) {
            fixup_types_in_expr(s, arg, result.type);
        }

        // FIXME: handle when an implicit conversion happens, as that
        // requires an additonal AST node.

        if (s.opts.verbose_coercions) {
            s.er.report_debug(expr.loc,
                              "{} -> {} result={} (source_requires_fixup={})",
                              arg->type, expected_type, result.type,
                              result.source_requires_fixup ? "yes" : "no");
        }
    }

    if (expected_rets.size() == 0) {
        expr.type = ty::make_void();
    } else if (expected_rets.size() == 1) {
        expr.type = expected_rets[0];
    } else {
        expr.type = s.ts.type_tuple(expected_rets);
    }

    // in case of an error, set our return to an error as well
    if (is_err) {
        expr.type = ty::Type{};
    }
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
    if (!info.is_lvalue) {
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

        case ast::ExprKind::Not:

        case ast::ExprKind::Equal:
        case ast::ExprKind::NotEqual:
        case ast::ExprKind::Less:
        case ast::ExprKind::Greater:
        case ast::ExprKind::LessEqual:
        case ast::ExprKind::GreaterEqual:
            sema_expr_comp(s, scope, expr->as_arith(), expected_type);
            break;

        case ast::ExprKind::Cast:
            sema_expr_cast(s, scope, expr->as_cast(), expected_type);
            break;

        case ast::ExprKind::Field: PANIC("SEMA EXPR: not implemented", *expr);

        case ast::ExprKind::Call:
            sema_expr_call(s, scope, expr->as_call(), expected_type);
            break;

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
            if (id.is_id_discard()) {
                s.er.report_error(id.loc,
                                  "can not use _ (discard) as identifier");
            }

            else if (auto sym = scope.lookup(id.value)) {
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

struct MultiVarDesc {
    std::span<ast::MultiVarName const> names;
    std::span<ast::Expr* const>        type_exprs;
    std::span<ast::Expr* const>        inits;
    Location                           loc;

    bool should_fixup;
};

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
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
            fixup_types_in_expr(s, var.init, expected_type);
        }

        // in case the type is a tuple, then we called a function that returns
        // multiple values but we only support a single return.
        if (expected_type.is_tuple()) {
            DEBUG_ASSERT(expected_type.as.tuple->items.size() > 1);

            s.er.report_error(var.init->loc,
                              "initializer returns {} values of types {}, but "
                              "unpacked only 1",
                              expected_type.as.tuple->items.size(),
                              expected_type);

            expected_type = expected_type.as.tuple->items[0];
        }
    }

    else {
        auto init_type = var.init->type;

        // in case the type is a tuple, then we called a function that returns
        // multiple values but we only support a single return.
        if (init_type.is_tuple()) {
            DEBUG_ASSERT(init_type.as.tuple->items.size() > 1);

            s.er.report_error(var.init->loc,
                              "initializer returns {} values of types {}, but "
                              "unpacked only 1",
                              init_type.as.tuple->items.size(), init_type);

            init_type = init_type.as.tuple->items[0];
        }

        // we may have failed to get the type from type_expr, in such case we
        // don't do anything here
        if (expected_type.is_valid()) {
            auto result = coerce_type(s, init_type, expected_type,
                                      {.loc = var.loc,
                                       .source_loc = var.type_expr->loc,
                                       .target_loc = var.init->loc});
            if (result.source_requires_fixup) {
                fixup_types_in_expr(s, var.init, result.type);
            }

            // FIXME: handle when an implicit conversion happens, as that
            // requires an additonal AST node.

            if (s.opts.verbose_coercions) {
                s.er.report_debug(var.loc,
                                  "{} -> {} result={} (requires_a_cast={}, "
                                  "source_requires_fixup={})",
                                  init_type, expected_type, result.type,
                                  result.requires_a_cast ? "yes" : "no",
                                  result.source_requires_fixup ? "yes" : "no");
            }

            expected_type = result.type;
        }
    }

    return expected_type;
}

auto sema_some_multi_var(State& s, Scope& scope, MultiVarDesc const& var)
    -> std::vector<ty::Type> {
    for (auto const& type : var.type_exprs) {
        sema_expr(s, scope, type, ty::make_type());
    }

    auto expected_types = std::vector<ty::Type>{};
    for (auto const& type_expr : var.type_exprs) {
        auto expr = eval_expr_to_type(s, scope, type_expr);
        expected_types.push_back(expr);
    }

    for (auto const& [idx, init_expr] : std::views::enumerate(var.inits)) {
        auto expected_type = ty::Type{};
        if (static_cast<size_t>(idx) < expected_types.size())
            expected_type = expected_types[idx];

        sema_expr(s, scope, init_expr, expected_type);
    }

    // type=empty and init=empty -> error
    //     var x, y;
    //
    // type=1 and init=empty -> spread type to all
    //     var x, y: s32;
    //
    // type=names and init=empty -> match types to names
    //     var x, y: s32, u32;
    //
    // type!=names and init=empty -> invalid combination
    //     var x, y, z: s32, u32;
    //
    // type=empty and init!empty -> need to spread any function calls with
    // multiple returns and fixup
    //     var x, y = 10, 11;
    //     var x, y = call_that_returns_2_things();
    //
    // type!empty and init!empty -> need to spread any function calls with
    // multiple returns, do fixup and then coercion
    //     var x, y: s32, s32 = 10, 11;
    //     var x, y: s32, s32 = call_that_returns_2_things();

    if (expected_types.empty() && var.inits.empty()) {
        s.er.report_error(var.loc,
                          "variable declaration requires at least type or "
                          "initializer, got none");
    }

    else if (expected_types.size() == 1 && var.inits.empty()) {
        auto expected_type = expected_types[0];
        expected_types.resize(var.names.size(), expected_type);
    }

    else if (expected_types.size() == var.names.size() && var.inits.empty()) {
        // every name has a type, everything is already setup
    }

    else if (var.inits.empty()) {
        // invalid combination, can not spread and number of names and types
        // does not match
        s.er.report_error(var.loc,
                          "wrong number of types in variable declaration, "
                          "found {} identifiers but {} types",
                          var.names.size(), expected_types.size());
    }

    else if (expected_types.empty()) {
        std::vector<std::pair<ast::Expr*, bool>> init_for_expanded_type;
        for (auto const& init_expr : var.inits) {
            if (init_expr->type.is_tuple()) {
                for (auto const& ty : init_expr->type.as.tuple->items) {
                    expected_types.push_back(ty);
                    init_for_expanded_type.emplace_back(init_expr, true);
                }
            } else {
                expected_types.push_back(init_expr->type);
                init_for_expanded_type.emplace_back(init_expr, false);
            }
        }

        if (var.names.size() != expected_types.size()) {
            s.er.report_error(var.loc,
                              "wrong number of values in variable declaration, "
                              "found {} identifiers but {} values",
                              var.names.size(), expected_types.size());

            for (auto const init_expr : var.inits) {
                if (init_expr->type.is_tuple()) {
                    s.er.report_note(
                        init_expr->loc,
                        "this function returns {} values of types {}",
                        init_expr->type.as.tuple->items.size(),
                        init_expr->type);
                }
            }
        }

        for (size_t i = 0; i < expected_types.size(); ++i) {
            auto& expected_type = expected_types[i];

            // in case the type of the expression is comptime_int, then we need
            // to move it to the default integer type
            if (expected_type.is_comptime_int() && var.should_fixup) {
                expected_type = get_default_int();

                ASSERT(init_for_expanded_type[i].second == false,
                       "should never get a comptime_int from a tuple",
                       *init_for_expanded_type[i].first);
                fixup_types_in_expr(s, init_for_expanded_type[i].first,
                                    expected_type);
            }
        }
    }

    else {
        std::vector<std::pair<ty::Type, Location>> init_types;
        for (auto const& init_expr : var.inits) {
            if (init_expr->type.is_tuple()) {
                for (auto const& ty : init_expr->type.as.tuple->items) {
                    init_types.emplace_back(ty, init_expr->loc);
                }
            } else {
                init_types.emplace_back(init_expr->type, init_expr->loc);
            }
        }

        if (var.names.size() != expected_types.size()) {
            s.er.report_error(var.loc,
                              "wrong number of types in variable declaration, "
                              "found {} identifiers but {} types",
                              var.names.size(), expected_types.size());
        }

        if (expected_types.size() != init_types.size()) {
            s.er.report_error(var.loc,
                              "wrong number of values in variable declaration, "
                              "found {} types but {} values",
                              expected_types.size(), init_types.size());
        }

        auto count = std::min(expected_types.size(), init_types.size());
        for (size_t i = 0; i < count; ++i) {
            auto expected_type = expected_types[i];
            auto expected_type_expr = var.type_exprs[i];
            auto init_type = init_types[i];

            auto result = coerce_type(s, init_type.first, expected_type,
                                      {.loc = var.loc,
                                       .source_loc = init_type.second,
                                       .target_loc = expected_type_expr->loc});
            if (result.source_requires_fixup) {
                fixup_types_in_expr(s, expected_type_expr, result.type);
            }

            // FIXME: handle when an implicit conversion happens, as that
            // requires an additonal AST node.

            if (s.opts.verbose_coercions) {
                s.er.report_debug(var.loc,
                                  "{} -> {} result={} (requires_a_cast={}, "
                                  "source_requires_fixup={})",
                                  init_type, expected_type, result.type,
                                  result.requires_a_cast ? "yes" : "no",
                                  result.source_requires_fixup ? "yes" : "no");
            }

            expected_type = result.type;
        }
    }

    return expected_types;
}

// ============================================================================

void sema_attributes(State& s, Scope& /* scope */, Symbol* sym,
                     std::span<ast::DeclAttribute> attributes) {
    for (auto const& attribute : attributes) {
        if (!attribute.qualified_name.empty()) {
            s.er.report_error(attribute.loc,
                              "qualified attributes have not been implemented");
            continue;
        }

        if (attribute.name == "extern") {
            sym->is_extern = true;

            if (attribute.args.size() > 0 || attribute.kwargs.size() > 0) {
                s.er.report_bug(
                    attribute.loc,
                    "arguments for @extern have not been implemented yet");
            }
        } else {
            s.er.report_warn(attribute.loc, "unknown attribute @{}",
                             attribute.name);
        }
    }
}

// ============================================================================

void sema_func_params(State& s, Scope& scope, ast::FuncDecl& decl) {
    for (auto& p : decl.params) {
        auto param_scope = scope.make_child(p.loc);
        sema_expr(s, param_scope, p.type_expr, ty::make_type());

        if (p.type_expr) {
            p.type = eval_expr_to_type(s, param_scope, p.type_expr);
        }

        if (!p.name_is_discard()) {
            p.sym = scope.define(p.name, p.loc, {.type = p.type, .as = {}},
                                 true, true);
        }
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

    sema_attributes(s, scope, decl.sym, decl.attributes);

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

    if (!decl.name_is_discard()) {
        decl.sym = parent_scope.define(decl.name, decl.name_loc,
                                       {.type = expected_type, .as = {}}, false,
                                       false);
    }
}

void sema_def_decl_header(State& s, Scope& parent_scope, ast::VarDecl& decl) {
    if (!decl.name_is_discard()) {
        // alread define the thing, as it may be needed recursivelly
        decl.sym =
            parent_scope.define(decl.name, decl.name_loc, {}, false, true);
    }

    auto scope = parent_scope.make_child(decl.name_loc);
    scope.current_decl = &decl;

    ASSERT(decl.attributes.size() == 0, decl.name,
           "attributes have not been implemented yet");

    /* auto expected_type = */ sema_some_var(s, scope,
                                             {.type_expr = decl.type_expr,
                                              .init = decl.init,
                                              .loc = decl.loc,
                                              .should_fixup = false});

    if (decl.init == nullptr) {
        s.er.report_error(decl.loc, "constants must have an initializer");
    }

    auto value = eval_expr(s, scope, decl.init);
    // FIXME: handle coercing value to expected_type

    if (!decl.name_is_discard()) {
        decl.sym->value = value;
    }
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
            fixup_types_in_expr(s, r, result.type);
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

void sema_stmt_expr(State& s, Scope& scope, ast::ExprStmt& stmt) {
    sema_expr(s, scope, stmt.child, {});

    if (stmt.child && !stmt.child->type.is_void() &&
        !stmt.child->type.is_err()) {
        s.er.report_warn(stmt.child->loc,
                         "discard of expression result of type {}",
                         stmt.child->type);
    }
}

void sema_stmt_while(State& s, Scope& scope, ast::WhileStmt& stmt) {
    auto expected_type = ty::make_bool();
    sema_expr(s, scope, stmt.cond, expected_type);

    auto result = coerce_type(s, stmt.cond->type, expected_type,
                              {.loc = stmt.loc,
                               .source_loc = stmt.cond->loc,
                               .target_loc = stmt.loc});
    if (result.source_requires_fixup) {
        fixup_types_in_expr(s, stmt.cond, result.type);
    }

    // FIXME: handle when an implicit conversion happens, as that requires
    // an additonal AST node.

    if (s.opts.verbose_coercions) {
        s.er.report_debug(stmt.loc,
                          "{} -> {} result={} (requires_a_cast={}, "
                          "source_requires_fixup={})",
                          stmt.cond->type, expected_type, result.type,
                          result.requires_a_cast ? "yes" : "no",
                          result.source_requires_fixup ? "yes" : "no");
    }

    sema_stmt(s, scope, stmt.body);
}

void sema_stmt_if(State& s, Scope& scope, ast::IfStmt& stmt) {
    auto expected_type = ty::make_bool();
    sema_expr(s, scope, stmt.cond, expected_type);

    auto result = coerce_type(s, stmt.cond->type, expected_type,
                              {.loc = stmt.loc,
                               .source_loc = stmt.cond->loc,
                               .target_loc = stmt.loc});
    if (result.source_requires_fixup) {
        fixup_types_in_expr(s, stmt.cond, result.type);
    }

    // FIXME: handle when an implicit conversion happens, as that requires
    // an additonal AST node.

    if (s.opts.verbose_coercions) {
        s.er.report_debug(stmt.loc,
                          "{} -> {} result={} (requires_a_cast={}, "
                          "source_requires_fixup={})",
                          stmt.cond->type, expected_type, result.type,
                          result.requires_a_cast ? "yes" : "no",
                          result.source_requires_fixup ? "yes" : "no");
    }

    sema_stmt(s, scope, stmt.when_true);
    sema_stmt(s, scope, stmt.when_false);
}

void sema_stmt_var(State& s, Scope& scope, ast::VarStmt& stmt) {
    auto expected_type = sema_some_var(s, scope,
                                       {.type_expr = stmt.type_expr,
                                        .init = stmt.init,
                                        .loc = stmt.loc,
                                        .should_fixup = true});

    if (!stmt.name_is_discard()) {
        stmt.sym = scope.define(stmt.name, stmt.name_loc,
                                {.type = expected_type, .as = {}}, true, false);
    }
}

void sema_stmt_multi_var(State& s, Scope& scope, ast::MultiVarStmt& stmt) {
    auto expected_type = sema_some_multi_var(s, scope,
                                             {.names = stmt.names,
                                              .type_exprs = stmt.types,
                                              .inits = stmt.inits,
                                              .loc = stmt.loc,
                                              .should_fixup = true});

    for (size_t i = 0; i < stmt.names.size(); ++i) {
        auto& name = stmt.names[i];
        auto  type = ty::Type{};
        if (i < expected_type.size()) type = expected_type[i];

        if (!name.name_is_discard()) {
            name.sym = scope.define(name.name, name.loc,
                                    {.type = type, .as = {}}, true, false);
        }
    }
}

void sema_stmt_assign(State& s, Scope& scope, ast::AssignStmt& stmt) {
    sema_expr(s, scope, stmt.lhs, {});

    auto info = calc_rvalue_info(stmt.lhs);
    if (!info.is_lvalue) {
        auto loc = stmt.lhs ? stmt.lhs->loc : stmt.loc;
        s.er.report_error(loc, "can not assign to rvalue");
    }

    if (info.is_const) {
        auto loc = stmt.lhs ? stmt.lhs->loc : stmt.loc;
        s.er.report_error(loc, "can not assign to constant");
    }

    auto expected_type = ty::Type{};
    if (stmt.lhs) expected_type = stmt.lhs->type;

    sema_expr(s, scope, stmt.rhs, expected_type);

    if (stmt.lhs && stmt.rhs) {
        auto result = coerce_type(s, stmt.rhs->type, stmt.lhs->type,
                                  {.loc = stmt.loc,
                                   .source_loc = stmt.rhs->loc,
                                   .target_loc = stmt.lhs->loc});
        if (result.source_requires_fixup) {
            fixup_types_in_expr(s, stmt.rhs, result.type);
        }

        // FIXME: handle when an implicit conversion happens, as that requires
        // an additonal AST node.

        if (s.opts.verbose_coercions) {
            s.er.report_debug(stmt.loc,
                              "{} -> {} result={} (requires_a_cast={}, "
                              "source_requires_fixup={})",
                              stmt.lhs->type, stmt.rhs->type, result.type,
                              result.requires_a_cast ? "yes" : "no",
                              result.source_requires_fixup ? "yes" : "no");
        }
    }
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

        case ast::StmtKind::Expr:
            sema_stmt_expr(s, scope, stmt->as_expr());
            break;

        case ast::StmtKind::While:
            sema_stmt_while(s, scope, stmt->as_while());
            break;
        case ast::StmtKind::If: sema_stmt_if(s, scope, stmt->as_if()); break;

        case ast::StmtKind::Var: sema_stmt_var(s, scope, stmt->as_var()); break;

        case ast::StmtKind::Def: PANIC("SEMA: not implemented", *stmt);

        case ast::StmtKind::MultiVar:
            sema_stmt_multi_var(s, scope, stmt->as_multi_var());
            break;

        case ast::StmtKind::MultiDef: PANIC("SEMA: not implemented", *stmt);

        case ast::StmtKind::Assign:
            sema_stmt_assign(s, scope, stmt->as_assign());
            break;

        case ast::StmtKind::MultiAssign: PANIC("SEMA: not implemented", *stmt);
    }
}

// ============================================================================

void sema_func_decl(State& s, Scope& parent_scope, ast::FuncDecl& decl) {
    auto scope = parent_scope.make_child(decl.name_loc);
    scope.current_decl = &decl;

    for (auto& p : decl.params) scope.define(p.sym);

    if (decl.body) {
        if (decl.sym->is_extern) {
            s.er.report_error(
                decl.loc, "function marked with @extern can not have a body");
        }

        sema_stmt(s, scope, decl.body);
    } else {
        if (!decl.sym->is_extern) {
            s.er.report_error(decl.loc,
                              "missing function body, missing @extern?");
        }
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

void define_builtins(ty::TypeStore& /* ts */, Scope& builtin_scope) {
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

    builtin_scope.define(
        "true", {},
        {.type = ty::make_bool(),
         .as = {.boolean = {.value = true, .has_value = true}}},
        false, true);
    builtin_scope.define(
        "false", {},
        {.type = ty::make_bool(),
         .as = {.boolean = {.value = false, .has_value = true}}},
        false, true);
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

    define_builtins(s.ts, builtin_scope);

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
