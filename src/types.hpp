#pragma once

#include <fmt/format.h>

#include <cstdint>
#include <nlohmann/json_fwd.hpp>
#include <span>

#include "arena.hpp"
#include "macros.hpp"

namespace yal {
struct Symbol;
}

namespace yal::ty {

enum struct TypeKind : uint8_t {
    Err,
    PendingCast,

    Void,
    Type,

    Int,
    ComptimeInt,

    Ptr,
    MultiPtr,
    Slice,

    Func,
    Tuple,  // just for function returns!
};

struct TypeFlags {
    enum Flag : uint8_t {
        None = 0,
        Const = 1 << 0,
    };

    Flag value = None;

    [[nodiscard]] constexpr auto is_const() const -> bool {
        return value & Const;
    }

    friend constexpr auto operator|(TypeFlags lhs, TypeFlags rhs) -> TypeFlags {
        return {static_cast<Flag>(lhs.value | rhs.value)};
    }

    friend constexpr auto operator|(TypeFlags lhs, Flag rhs) -> TypeFlags {
        return {static_cast<Flag>(lhs.value | rhs)};
    }

    friend constexpr auto operator|=(TypeFlags& lhs, Flag rhs) -> TypeFlags {
        lhs = lhs | rhs;
        return lhs;
    }
};

struct TypeInt {
    uint8_t byte_size;
    bool    is_signed;
};

struct TypePtr;
struct TypeFunc;
struct TypeTuple;
union TypeAs {
    TypeInt    integer;
    TypePtr*   ptr;
    TypeFunc*  func;
    TypeTuple* tuple;
};

struct Type {
    TypeKind  kind = TypeKind::Err;
    TypeFlags flags = {};
    TypeAs    as;

    Symbol* sym;

    [[nodiscard]] constexpr auto is_valid() const -> bool { return !is_err(); }

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == TypeKind::Err;
    }

    [[nodiscard]] constexpr auto is_void() const -> bool {
        return kind == TypeKind::Void;
    }

    [[nodiscard]] constexpr auto is_type() const -> bool {
        return kind == TypeKind::Type;
    }

    [[nodiscard]] constexpr auto is_int() const -> bool {
        return kind == TypeKind::Int;
    }

    [[nodiscard]] constexpr auto is_comptime_int() const -> bool {
        return kind == TypeKind::ComptimeInt;
    }

    [[nodiscard]] constexpr auto is_ptr() const -> bool {
        return kind == TypeKind::Ptr;
    }

    [[nodiscard]] constexpr auto is_multi_ptr() const -> bool {
        return kind == TypeKind::MultiPtr;
    }

    [[nodiscard]] constexpr auto is_slice() const -> bool {
        return kind == TypeKind::Slice;
    }

    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == TypeKind::Func;
    }

    [[nodiscard]] constexpr auto is_tuple() const -> bool {
        return kind == TypeKind::Tuple;
    }
};

struct TypePtr {
    Type inner;
};

struct TypeFunc {
    std::span<Type> params;
    std::span<Type> rets;
};

struct TypeTuple {
    std::span<Type> items;
};

struct TypeStore {
    mem::Arena arena;

    auto type_func(std::span<ty::Type const> params,
                   std::span<ty::Type const> rets, Symbol* sym = nullptr)
        -> Type {
        auto func = arena.create<TypeFunc>(arena.alloc<Type>(params),
                                           arena.alloc<Type>(rets));
        return {.kind = TypeKind::Func, .as = {.func = func}, .sym = sym};
    }

    auto type_tuple(std::span<ty::Type const> items, Symbol* sym = nullptr)
        -> Type {
        auto tuple = arena.create<TypeTuple>(arena.alloc<Type>(items));
        return {.kind = TypeKind::Tuple, .as = {.tuple = tuple}, .sym = sym};
    }

    auto type_ptr(ty::Type inner, bool is_const, Symbol* sym = nullptr)
        -> Type {
        return type_ptr_like(TypeKind::Ptr, inner, is_const, sym);
    }

    auto type_multi_ptr(ty::Type inner, bool is_const, Symbol* sym = nullptr)
        -> Type {
        return type_ptr_like(TypeKind::MultiPtr, inner, is_const, sym);
    }

    auto type_slice(ty::Type inner, bool is_const, Symbol* sym = nullptr)
        -> Type {
        return type_ptr_like(TypeKind::Slice, inner, is_const, sym);
    }

    auto type_ptr_like(TypeKind kind, ty::Type inner, bool is_const,
                       Symbol* sym = nullptr) -> Type {
        auto ptr = arena.create<TypePtr>(inner);
        auto flags = TypeFlags{};
        if (is_const) flags |= TypeFlags::Const;

        return {.kind = kind, .flags = flags, .as = {.ptr = ptr}, .sym = sym};
    }
};

constexpr auto make_void(Symbol* sym = nullptr) -> Type {
    return {.kind = TypeKind::Void, .as = {}, .sym = sym};
}

constexpr auto make_type(Symbol* sym = nullptr) -> Type {
    return {.kind = TypeKind::Type, .as = {}, .sym = sym};
}

constexpr auto make_int(uint8_t byte_size, bool is_signed,
                        Symbol* sym = nullptr) -> Type {
    return {.kind = TypeKind::Int,
            .as = {.integer = {.byte_size = byte_size, .is_signed = is_signed}},
            .sym = sym};
}

constexpr auto make_comptime_int(Symbol* sym = nullptr) -> Type {
    return {.kind = TypeKind::ComptimeInt, .as = {}, .sym = sym};
}

constexpr auto make_ptr(ty::TypePtr* ptr, bool is_const, Symbol* sym = nullptr)
    -> Type {
    return {.kind = TypeKind::Ptr,
            .flags = {.value = is_const ? TypeFlags::Const : TypeFlags::None},
            .as = {.ptr = ptr},
            .sym = sym};
}

void to_json(nlohmann::json& j, TypeKind const& n);
void to_json(nlohmann::json& j, TypeFlags const& n);
void to_json(nlohmann::json& j, Type const& n);

void to_repr(fmt::format_context& ctx, Type const* type);
void to_repr(fmt::format_context& ctx, Type const& type);

}  // namespace yal::ty

define_formatter_from_string_view(yal::ty::TypeKind);
define_formatter_from_string_view(yal::ty::TypeFlags);
define_formatter_from_string_view(yal::ty::Type);
