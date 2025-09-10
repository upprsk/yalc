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

    Void,
    Type,

    Int,
    ComptimeInt,

    Ptr,
    MultiPtr,
    Slice,

    Func,
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
};

struct TypeInt {
    uint8_t byte_size;
    bool    is_signed;
};

struct TypePtr;
struct TypeFunc;
union TypeAs {
    TypeInt   integer;
    TypePtr*  ptr;
    TypeFunc* func;
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

    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == TypeKind::Func;
    }
};

struct TypePtr {
    Type inner;
};

struct TypeFunc {
    std::span<Type> params;
    std::span<Type> rets;
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

void to_json(nlohmann::json& j, TypeKind const& n);
void to_json(nlohmann::json& j, TypeFlags const& n);
void to_json(nlohmann::json& j, Type const& n);

void to_repr(fmt::format_context& ctx, Type const* type);
void to_repr(fmt::format_context& ctx, Type const& type);

}  // namespace yal::ty

define_formatter_from_string_view(yal::ty::TypeKind);
define_formatter_from_string_view(yal::ty::TypeFlags);
define_formatter_from_string_view(yal::ty::Type);
