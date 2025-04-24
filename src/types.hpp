#pragma once

#include <ranges>
#include <vector>

#include "arena.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "nlohmann/json_fwd.hpp"

namespace yal::types {
using json = nlohmann::json;

enum class TypeKind {
    Err,

    /// The type of types. As types can be used as types, we need a type for
    /// them.
    Type,

    /// Absence of a value.
    Void,

    /// Sized integers.
    Uint64,
    Int64,
    Uint32,
    Int32,
    Uint16,
    Int16,
    Uint8,
    Int8,

    /// Integers with platform dependent size. u64 on 64bit systems an u32 on
    /// 32bit systems.
    Usize,
    Isize,

    /// Boolean type. Represented as a byte.
    Bool,

    /// Floating point types
    Float32,
    Float64,

    /// Pointers
    Ptr,
    PtrConst,

    /// Multi pointers
    MultiPtr,
    MultiPtrConst,

    /// The type of immutable strings. Mutable strings should use slices of
    /// bytes.
    StrView,

    /// Nil type, will coerce to any optional type.
    Nil,

    // Function type.
    //
    // The `inner` field contains:
    //
    // 1. `Pack` with parameter types.
    // 2. `Pack` with return types.
    Func,
    FuncWithVarArgs,

    Pack,
};

struct Type {
    [[nodiscard]] constexpr auto is_integral() const -> bool {
        switch (kind) {
            case TypeKind::Uint64:
            case TypeKind::Int64:
            case TypeKind::Uint32:
            case TypeKind::Int32:
            case TypeKind::Uint16:
            case TypeKind::Int16:
            case TypeKind::Uint8:
            case TypeKind::Int8:
            case TypeKind::Usize:
            case TypeKind::Isize: return true;
            default: return false;
        }
    }

    [[nodiscard]] constexpr auto is_float() const -> bool {
        switch (kind) {
            case TypeKind::Float32:
            case TypeKind::Float64: return true;
            default: return false;
        }
    }

    // assumes that the type is an integer
    [[nodiscard]] constexpr auto is_signed() const -> bool {
        switch (kind) {
            case TypeKind::Int64:
            case TypeKind::Int32:
            case TypeKind::Int16:
            case TypeKind::Int8:
            case TypeKind::Isize: return true;
            default: return false;
        }
    }

    // assumes that the type is an integer
    [[nodiscard]] constexpr auto is_unsigned() const -> bool {
        switch (kind) {
            case TypeKind::Uint64:
            case TypeKind::Uint32:
            case TypeKind::Uint16:
            case TypeKind::Uint8:
            case TypeKind::Usize: return true;
            default: return false;
        }
    }

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == TypeKind::Err;
    }

    [[nodiscard]] constexpr auto is_type() const -> bool {
        return kind == TypeKind::Type;
    }

    [[nodiscard]] constexpr auto is_void() const -> bool {
        return kind == TypeKind::Void ||
               (kind == TypeKind::Pack && inner.size() == 1 &&
                inner[0]->is_void());
    }

    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == TypeKind::Func || kind == TypeKind::FuncWithVarArgs;
    }

    [[nodiscard]] constexpr auto is_bool() const -> bool {
        return kind == TypeKind::Bool;
    }

    [[nodiscard]] constexpr auto is_pack() const -> bool {
        return kind == TypeKind::Pack;
    }

    [[nodiscard]] constexpr auto is_strview() const -> bool {
        return kind == TypeKind::StrView;
    }

    struct Func {
        Type* params;
        Type* ret;
        bool  is_var_args;

        [[nodiscard]] constexpr auto get_params() const -> std::span<Type*> {
            return params->inner;
        }

        [[nodiscard]] constexpr auto get_ret() const -> std::span<Type*> {
            return ret->inner;
        }
    };

    [[nodiscard]] auto as_func() const -> Func;

    // ========================================================================

    [[nodiscard]] constexpr auto size() const -> size_t {
        switch (kind) {
            case TypeKind::Err:
            case TypeKind::Type:
            case TypeKind::Void: return 0;
            case TypeKind::Uint64:
            case TypeKind::Int64: return sizeof(uint64_t);
            case TypeKind::Uint32:
            case TypeKind::Int32: return sizeof(uint32_t);
            case TypeKind::Uint16:
            case TypeKind::Int16: return sizeof(uint16_t);
            case TypeKind::Uint8:
            case TypeKind::Int8: return sizeof(uint8_t);
            case TypeKind::Usize:
            case TypeKind::Isize: return sizeof(size_t);

            case TypeKind::Bool: return sizeof(bool);

            case TypeKind::Float32: return sizeof(float);
            case TypeKind::Float64: return sizeof(double);

            // made of 2 pointers, so double the size of a pointer
            case TypeKind::StrView: return sizeof(uintptr_t) * 2; break;

            case TypeKind::Ptr:
            case TypeKind::PtrConst:
            case TypeKind::MultiPtr:
            case TypeKind::MultiPtrConst: return sizeof(uintptr_t); break;

            case TypeKind::Nil:
            case TypeKind::Func:
            case TypeKind::FuncWithVarArgs: return 0; break;

            // this can't be instantiated, so it should not have size
            case TypeKind::Pack: return 0;
        }

        return 0;
    }

    // ========================================================================

    constexpr auto operator==(Type const& other) const -> bool {
        namespace r = std::ranges;
        namespace rv = std::ranges::views;

        auto toref = [](Type* o) -> Type const& { return *o; };
        return kind == other.kind &&
               r::equal(rv::transform(inner, toref),
                        rv::transform(other.inner, toref));
    }

    TypeKind         kind;
    std::span<Type*> inner;
};

struct TypeStore {
    struct TypeItem {
        TypeItem* next;
        Type      type;
    };

    struct Builtin {
        Type* error;
        Type* type;
        Type* _void;

        Type* uint64;
        Type* int64;
        Type* uint32;
        Type* int32;
        Type* uint16;
        Type* int16;
        Type* uint8;
        Type* int8;

        Type* usize;
        Type* isize;

        Type* _bool;

        Type* f32;
        Type* f64;

        Type* strview;
        Type* nil;
    };

public:
    // https://www.studyplan.dev/pro-cpp/iterator-concepts
    struct Iterator {
        using iterator_category = std::forward_iterator_tag;
        using value_type = Type;
        using element_type = value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using difference_type = std::ptrdiff_t;

        constexpr Iterator() = default;
        constexpr Iterator(TypeItem* it) : item{it} {}

        constexpr auto operator*() const -> reference { return item->type; }
        constexpr auto operator->() const -> pointer { return &operator*(); }

        constexpr auto operator++() -> Iterator& {
            item = item->next;
            return *this;
        }

        constexpr auto operator++(int) -> Iterator {
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        constexpr auto operator==(Iterator const& b) const -> bool {
            return item == b.item;
        }

        TypeItem* item{};
    };

    TypeStore() { add_builtins(); }

    [[nodiscard]] auto get_error() const -> Type* { return builtin.error; }
    [[nodiscard]] auto get_type() const -> Type* { return builtin.type; }
    [[nodiscard]] auto get_void() const -> Type* { return builtin._void; }

    [[nodiscard]] auto get_i64() const -> Type* { return builtin.int64; }
    [[nodiscard]] auto get_u64() const -> Type* { return builtin.uint64; }
    [[nodiscard]] auto get_i32() const -> Type* { return builtin.int32; }
    [[nodiscard]] auto get_u32() const -> Type* { return builtin.uint32; }
    [[nodiscard]] auto get_i16() const -> Type* { return builtin.int16; }
    [[nodiscard]] auto get_u16() const -> Type* { return builtin.uint16; }
    [[nodiscard]] auto get_i8() const -> Type* { return builtin.int8; }
    [[nodiscard]] auto get_u8() const -> Type* { return builtin.uint8; }

    [[nodiscard]] auto get_f32() const -> Type* { return builtin.f32; }
    [[nodiscard]] auto get_f64() const -> Type* { return builtin.f64; }

    [[nodiscard]] auto get_usize() const -> Type* { return builtin.usize; }
    [[nodiscard]] auto get_isize() const -> Type* { return builtin.isize; }

    [[nodiscard]] auto get_bool() const -> Type* { return builtin._bool; }

    [[nodiscard]] auto get_strview() const -> Type* { return builtin.strview; }
    [[nodiscard]] auto get_nil() const -> Type* { return builtin.nil; }

    [[nodiscard]] auto new_ptr(Type* inner, bool is_const) -> Type* {
        return new_type(is_const ? TypeKind::PtrConst : TypeKind::Ptr,
                        std::array{inner});
    }

    [[nodiscard]] auto new_mptr(Type* inner, bool is_const) -> Type* {
        return new_type(is_const ? TypeKind::MultiPtrConst : TypeKind::MultiPtr,
                        std::array{inner});
    }

    [[nodiscard]] auto new_pack(std::span<Type* const> inner) -> Type* {
        return new_type(TypeKind::Pack, inner);
    }

    [[nodiscard]] auto new_func(Type* params, Type* ret, bool has_var_args)
        -> Type* {
        return new_type(
            has_var_args ? TypeKind::FuncWithVarArgs : TypeKind::Func,
            std::array{params, ret});
    }

    [[nodiscard]] auto new_type(TypeKind kind, std::span<Type* const> inner)
        -> Type* {
        auto ty = types.create<TypeItem>(
            head, Type{.kind = kind, .inner = new_array(inner)});
        head = ty;

        return &ty->type;
    }

    [[nodiscard]] auto new_array(std::span<Type* const> arr)
        -> std::span<Type*> {
        if (arr.empty()) return {};
        return types.alloc<Type*>(arr);
    }

    // ========================================================================

    [[nodiscard]] auto coerce(Type* dst, Type* src) -> Type*;

    // ========================================================================

    [[nodiscard]] auto begin() const -> Iterator { return {head}; }
    [[nodiscard]] auto end() const -> Iterator { return {}; }

private:
    void add_builtins() {
        builtin.error = new_type(TypeKind::Err, {});
        builtin.type = new_type(TypeKind::Type, {});
        builtin._void = new_type(TypeKind::Void, {});

        builtin.uint64 = new_type(TypeKind::Uint64, {});
        builtin.int64 = new_type(TypeKind::Int64, {});
        builtin.uint32 = new_type(TypeKind::Uint32, {});
        builtin.int32 = new_type(TypeKind::Int32, {});
        builtin.uint16 = new_type(TypeKind::Uint16, {});
        builtin.int16 = new_type(TypeKind::Int16, {});
        builtin.uint8 = new_type(TypeKind::Uint8, {});
        builtin.int8 = new_type(TypeKind::Int8, {});

        builtin.usize = new_type(TypeKind::Usize, {});
        builtin.isize = new_type(TypeKind::Isize, {});

        builtin._bool = new_type(TypeKind::Bool, {});

        builtin.f32 = new_type(TypeKind::Float32, {});
        builtin.f64 = new_type(TypeKind::Float64, {});

        builtin.strview = new_type(TypeKind::StrView, {});
        builtin.nil = new_type(TypeKind::Nil, {});
    }

private:
    TypeItem* head{};

    mem::Arena types;
    mem::Arena arrays;
    Builtin    builtin;
};

static_assert(std::forward_iterator<TypeStore::Iterator>);
static_assert(std::ranges::forward_range<TypeStore>);

constexpr auto format_as(TypeKind kind) {
    std::string_view name;

    switch (kind) {
        case TypeKind::Err: name = "Err"; break;
        case TypeKind::Type: name = "Type"; break;
        case TypeKind::Void: name = "Void"; break;
        case TypeKind::Uint64: name = "Uint64"; break;
        case TypeKind::Int64: name = "Int64"; break;
        case TypeKind::Uint32: name = "Uint32"; break;
        case TypeKind::Int32: name = "Int32"; break;
        case TypeKind::Uint16: name = "Uint16"; break;
        case TypeKind::Int16: name = "Int16"; break;
        case TypeKind::Uint8: name = "Uint8"; break;
        case TypeKind::Int8: name = "Int8"; break;
        case TypeKind::Usize: name = "Usize"; break;
        case TypeKind::Isize: name = "Isize"; break;
        case TypeKind::Bool: name = "Bool"; break;
        case TypeKind::Float32: name = "f32"; break;
        case TypeKind::Float64: name = "f64"; break;
        case TypeKind::StrView: name = "string_view"; break;
        case TypeKind::Ptr: name = "Ptr"; break;
        case TypeKind::PtrConst: name = "PtrConst"; break;
        case TypeKind::MultiPtr: name = "MultiPtr"; break;
        case TypeKind::MultiPtrConst: name = "MultiPtrConst"; break;
        case TypeKind::Nil: name = "Nil"; break;
        case TypeKind::Func: name = "Func"; break;
        case TypeKind::FuncWithVarArgs: name = "FuncWithVarArgs"; break;
        case TypeKind::Pack: name = "(pack)"; break;
    }

    return name;
}

void to_json(json& j, Type const& t);
void to_json(json& j, TypeStore const& ts);

}  // namespace yal::types

template <>
struct fmt::formatter<yal::types::Type> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::types::Type ty, format_context& ctx) const
        -> format_context::iterator;
};
