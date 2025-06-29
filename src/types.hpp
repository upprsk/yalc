#pragma once

#include <cstdint>
#include <ranges>
#include <string_view>
#include <vector>

#include "arena.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "nlohmann/json_fwd.hpp"

namespace yal {
struct Decl;
}

namespace yal::types {
using json = nlohmann::json;

enum class TypeKind : uint16_t {
    Err,

    /// The type of types. As types can be used as types, we need a type for
    /// them.
    Type,

    /// Absence of a value.
    Void,

    /// Integer literals, these are untyped until a type can be inferred for the
    /// literal
    UntypedInt,

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
    RawPtr,

    /// Multi pointers
    MultiPtr,
    MultiPtrConst,

    /// Slice types
    Slice,
    SliceConst,

    /// The type of immutable strings. Mutable strings should use slices of
    /// bytes.
    StrView,

    /// An array.
    ///
    /// - `inner` contains the inner array type.
    /// - `count` contains the size of the array.
    Array,

    /// A struct.
    ///
    /// - `inner` contains a list of `StructField`.
    Struct,
    /// A struct field.
    StructField,

    /// A struct literal, the type of a struct before inference.
    ///
    /// - `inner` contains a list of either `LitField` or another type
    /// directly. `LitField` encodes a named field and anything else a
    /// positional initializer.
    Lit,
    LitField,

    /// Nil type, will coerce to any optional type.
    Nil,

    /// Function type.
    ///
    /// The `inner` field contains:
    ///
    /// 1. `Pack` with parameter types.
    /// 2. `Pack` with return types.
    Func,
    FuncWithVarArgs,

    /// Method type. Very similar to `Func`
    BoundFunc,

    /// A pack of types (kinda of a tuple) that functions return.
    Pack,

    /// A distinct type. It is only coercible to itself.
    Distinct,
};

struct Type {
    [[nodiscard]] constexpr auto is_integral() const -> bool {
        switch (kind) {
            case TypeKind::UntypedInt:
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

    [[nodiscard]] constexpr auto is_u8() const -> bool {
        return kind == TypeKind::Uint8;
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

    [[nodiscard]] constexpr auto is_always_stack() const -> bool {
        return kind == TypeKind::Struct;
    }

    [[nodiscard]] constexpr auto is_size() const -> bool {
        return kind == TypeKind::Usize || kind == TypeKind::Isize;
    }

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == TypeKind::Err;
    }

    [[nodiscard]] constexpr auto is_untyped_int() const -> bool {
        return kind == TypeKind::UntypedInt;
    }

    [[nodiscard]] constexpr auto contains_untyped() const -> bool {
        namespace r = std::ranges;
        namespace rv = std::ranges::views;
        return kind == TypeKind::UntypedInt || kind == TypeKind::Lit ||
               (kind == TypeKind::Pack
                    ? r::any_of(
                          rv::transform(inner,
                                        [](Type* inner) {
                                            return inner->contains_untyped();
                                        }),
                          [](bool v) { return v; })
                    : false);
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
        return kind == TypeKind::Func || kind == TypeKind::FuncWithVarArgs ||
               kind == TypeKind::BoundFunc;
    }

    [[nodiscard]] constexpr auto is_bool() const -> bool {
        return kind == TypeKind::Bool;
    }

    [[nodiscard]] constexpr auto is_pack() const -> bool {
        return kind == TypeKind::Pack;
    }

    [[nodiscard]] constexpr auto is_ptr() const -> bool {
        return kind == TypeKind::Ptr || kind == TypeKind::PtrConst;
    }

    [[nodiscard]] constexpr auto is_ptr_mut() const -> bool {
        return kind == TypeKind::Ptr;
    }

    [[nodiscard]] constexpr auto is_ptr_const() const -> bool {
        return kind == TypeKind::PtrConst;
    }

    [[nodiscard]] constexpr auto is_rawptr() const -> bool {
        return kind == TypeKind::RawPtr;
    }

    [[nodiscard]] constexpr auto is_mptr() const -> bool {
        return kind == TypeKind::MultiPtr || kind == TypeKind::MultiPtrConst;
    }

    [[nodiscard]] constexpr auto is_mptr_mut() const -> bool {
        return kind == TypeKind::MultiPtr;
    }

    [[nodiscard]] constexpr auto is_mptr_const() const -> bool {
        return kind == TypeKind::MultiPtrConst;
    }

    [[nodiscard]] constexpr auto is_const_ref() const -> bool {
        return is_ptr_const() || is_mptr_const() || is_slice_const();
    }

    [[nodiscard]] constexpr auto is_distinct() const -> bool {
        return kind == TypeKind::Distinct;
    }

    [[nodiscard]] constexpr auto is_strview() const -> bool {
        return kind == TypeKind::StrView;
    }

    [[nodiscard]] constexpr auto is_array() const -> bool {
        return kind == TypeKind::Array;
    }

    [[nodiscard]] constexpr auto is_slice() const -> bool {
        return kind == TypeKind::Slice || kind == TypeKind::SliceConst;
    }

    [[nodiscard]] constexpr auto is_slice_mut() const -> bool {
        return kind == TypeKind::Slice;
    }

    [[nodiscard]] constexpr auto is_slice_const() const -> bool {
        return kind == TypeKind::SliceConst;
    }

    [[nodiscard]] constexpr auto is_struct() const -> bool {
        return kind == TypeKind::Struct;
    }

    [[nodiscard]] constexpr auto is_struct_field() const -> bool {
        return kind == TypeKind::StructField;
    }

    [[nodiscard]] constexpr auto is_lit() const -> bool {
        return kind == TypeKind::Lit;
    }

    [[nodiscard]] constexpr auto is_lit_field() const -> bool {
        return kind == TypeKind::LitField;
    }

    struct Func {
        Type* params;
        Type* ret;
        bool  is_var_args;
        bool  is_bound;

        [[nodiscard]] constexpr auto get_params() const -> std::span<Type*> {
            if (is_bound) return params->inner.subspan(1);
            return params->inner;
        }

        [[nodiscard]] constexpr auto get_params_direct() const
            -> std::span<Type*> {
            return params->inner;
        }

        [[nodiscard]] constexpr auto get_ret() const -> std::span<Type*> {
            return ret->inner;
        }

        [[nodiscard]] constexpr auto is_void() const -> bool {
            return get_ret().size() == 1 && get_ret()[0]->is_void();
        }
    };

    [[nodiscard]] auto as_func() const -> Func;

    struct StructField {
        size_t offset;
        Type*  type;
    };

    /// Return a map of field name to field type
    [[nodiscard]] auto as_struct_get_fields() const
        -> std::unordered_map<std::string_view, StructField>;

    [[nodiscard]] auto as_struct_get_fields_vec() const
        -> std::vector<std::pair<std::string_view, StructField>>;

    [[nodiscard]] auto undistinct() -> Type* {
        auto t = this;
        while (t->is_distinct()) t = t->inner[0];
        return t;
    }

    [[nodiscard]] auto unpacked() -> Type* {
        auto t = this;
        while (t->is_pack() && t->inner.size() == 1) t = t->inner[0];
        return t;
    }

    // ========================================================================

    [[nodiscard]] constexpr auto alignment() const -> size_t {
        switch (kind) {
            case TypeKind::Err:
            case TypeKind::Type:
            case TypeKind::Void:
            case TypeKind::UntypedInt:
            case TypeKind::Lit: return 0;
            case TypeKind::Uint64:
            case TypeKind::Int64: return alignof(uint64_t);
            case TypeKind::Uint32:
            case TypeKind::Int32: return alignof(uint32_t);
            case TypeKind::Uint16:
            case TypeKind::Int16: return alignof(uint16_t);
            case TypeKind::Uint8:
            case TypeKind::Int8: return alignof(uint8_t);
            case TypeKind::Usize:
            case TypeKind::Isize: return alignof(size_t);

            case TypeKind::Bool: return alignof(bool);

            case TypeKind::Float32: return alignof(float);
            case TypeKind::Float64: return alignof(double);

            // made of 2 pointers, so aligns to pointer
            case TypeKind::StrView:
            case TypeKind::Slice:
            case TypeKind::SliceConst: return alignof(uintptr_t); break;

            case TypeKind::Ptr:
            case TypeKind::PtrConst:
            case TypeKind::RawPtr:
            case TypeKind::MultiPtr:
            case TypeKind::MultiPtrConst: return alignof(uintptr_t); break;

            case TypeKind::Array: return inner[0]->alignment();

            case TypeKind::Struct: {
                size_t align = 1;
                for (auto it : inner) align = std::max(align, it->alignment());
                return align;
            }
            case TypeKind::StructField:
            case TypeKind::LitField: return inner[0]->alignment();

            case TypeKind::Nil:
            case TypeKind::Func:
            case TypeKind::FuncWithVarArgs:
            case TypeKind::BoundFunc: return 0;

            // this can't be instantiated, so it should not have alignment
            case TypeKind::Pack: return 0;

            case TypeKind::Distinct: return inner[0]->alignment();
        }

        return 0;
    }

    [[nodiscard]] constexpr auto size() const -> size_t {
        switch (kind) {
            case TypeKind::Err:
            case TypeKind::Type:
            case TypeKind::Void:
            case TypeKind::UntypedInt:
            case TypeKind::Lit: return 0;
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
            case TypeKind::StrView:
            case TypeKind::Slice:
            case TypeKind::SliceConst: return sizeof(uintptr_t) * 2; break;

            case TypeKind::Ptr:
            case TypeKind::PtrConst:
            case TypeKind::RawPtr:
            case TypeKind::MultiPtr:
            case TypeKind::MultiPtrConst: return sizeof(uintptr_t); break;

            case TypeKind::Array: return inner[0]->size() * count;

            case TypeKind::Struct: {
                size_t total{};

                for (auto it : inner) {
                    auto align = it->alignment();
                    total = (total + (align - 1)) & -align;
                    total += it->size();
                }

                return total;
            }
            case TypeKind::StructField:
            case TypeKind::LitField: return inner[0]->size();

            case TypeKind::Nil:
            case TypeKind::Func:
            case TypeKind::FuncWithVarArgs:
            case TypeKind::BoundFunc: return 0;

            // this can't be instantiated, so it should not have size
            case TypeKind::Pack: return 0;

            case TypeKind::Distinct: return inner[0]->size();
        }

        return 0;
    }

    // ========================================================================

    constexpr auto operator==(Type const& other) const -> bool {
        namespace r = std::ranges;
        namespace rv = std::ranges::views;

        auto toref = [](Type* o) -> Type const& { return *o; };
        return kind == other.kind && id == other.id &&
               r::equal(rv::transform(inner, toref),
                        rv::transform(other.inner, toref));
    }

    TypeKind         kind;
    uint32_t         count{};
    std::string_view id;
    std::span<Type*> inner;

    Decl* decl{};
};

}  // namespace yal::types

template <>
struct std::hash<yal::types::Type> {
    auto operator()(yal::types::Type const& type) const -> std::size_t {
        auto v = std::hash<uint16_t>{}(static_cast<uint16_t>(type.kind)) ^
                 std::hash<std::string_view>{}(type.id);

        for (auto const& inner : type.inner) {
            v ^= std::hash<yal::types::Type>{}(*inner);
        }

        return v;
    }
};

namespace yal::types {

struct TypeStore {
    struct TypeItem {
        TypeItem* next;
        Type      type;
    };

    struct Builtin {
        Type* error;
        Type* type;
        Type* _void;

        Type* untyped_int;

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

        Type* rawptr;
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

    TypeStore() = default;

    [[nodiscard]] auto get_error() const -> Type* { return builtin.error; }
    [[nodiscard]] auto get_type() const -> Type* { return builtin.type; }
    [[nodiscard]] auto get_void() const -> Type* { return builtin._void; }

    [[nodiscard]] auto get_untyped_int() const -> Type* {
        return builtin.untyped_int;
    }

    [[nodiscard]] auto get_default_int() const -> Type* { return get_i32(); }

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
    [[nodiscard]] auto get_rawptr() const -> Type* { return builtin.rawptr; }

    [[nodiscard]] auto new_ptr(Type* inner, bool is_const) -> Type* {
        return new_type(is_const ? TypeKind::PtrConst : TypeKind::Ptr,
                        std::array{inner});
    }

    [[nodiscard]] auto new_mptr(Type* inner, bool is_const) -> Type* {
        return new_type(is_const ? TypeKind::MultiPtrConst : TypeKind::MultiPtr,
                        std::array{inner});
    }

    [[nodiscard]] auto new_slice(Type* inner, bool is_const) -> Type* {
        return new_type(is_const ? TypeKind::SliceConst : TypeKind::Slice,
                        std::array{inner});
    }

    [[nodiscard]] auto new_pack(std::span<Type* const> inner) -> Type*;

    [[nodiscard]] auto new_array_type(Type* inner, uint32_t count) -> Type* {
        return new_type(TypeKind::Array, std::array{inner}, "", count);
    }

    [[nodiscard]] auto new_struct(std::span<Type* const> fields) -> Type* {
        return new_type(TypeKind::Struct, fields);
    }

    [[nodiscard]] auto new_struct_field(std::string_view name, Type* type)
        -> Type* {
        return new_type(TypeKind::StructField, std::array{type}, name);
    }

    [[nodiscard]] auto new_lit(std::span<Type* const> fields) -> Type* {
        return new_type(TypeKind::Lit, fields);
    }

    [[nodiscard]] auto new_lit_field(std::string_view name, Type* type)
        -> Type* {
        return new_type(TypeKind::LitField, std::array{type}, name);
    }

    [[nodiscard]] auto new_func(Type* params, Type* ret, bool has_var_args)
        -> Type* {
        return new_type(
            has_var_args ? TypeKind::FuncWithVarArgs : TypeKind::Func,
            std::array{params, ret});
    }

    // [[nodiscard]] auto new_distinct(Type* inner) -> Type* {
    //     return new_type(TypeKind::Distinct, std::array{inner});
    // }

    [[nodiscard]] auto new_distinct_of(std::string_view id, Type* inner)
        -> Type* {
        return new_type(TypeKind::Distinct, std::array{inner}, id);
    }

    [[nodiscard]] auto new_type(TypeKind kind, std::span<Type* const> inner,
                                std::string_view id = "", uint32_t count = 0)
        -> Type* {
        auto ty = types.create<TypeItem>(
            head, Type{.kind = kind,
                       .count = count,
                       .id = strings.alloc_string_view(id),
                       .inner = new_array(inner)});
        head = ty;

        return &ty->type;
    }

    [[nodiscard]] auto new_array(std::span<Type* const> arr)
        -> std::span<Type*> {
        if (arr.empty()) return {};
        return types.alloc<Type*>(arr);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto get_default_for(Type* ty) const -> Type* {
        if (ty->is_integral() && ty->is_untyped_int()) {
            return get_default_int();
        }

        return ty;
    }

    // ------------------------------------------------------------------------

    void add_function_to_type(Type const& ty, std::string_view name, Decl* d);

    auto get_function_from_type(Type const& ty, std::string_view name) const
        -> Decl*;

    auto new_bound_from(Type const* ty) -> Type*;

    auto get_namespaced_functions() const -> std::unordered_map<
        Type, std::unordered_map<std::string_view, Decl*>> const& {
        return namespaced;
    }

    // ========================================================================

    [[nodiscard]] auto coerce(Type* dst, Type* src) -> Type*;
    [[nodiscard]] auto cast(Type* dst, Type* src) -> Type*;

    // ========================================================================

    [[nodiscard]] auto begin() const -> Iterator { return {head}; }
    [[nodiscard]] auto end() const -> Iterator { return {}; }

    void set_builtins(Builtin const& builtins) { builtin = builtins; }

private:
    TypeItem* head{};

    std::unordered_map<Type, std::unordered_map<std::string_view, Decl*>>
        namespaced;

    mem::Arena strings;
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
        case TypeKind::UntypedInt: name = "UntypedInt"; break;
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
        case TypeKind::Array: name = "Array"; break;
        case TypeKind::Struct: name = "struct"; break;
        case TypeKind::StructField: name = "struct.field"; break;
        case TypeKind::Lit: name = "Lit"; break;
        case TypeKind::LitField: name = "LitField"; break;
        case TypeKind::Ptr: name = "Ptr"; break;
        case TypeKind::PtrConst: name = "PtrConst"; break;
        case TypeKind::RawPtr: name = "RawPtr"; break;
        case TypeKind::MultiPtr: name = "MultiPtr"; break;
        case TypeKind::MultiPtrConst: name = "MultiPtrConst"; break;
        case TypeKind::Nil: name = "Nil"; break;
        case TypeKind::Func: name = "Func"; break;
        case TypeKind::FuncWithVarArgs: name = "FuncWithVarArgs"; break;
        case TypeKind::Pack: name = "(pack)"; break;
        case TypeKind::BoundFunc: name = "BoundFunc"; break;
        case TypeKind::Distinct: name = "Distinct"; break;
        case TypeKind::Slice: name = "Slice"; break;
        case TypeKind::SliceConst: name = "SliceConst"; break;
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
