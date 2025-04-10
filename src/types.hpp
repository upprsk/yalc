#pragma once

#include <vector>

#include "type-id.hpp"

namespace yal::types {

enum class TypeKind {
    Err,

    Uint64,
    Int64,
    Uint32,
    Int32,
    Uint16,
    Int16,
    Uint8,
    Int8,

    Usize,
    Isize,

    Float32,
    Float64,

    StrView,
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

    TypeKind            kind;
    TypeId              id;
    std::vector<TypeId> inner;
};

struct TypeStore {
    struct Builtin {
        TypeId error;

        TypeId uint64;
        TypeId int64;
        TypeId uint32;
        TypeId int32;
        TypeId uint16;
        TypeId int16;
        TypeId uint8;
        TypeId int8;

        TypeId usize;
        TypeId isize;

        TypeId f32;
        TypeId f64;

        TypeId strview;
    };

    void add_builtins() {
        builtin.error = new_type(TypeKind::Err, {});

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

        builtin.f32 = new_type(TypeKind::Float32, {});
        builtin.f64 = new_type(TypeKind::Float64, {});

        builtin.strview = new_type(TypeKind::StrView, {});
    }

    [[nodiscard]] auto get(TypeIdOfRef h) const -> Type {
        return types.at(h.value);
    }

    [[nodiscard]] auto get_error() const -> TypeId { return builtin.error; }
    [[nodiscard]] auto get_i32() const -> TypeId { return builtin.int32; }
    [[nodiscard]] auto get_f32() const -> TypeId { return builtin.f32; }
    [[nodiscard]] auto get_f64() const -> TypeId { return builtin.f64; }
    [[nodiscard]] auto get_strview() const -> TypeId { return builtin.strview; }

    [[nodiscard]] auto new_type(TypeKind kind, std::vector<TypeId> inner)
        -> TypeId {
        auto sz = types.size();
        auto id = TypeId::from_raw_data(sz);
        types.push_back({kind, id, inner});

        return id;
    }

    [[nodiscard]] auto coerce(TypeId from, TypeId to) -> TypeId;

    [[nodiscard]] auto equal(Type const &lhs, Type const &rhs) const -> bool;
    [[nodiscard]] auto equal(TypeId lhs, TypeId rhs) const -> bool {
        if (lhs.is_valid() && rhs.is_valid())
            return equal(get(lhs.as_ref()), get(rhs.as_ref()));

        return lhs.is_valid() == rhs.is_valid();
    }

    [[nodiscard]] constexpr auto type_size(Type const &ty) const -> size_t {
        switch (ty.kind) {
            case TypeKind::Err: return 0;
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

            case TypeKind::Float32: return sizeof(float);
            case TypeKind::Float64: return sizeof(double);

            // made of 2 pointers, so double the size of a pointer
            case TypeKind::StrView: return sizeof(uintptr_t) * 2; break;
        }

        return 0;
    }

    std::vector<Type> types;
    Builtin           builtin;
};

constexpr auto format_as(TypeKind kind) {
    std::string_view name;

    switch (kind) {
        case TypeKind::Err: name = "Err"; break;
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
        case TypeKind::Float32: name = "f32"; break;
        case TypeKind::Float64: name = "f64"; break;
        case TypeKind::StrView: name = "string_view"; break;
    }

    return name;
}

void to_json(json &j, Type const &t);
void to_json(json &j, TypeStore const &ts);

}  // namespace yal::types

template <>
struct fmt::formatter<yal::types::Type> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::types::Type ty, format_context &ctx) const
        -> format_context::iterator;
};
