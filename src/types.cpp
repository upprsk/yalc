#include "types.hpp"

#include <string_view>

#include "fmt/format.h"
#include "nlohmann/json.hpp"

namespace yal::types {

void to_json(json &j, Type const &t) {
    j = json{
        {   "id",                   t.id},
        { "kind", fmt::to_string(t.kind)},
        {"inner",                t.inner},
    };
}

void to_json(json &j, TypeStore const &ts) {
    j = json{
        {"types", ts.types},
    };
}

auto TypeStore::equal(Type const &lhs, Type const &rhs) const -> bool {
    if (lhs.kind != rhs.kind) return false;

    // TODO: handle other types (that may have inner types)
    return true;
}

auto TypeStore::coerce(TypeId from, TypeId to) -> TypeId {
    if (from.is_valid() && !to.is_valid()) return from;
    if (!from.is_valid() && to.is_valid()) return to;
    if (!from.is_valid() && !to.is_valid()) return TypeId::invalid();

    auto from_type = get(from.as_ref());
    auto to_type = get(to.as_ref());

    // same type, just return it.
    if (equal(from_type, to_type)) return to;

    auto from_size = type_size(from_type);
    auto to_size = type_size(to_type);

    switch (from_type.kind) {
        // from error, just return target to avoid further unneeded error
        // messages
        case TypeKind::Err: return to;

        // unsigned integers
        // the other must be an integer, have the same sign and the same
        // bit-width or more.
        case TypeKind::Usize:
        case TypeKind::Uint64:
        case TypeKind::Uint32:
        case TypeKind::Uint16:
        case TypeKind::Uint8:
            if (!to_type.is_integral() || to_type.is_signed())
                return TypeId::invalid();
            if (to_size >= from_size) return to;
            return TypeId::invalid();

        // signed integers
        // the other must be an integer, have the same sign and the same
        // bit-width or more.
        case TypeKind::Isize:
        case TypeKind::Int64:
        case TypeKind::Int32:
        case TypeKind::Int16:
        case TypeKind::Int8:
            if (!to_type.is_integral() || to_type.is_unsigned())
                return TypeId::invalid();
            if (to_size >= from_size) return to;
            return TypeId::invalid();

        // floats, no coercion
        case TypeKind::Float32:
        case TypeKind::Float64: return TypeId::invalid();

        // No coercion
        case TypeKind::StrView: return TypeId::invalid();
    }

    return TypeId::invalid();
}

}  // namespace yal::types

auto fmt::formatter<yal::types::Type>::format(yal::types::Type ty,
                                              format_context  &ctx) const
    -> format_context::iterator {
    auto format_no_inner = [&](yal::types::TypeKind ty) {
        std::string_view result;

        switch (ty) {
            case yal::types::TypeKind::Err: result = "<error>"; break;
            case yal::types::TypeKind::Uint64: result = "u64"; break;
            case yal::types::TypeKind::Int64: result = "i64"; break;
            case yal::types::TypeKind::Uint32: result = "u32"; break;
            case yal::types::TypeKind::Int32: result = "i32"; break;
            case yal::types::TypeKind::Uint16: result = "u16"; break;
            case yal::types::TypeKind::Int16: result = "i16"; break;
            case yal::types::TypeKind::Uint8: result = "u8"; break;
            case yal::types::TypeKind::Int8: result = "i8"; break;
            case yal::types::TypeKind::Usize: result = "usize"; break;
            case yal::types::TypeKind::Isize: result = "isize"; break;
            case yal::types::TypeKind::Float32: result = "f32"; break;
            case yal::types::TypeKind::Float64: result = "f64"; break;
            case yal::types::TypeKind::StrView: result = "string_view"; break;
            default: break;
        }

        return fmt::format_to(ctx.out(), "{}", result);
    };

    switch (ty.kind) {
        case yal::types::TypeKind::Err:
        case yal::types::TypeKind::Uint64:
        case yal::types::TypeKind::Int64:
        case yal::types::TypeKind::Uint32:
        case yal::types::TypeKind::Int32:
        case yal::types::TypeKind::Uint16:
        case yal::types::TypeKind::Int16:
        case yal::types::TypeKind::Uint8:
        case yal::types::TypeKind::Int8:
        case yal::types::TypeKind::Usize:
        case yal::types::TypeKind::Isize:
        case yal::types::TypeKind::Float32:
        case yal::types::TypeKind::Float64:
        case yal::types::TypeKind::StrView: return format_no_inner(ty.kind);
    }

    return fmt::format_to(ctx.out(), "<invalid kind>");
}
