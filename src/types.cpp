#include "types.hpp"

#include <algorithm>
#include <ranges>
#include <string_view>

#include "fmt/base.h"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"

namespace yal::types {

void to_json(json &j, Type const &t) {
    auto arr = json::array();
    for (auto inner : t.inner) {
        arr.push_back(*inner);
    }

    j = json{
        { "kind", fmt::to_string(t.kind)},
        {"inner",                    arr},
    };
}

void to_json(json &j, TypeStore const &ts) {
    auto arr = json::array();
    for (auto const &t : ts) {
        arr.push_back(t);
    }

    j = json{
        {"types", arr},
    };
}

}  // namespace yal::types

auto fmt::formatter<yal::types::Type>::format(yal::types::Type ty,
                                              format_context  &ctx) const
    -> format_context::iterator {
    auto format_no_inner = [&](yal::types::TypeKind ty) {
        std::string_view result;

        switch (ty) {
            case yal::types::TypeKind::Err: result = "<error>"; break;
            case yal::types::TypeKind::Type: result = "type"; break;
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
        case yal::types::TypeKind::Type:
        case yal::types::TypeKind::Void:
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
        case yal::types::TypeKind::Bool:
        case yal::types::TypeKind::Float32:
        case yal::types::TypeKind::Float64:
        case yal::types::TypeKind::StrView:
        case yal::types::TypeKind::Nil: return format_no_inner(ty.kind);

        case yal::types::TypeKind::Pack:
            return fmt::format_to(
                ctx.out(), "({})",
                fmt::join(std::ranges::views::transform(
                              ty.inner,
                              [](yal::types::Type *t) {
                                  return static_cast<yal::types::Type const &>(
                                      *t);
                              }),
                          ", "));
    }

    return fmt::format_to(ctx.out(), "<invalid kind>");
}
