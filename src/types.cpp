#include "types.hpp"

namespace yal {}

auto fmt::formatter<yal::TypeHandle>::format(yal::TypeHandle n,
                                             format_context& ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "{{");

    if (!n.is_valid()) {
        fmt::format_to(ctx.out(), "!");
    }

    if (n.is_array()) {
        fmt::format_to(ctx.out(), "[{:x}]", n.as_idx());
    } else {
        fmt::format_to(ctx.out(), "{:x}", n.as_idx());
    }

    return fmt::format_to(ctx.out(), "}}");
}

auto fmt::formatter<yal::TypeKind>::format(yal::TypeKind   n,
                                           format_context& ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yal::TypeKind::Err: name = "Err"; break;
        case yal::TypeKind::Type: name = "Type"; break;
        case yal::TypeKind::Void: name = "Void"; break;
        case yal::TypeKind::Int32: name = "Int32"; break;
        case yal::TypeKind::Ptr: name = "Ptr"; break;
        case yal::TypeKind::Func: name = "Func"; break;
        case yal::TypeKind::Pack: name = "Pack"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::Type>::format(yal::Type t, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", t.kind, t.first,
                          t.second);
}
