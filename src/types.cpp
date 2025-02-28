#include "types.hpp"

#include "fmt/base.h"

namespace yal {
using fmt::format_to;

auto TypeStore::dump(fmt::format_context& ctx, TypeHandle n) const
    -> fmt::format_context::iterator {
    auto type = get(n);
    switch (type->kind) {
        case TypeKind::Err: return format_to(ctx.out(), "Err");
        case TypeKind::Type: return format_to(ctx.out(), "type");
        case TypeKind::Pack: {
            auto children = get_children(type);

            format_to(ctx.out(), "(");

            size_t i{};
            for (auto const& chld : children) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return format_to(ctx.out(), ")");
        }
        case TypeKind::Void: return format_to(ctx.out(), "void");
        case TypeKind::Int32: return format_to(ctx.out(), "i32");
        case TypeKind::Ptr:
            format_to(ctx.out(), "*");
            return dump(ctx, type->first);
        case TypeKind::Func: {
            auto f = type->as_func(*this);
            format_to(ctx.out(), "func(");

            size_t i{};
            for (auto const& chld : f.args) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            format_to(ctx.out(), ") ");
            return dump(ctx, f.ret);
        }
    }

    return format_to(ctx.out(), "unknown");
}

}  // namespace yal

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

auto fmt::formatter<yal::FatTypeHandle>::format(yal::FatTypeHandle n,
                                                format_context&    ctx) const
    -> format_context::iterator {
    return n.ts->dump(ctx, n.type);
}
