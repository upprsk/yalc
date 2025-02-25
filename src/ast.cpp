#include "ast.hpp"

namespace yal {}  // namespace yal

auto fmt::formatter<yal::NodeHandle>::format(yal::NodeHandle n,
                                             format_context& ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "{{");

    if (!n.is_valid()) {
        fmt::format_to(ctx.out(), "!");
    }

    if (n.is_array()) {
        fmt::format_to(ctx.out(), "[{}]", n.as_idx());
    } else {
        fmt::format_to(ctx.out(), "{}", n.as_idx());
    }

    return fmt::format_to(ctx.out(), "}}");
}

auto fmt::formatter<yal::NodeKind>::format(yal::NodeKind   n,
                                           format_context& ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yal::NodeKind::Err: name = "Err"; break;
        case yal::NodeKind::Nil: name = "Nil"; break;
        case yal::NodeKind::File: name = "File"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::Node>::format(yal::Node n, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", n.kind, n.first,
                          n.second);
}
