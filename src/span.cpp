#include "span.hpp"

auto fmt::formatter<yal::Span>::format(yal::Span s, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}}}", s.begin, s.end);
}
