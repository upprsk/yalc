#include "location.hpp"

#include <fmt/format.h>

#include <nlohmann/json.hpp>

namespace yal {

void to_json(nlohmann::json &j, Span const &span) {
    j = fmt::format("{}", span);
}

void to_json(nlohmann::json &j, Location const &location) {
    j = fmt::format("{}", location);
}

}  // namespace yal

// ============================================================================

auto fmt::formatter<yal::Span>::format(yal ::Span const &p,
                                       format_context   &ctx) const
    -> format_context ::iterator {
    return fmt::format_to(ctx.out(), "{}-{}", p.begin, p.end);
}

auto fmt::formatter<yal::Location>::format(yal ::Location const &p,
                                           format_context       &ctx) const
    -> format_context ::iterator {
    return fmt::format_to(ctx.out(), "{}@{}", p.span, p.fileid);
}
