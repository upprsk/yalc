#include "span.hpp"

#include "nlohmann/json.hpp"

namespace yal {

void to_json(json &j, Span const &n) {
    j = json{
        {"begin", n.begin},
        {  "end",   n.end},
    };
}

void from_json(json const &j, Span &n) {
    j.at("begin").get_to(n.begin);
    j.at("end").get_to(n.end);
}

}  // namespace yal

auto fmt::formatter<yal::Span>::format(yal::Span s, format_context &ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}}}", s.begin, s.end);
}
