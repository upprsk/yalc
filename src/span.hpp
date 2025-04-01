#pragma once

#include <cstdint>
#include <string_view>

#include "fmt/base.h"
#include "nlohmann/json_fwd.hpp"

namespace yal {
using json = nlohmann::json;

struct Span {
    uint32_t begin;
    uint32_t end;

    [[nodiscard]] constexpr auto size() const -> uint32_t {
        return end - begin;
    }

    [[nodiscard]] constexpr auto str(std::string_view source) const
        -> std::string_view {
        return source.substr(begin, size());
    }

    [[nodiscard]] constexpr auto extend(Span o) const -> Span {
        return {.begin = begin, .end = o.end};
    }

    [[nodiscard]] constexpr auto offset(uint32_t off) const -> Span {
        return {.begin = begin + off, .end = end};
    }

    [[nodiscard]] constexpr auto trim_to_size(uint32_t sz) const -> Span {
        return {.begin = begin, .end = begin + sz};
    }

    constexpr auto operator==(Span const &o) const -> bool = default;
};

void to_json(json &j, Span const &n);
void from_json(json const &j, Span &n);

}  // namespace yal

template <>
struct fmt::formatter<yal::Span> {
    constexpr auto parse(format_parse_context &ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::Span s, format_context &ctx) const
        -> format_context::iterator;
};
