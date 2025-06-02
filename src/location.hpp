#pragma once

#include <cstdint>
#include <nlohmann/json_fwd.hpp>
#include <string_view>

#include "file_store.hpp"
#include "macros.hpp"

namespace yal {

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

struct Location {
    FileId fileid;
    Span   span;

    [[nodiscard]] constexpr auto str(std::string_view source) const
        -> std::string_view {
        return span.str(source);
    }

    [[nodiscard]] constexpr auto extend(Location const &other) const
        -> Location {
        // assume: other.fileid == this.fileid
        return {.fileid = fileid, .span = span.extend(other.span)};
    }

    [[nodiscard]] constexpr auto extend(Span const &other) const -> Location {
        return {.fileid = fileid, .span = span.extend(other)};
    }

    constexpr auto operator==(Location const &o) const -> bool = default;
};

// ============================================================================

void to_json(nlohmann::json &j, Span const &span);
void to_json(nlohmann::json &j, Location const &location);

}  // namespace yal

// ============================================================================

define_formatter_from_string_view(yal::Span);
define_formatter_from_string_view(yal::Location);
