#pragma once

#include <string_view>

namespace yal {

namespace detail {

static inline constexpr std::string_view VERSION = "0.0.1-alpha1";

}  // namespace detail

constexpr auto get_version() -> std::string_view { return detail::VERSION; }

}  // namespace yal
