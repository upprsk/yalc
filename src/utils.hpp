#pragma once

#include <optional>
#include <string>

namespace yal {

auto read_entire_file(std::string const& path) -> std::optional<std::string>;

}  // namespace yal
