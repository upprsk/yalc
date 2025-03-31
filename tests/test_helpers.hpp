#pragma once

#include <string>

#include "nlohmann/json_fwd.hpp"

using json = nlohmann::json;

struct Context {
    int failed{};
    int ok{};

    [[nodiscard]] constexpr auto total() const -> int { return ok + failed; }
};

// Generate a path relative to the current .cpp file
auto gen_filepath(std::string name) -> std::string;

/// Load a json from the given filename (path). In case the expectation failed
/// to load, return null.
auto load_expectation(std::string filename) -> json;

/// As the user for generating expectations for the given test.
auto ask_for_updates(std::string_view name) -> bool;
