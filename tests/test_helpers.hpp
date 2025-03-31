#pragma once

#include <string>

#include "nlohmann/json_fwd.hpp"

using json = nlohmann::json;

struct Context {
    int failed{};
    int ok{};

    [[nodiscard]] constexpr auto total() const -> int { return ok + failed; }
};

struct TestParams {
    bool ask_for_updates;
};

// Generate a path relative to the current .cpp file
auto gen_filepath(std::string name) -> std::string;

/// Load a json from the given filename (path). In case the expectation failed
/// to load, return null.
auto load_expectation(std::string filename) -> json;

/// As the user for generating expectations for the given test.
auto ask_for_updates(std::string_view name) -> bool;

/// Given the output of the test, check against expectations. Returns true when
/// the test case succeeds and false when it fails.
auto run_checks_for_test_output(TestParams const& p, std::string name,
                                json const& output) -> bool;

/// Run a given test.
///
/// If the test had success, increments `ctx.ok`, otherwise, increments
/// `ctx.failed`.
inline void run_checks_for_test(Context& ctx, TestParams const& p,
                                std::string name, auto&& get_output) {
    auto ok = run_checks_for_test_output(p, name, get_output());
    if (ok)
        ctx.ok++;
    else
        ctx.failed++;
}
