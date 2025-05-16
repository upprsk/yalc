#pragma once

#include <algorithm>
#include <exception>
#include <string>
#include <string_view>
#include <unordered_set>

#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "nlohmann/json_fwd.hpp"

using json = nlohmann::json;

struct Context {
    std::vector<std::string>        tags;
    std::unordered_set<std::string> filters;
    std::unordered_set<std::string> tests_ran;

    int failed{};
    int ok{};
    int skipped{};

    void           mark_run(std::string name) { tests_ran.insert(name); }
    constexpr auto has_run(std::string const& name) const -> bool {
        return tests_ran.contains(name);
    }

    [[nodiscard]] constexpr auto total() const -> int {
        return ok + failed + skipped;
    }

    constexpr auto should_run(std::string const& name) const -> bool {
        if (filters.size() == 0 || filters.contains(name)) return true;

        return std::ranges::any_of(
            tags, [&](auto tag) { return filters.contains(tag); });
    }
};

struct TestParams {
    std::unordered_set<std::string> filters;

    bool verbose;
    bool use_diff;
    bool ask_for_updates;
    bool show_ok;
};

struct AssertionError : public std::exception {
    [[nodiscard]] explicit AssertionError(std::string message)
        : message(std::move(message)) {}

    [[nodiscard]] auto what() const noexcept -> char const* override {
        return message.c_str();
    }

    std::string message;
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
auto run_checks_for_test_output(Context& ctx, TestParams const& p,
                                std::string name, json const& output) -> bool;

/// Run a given test.
///
/// If the test had success, increments `ctx.ok`, otherwise, increments
/// `ctx.failed`.
inline void run_checks_for_test(Context& ctx, TestParams const& p,
                                std::string name, bool skip,
                                auto&& get_output) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        ctx.skipped++;
        return;
    }

    if (!ctx.should_run(name)) return;

    auto ok = false;
    try {
        ok = run_checks_for_test_output(ctx, p, name, get_output());
    } catch (AssertionError const& e) {
        fmt::print(fmt::bg(fmt::color::red), "FAIL");
        fmt::println(" {} assertion failed", name);
        ok = false;
    }

    if (ok)
        ctx.ok++;
    else
        ctx.failed++;
}

/// Handler for libassert assertion failures.
void handle_assertion(libassert::assertion_info const& info);

void print_test_results(std::string_view name, Context const& ctx);
void print_test_results(std::string_view name, int total, int ok, int failed,
                        int skipped);
