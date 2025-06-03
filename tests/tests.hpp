#pragma once

#include <any>
#include <functional>
#include <string>
#include <vector>

#include "nlohmann/json_fwd.hpp"

namespace ut {

static constexpr int VERBOSE_LEVEL_NONE = 0;
static constexpr int VERBOSE_LEVEL_INFO = 1;
static constexpr int VERBOSE_LEVEL_DETAIL = 2;
static constexpr int VERBOSE_LEVEL_MAX = VERBOSE_LEVEL_DETAIL;

struct Options {
    int  verbose{};
    bool ask{};
    bool diff{};

    [[nodiscard]] constexpr auto is_verbose_info() const -> bool {
        return verbose >= VERBOSE_LEVEL_INFO;
    }

    [[nodiscard]] constexpr auto is_verbose_detail() const -> bool {
        return verbose >= VERBOSE_LEVEL_DETAIL;
    }
};

struct Test;

using TestParams = std::vector<std::any>;
struct TestContext {
    TestParams     params;
    Test const&    test;
    Options const& opt;
    size_t         idx{};
};

struct Test {
    std::string name;

    std::vector<TestParams>                        params_list;
    std::function<bool(TestContext const& params)> func;

    std::vector<Test> children;

    bool skip = false;
    bool snapshot = false;
};

struct TestResult {
    int successful{};
    int failed{};
    int skipped{};
    int crashed{};

    [[nodiscard]] constexpr auto count() const -> int {
        return successful + failed + skipped + crashed;
    }

    constexpr void operator+=(TestResult const& rhs) {
        successful += rhs.successful;
        failed += rhs.failed;
        skipped += rhs.skipped;
        crashed += rhs.crashed;
    }
};

auto new_test(std::string name) -> Test;
auto new_test(std::string name, std::vector<Test> children) -> Test;

inline auto new_test(std::string name, auto&& fn) -> Test {
    return {
        .name = std::move(name),
        .params_list = {},
        .func = std::move(fn),
        .children = {},
    };
}

inline auto new_snap(std::string name, std::vector<TestParams> params,
                     auto&& fn) -> Test {
    return {
        .name = std::move(name),
        .params_list = std::move(params),
        .func = std::move(fn),
        .children = {},
        .snapshot = true,
    };
}

inline auto new_snap(std::string name, auto&& fn) -> Test {
    return new_snap(std::move(name), {}, std::move(fn));
}

auto snap_function(
    TestContext const&                                           c,
    std::function<nlohmann::json(FILE* out, TestContext const&)> fn) -> bool;

auto run_tests(Options const& opt, Test const& test, int level = 1)
    -> TestResult;
void print_result(TestResult const& result, std::string_view name = "");

template <typename F>
concept TestFunction = requires(F fn, TestContext const& c) {
    { fn(c) } -> std::convertible_to<bool>;
};

template <typename F>
concept SnapFunction = requires(F fn, FILE* out, TestContext const& c) {
    { fn(out, c) } -> std::convertible_to<bool>;
};

void add_test(Test& t, std::string name, TestFunction auto&& fn) {
    t.children.push_back(new_test(std::move(name), std::move(fn)));
}

void add_snap(Test& t, std::string name, SnapFunction auto&& fn) {
    t.children.push_back(
        new_snap(std::move(name), [f = std::move(fn)](TestContext const& c) {
            return snap_function(c, std::move(f));
        }));
}

void add_snap(Test& t, std::string name, std::vector<TestParams> params,
              SnapFunction auto&& fn) {
    t.children.push_back(new_snap(std::move(name), std::move(params),
                                  [f = std::move(fn)](TestContext const& c) {
                                      return snap_function(c, std::move(f));
                                  }));
}

}  // namespace ut

namespace yal::tests {

auto file_store() -> ut::Test;
auto integration() -> ut::Test;

}  // namespace yal::tests
