#pragma once

#include <any>
#include <functional>
#include <string>
#include <vector>

namespace ut {

static constexpr int VERBOSE_LEVEL_NONE = 0;
static constexpr int VERBOSE_LEVEL_INFO = 1;
static constexpr int VERBOSE_LEVEL_DETAIL = 2;
static constexpr int VERBOSE_LEVEL_MAX = VERBOSE_LEVEL_DETAIL;

struct Options {
    int verbose{};

    [[nodiscard]] constexpr auto is_verbose_info() const -> bool {
        return verbose >= VERBOSE_LEVEL_INFO;
    }

    [[nodiscard]] constexpr auto is_verbose_detail() const -> bool {
        return verbose >= VERBOSE_LEVEL_DETAIL;
    }
};

using TestParams = std::vector<std::any>;

struct Test {
    std::string name;

    std::vector<TestParams>                       params_list;
    std::function<bool(TestParams const& params)> func;

    std::vector<Test> children;

    bool skip = false;

    void add_test(std::string name, auto&& fn);
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

auto run_tests(Options const& opt, Test const& test, int level = 1)
    -> TestResult;
void print_result(TestResult const& result, std::string_view name = "");

inline void Test::add_test(std::string name, auto&& fn) {
    children.push_back(new_test(std::move(name), std::move(fn)));
}

}  // namespace ut

namespace yal::tests {

auto file_store() -> ut::Test;

}  // namespace yal::tests
