#include "tests.hpp"

#include <fmt/color.h>
#include <fmt/format.h>

#include <libassert/assert.hpp>
#include <string_view>

using fmt::print;
using fmt::println;

namespace ut {

struct assertion_error : std::exception {
    explicit assertion_error(libassert::assertion_info info)
        : info(std::move(info)) {}

    auto what() const noexcept -> char const* override {
        return "assertion error";
    }

    libassert::assertion_info info;
};

// ============================================================================

constexpr auto header_style = fmt::emphasis::bold;
constexpr auto success_style = fmt::fg(fmt::color::lime_green);
constexpr auto skip_style = fmt::fg(fmt::color::orange);
constexpr auto fail_style = fmt::fg(fmt::color::red);
constexpr auto crash_style =
    fmt::bg(fmt::color::red) | fmt::fg(fmt::color::white) | fmt::emphasis::bold;
constexpr auto results_style = fmt::fg(fmt::color::blue);
constexpr auto string_style = fmt::fg(fmt::color::dark_green);

// ============================================================================

auto new_test(std::string name) -> Test {
    return {
        .name = std::move(name),
        .params_list = {},
        .func = {},
        .children = {},
    };
}

auto new_test(std::string name, std::vector<Test> children) -> Test {
    return {
        .name = std::move(name),
        .params_list = {},
        .func = {},
        .children = std::move(children),
    };
}

// ----------------------------------------------------------------------------

void print_result(TestResult const& result, std::string_view name) {
    if (!name.empty()) {
        print(stderr, "{:<30?}", fmt::styled(name, string_style));
    }

    print(stderr,
          "{} tests: ", fmt::styled(result.count(), fmt::emphasis::bold));

    if (result.successful > 0) {
        print(stderr, success_style, "{} successful", result.successful);
    } else {
        print(stderr, fail_style, "{} successful", result.successful);
    }

    print(stderr, ", ");

    if (result.skipped > 0) {
        print(stderr, skip_style, "{} skipped", result.skipped);
    } else {
        print(stderr, "{} skipped", result.skipped);
    }

    print(stderr, ", ");

    if (result.failed > 0) {
        print(stderr, fail_style, "{} failed", result.failed);
    } else {
        print(stderr, "{} failed", result.failed);
    }

    print(stderr, ", ");

    if (result.crashed > 0) {
        print(stderr, crash_style, "{} crashed", result.crashed);
    } else {
        print(stderr, "{} crashed", result.crashed);
    }

    fmt::println(stderr, "");
}

void run_one(Options const& opt, Test const& test, TestResult& result,
             auto&& fn) {
    if (test.skip) {
        if (opt.is_verbose_detail()) {
            fmt::println(stderr, "{:?} skipped",
                         fmt::styled(test.name, skip_style));
        }
    }

    try {
        auto ok = fn();
        if (ok)
            result.successful += 1;
        else
            result.failed += 1;
    } catch (assertion_error const& err) {
        if (opt.is_verbose_info()) {
            fmt::println(stderr, "{:?} crashed:\n{}",
                         fmt::styled(test.name, string_style),
                         err.info.to_string());
        } else {
            fmt::println(stderr, "{:?} {}",
                         fmt::styled(test.name, string_style),
                         fmt::styled("crashed", crash_style));
        }

        result.crashed += 1;
    }
}

auto run_tests(Options const& opt, Test const& test, int level) -> TestResult {
    // void set_failure_handler(void (*handler)(assertion_info const&));
    libassert::set_failure_handler([](libassert::assertion_info const& info) {
        throw assertion_error{info};
    });

    TestResult result;

    if (opt.is_verbose_detail()) {
        if (level > 0) {
            println(stderr, "{:>{}}{} {:?}", "+", level,
                    fmt::styled("starting:", header_style),
                    fmt::styled(test.name, string_style), level);
        } else {
            println(stderr, "{} {:?}", fmt::styled("starting:", header_style),
                    test.name);
        }
    }

    if (test.func) {
        if (test.params_list.empty()) {
            run_one(opt, test, result, [&] { return test.func({}); });
        } else {
            TestResult kids_result;

            for (auto const& params : test.params_list) {
                run_one(opt, test, kids_result,
                        [&] { return test.func(params); });
            }

            println(stderr, "{} ({} parameters): {:?}",
                    fmt::styled("finished computed", results_style),
                    kids_result.count(), test.name);
        }
    }

    for (auto const& child : test.children) {
        result += run_tests(opt, child, level + 1);
    }

    if (opt.is_verbose_detail()) {
        println(stderr, "{:>{}}{} {:?}:", "-", level,
                fmt::styled("finished test:", header_style),
                fmt::styled(test.name, string_style));
    }

    if (!test.children.empty() && opt.is_verbose_info()) {
        print_result(result, opt.is_verbose_detail() ? "" : test.name);
    }

    return result;
}

}  // namespace ut
