#include "tests.hpp"

#include <fmt/color.h>
#include <fmt/format.h>

#include <filesystem>
#include <libassert/assert.hpp>
#include <ranges>
#include <string_view>

#include "nlohmann/json.hpp"
#include "utils.hpp"

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

void run_one(Options const& opt, Test const& test, TestResult& result,
             auto&& fn) {
    if (test.skip) {
        if (opt.is_verbose_detail()) {
            println(stderr, "{:?} skipped", fmt::styled(test.name, skip_style));
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
            println(stderr, "{:?} crashed:\n{}",
                    fmt::styled(test.name, string_style), err.info.to_string());
        } else {
            println(stderr, "{:?} {}", fmt::styled(test.name, string_style),
                    fmt::styled("crashed", crash_style));
        }

        result.crashed += 1;
    }
}

auto load_expectation(std::filesystem::path const& path) -> nlohmann::json {
    auto data = yal::read_entire_file(path.string());
    if (!data) return {};

    return nlohmann::json::parse(*data);
}

auto snap_function(
    TestContext const&                                           c,
    std::function<nlohmann::json(FILE* out, TestContext const&)> fn) -> bool {
    auto const& p = c.params;
    ASSUME(p.size() == 2);

    auto const& name = std::any_cast<std::string>(p[0]);
    auto const& source = std::any_cast<std::string>(p[1]);

    auto err = yal::MemStream{};
    auto received = fn(err.f, c);

    auto j = nlohmann::json{
        {"tokens", std::move(received)},
        {"stderr",     err.flush_str()},
    };

    namespace fs = std::filesystem;
    auto path = fs::path{"tests"} / ".cases";
    if (!fs::is_directory(path)) {
        fs::create_directory(path);
    }

    path /= fmt::format("case-{}-{:03}.json", name, c.idx);

    auto expectation = load_expectation(path);
    if (expectation.is_null()) {
        println(stderr, "{:?} has {}", fmt::styled(c.test.name, string_style),
                fmt::styled("no expectation", fail_style));
        if (c.opt.ask) {
            println(stderr, "no expectation found for {:?}, generate? (Y/n",
                    fmt::styled(c.test.name, string_style));
            auto c = getchar();
            while (c != '\n' && getchar() != '\n');

            if (c == '\n' || c == 'y' || c == 'Y') {
                yal::write_file(path, j.dump());
                return true;
            }
        }

        return false;
    }

    if (j != expectation) {
        println(stderr, "{:?} received wrong value",
                fmt::styled(c.test.name, string_style));
        if (c.opt.is_verbose_info()) {
            // TODO: show diff of output
        }

        if (c.opt.ask) {
            println(stderr, "no expectation found for {:?}, generate? (Y/n",
                    fmt::styled(c.test.name, string_style));
            auto c = getchar();
            while (c != '\n' && getchar() != '\n');

            if (c == '\n' || c == 'y' || c == 'Y') {
                yal::write_file(path, j.dump());
                return true;
            }
        }

        return false;
    }

    if (c.opt.is_verbose_detail()) {
        println(stderr, "{:?}: success",
                fmt::styled(c.test.name, string_style));
    }

    return true;
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
                    fmt::styled("running:", header_style),
                    fmt::styled(test.name, string_style), level);
        } else {
            println(stderr, "{} {:?}", fmt::styled("starting:", header_style),
                    test.name);
        }
    }

    if (test.func) {
        if (test.params_list.empty()) {
            run_one(opt, test, result, [&] {
                return test.func({.params = {}, .test = test, .opt = opt});
            });
        } else {
            TestResult kids_result;

            for (auto const& [idx, params] :
                 std::ranges::views::enumerate(test.params_list)) {
                run_one(opt, test, kids_result, [&] {
                    return test.func({.params = params,
                                      .test = test,
                                      .opt = opt,
                                      .idx = static_cast<size_t>(idx)});
                });
            }

            result += kids_result;
        }
    }

    for (auto const& child : test.children) {
        result += run_tests(opt, child, level + 1);
    }

    if (!test.children.empty() && opt.is_verbose_info()) {
        print_result(result, opt.is_verbose_detail() ? "" : test.name);
    }

    return result;
}

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

    println(stderr, "");
}

}  // namespace ut
