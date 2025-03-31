#include "test_helpers.hpp"

#include <algorithm>
#include <filesystem>
#include <stdexcept>

#include "fmt/color.h"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "utils.hpp"

auto gen_filepath(std::string name) -> std::string {
    std::filesystem::path path = __FILE__;
    std::ranges::replace(name, ' ', '-');

    return path.parent_path().append(fmt::format("{}.test.json", name));
}

auto load_expectation(std::string filename) -> json {
    auto filedata = yal::read_entire_file(filename);
    if (!filedata) return json{};

    return json::parse(*filedata);
}

auto ask_for_updates(std::string_view name) -> bool {
    fmt::print(fmt::fg(fmt::color::yellow), "Wrong expectation found for ");
    fmt::print("{}", name);
    fmt::print(fmt::fg(fmt::color::yellow), ", generate? (Y/n)");

    auto c = getchar();
    return c == '\n' || c == 'y' || c == 'Y';
}

auto run_checks_for_test_output(Context const& ctx, TestParams const& p,
                                std::string name, json const& output) -> bool {
    auto name_with_tags = ctx.tags;
    name_with_tags.push_back(name);
    auto fullname = fmt::to_string(fmt::join(name_with_tags, "-"));
    auto filename = gen_filepath(fullname);

    auto exp = load_expectation(filename);
    if (exp.is_null()) {
        // no expectation for test, ask for it if in preview mode
        if (p.ask_for_updates) {
            if (output.contains("stderr")) {
                fmt::println("got from tokenizing:\n{}",
                             output.at("stderr").get<std::string_view>());
            } else {
                fmt::println("got from tokenizing:\n{}", output.dump(2));
            }

            auto gen = ask_for_updates(fullname);
            if (gen) {
                yal::write_file(filename, output.dump());
                return true;
            }
        }

        // error
        fmt::print(fmt::bg(fmt::color::red), "FAIL");
        fmt::println(" {} has no expectation", fullname);
        return false;
    }

    // check value
    if (exp != output) {
        if (output.contains("stderr")) {
            fmt::println("got from tokenizing:\n{}",
                         output.at("stderr").get<std::string_view>());
        } else {
            fmt::println("got from tokenizing:\n{}", output.dump(2));
        }

        fmt::println("but expected:\n{}", exp.dump(2));
        fmt::print(fmt::bg(fmt::color::red), "FAIL");
        fmt::println(" got unexpected value", fullname);

        if (p.ask_for_updates) {
            auto gen = ask_for_updates(fullname);
            if (gen) {
                yal::write_file(filename, output.dump());
                return true;
            }
        }

        return false;
    }

    fmt::print(fmt::bg(fmt::color::green), "OK");
    fmt::println(" {}", fullname);
    return true;
}

void handle_assertion(libassert::assertion_info const& info) {
    fmt::print(fmt::bg(fmt::color::red), "FAIL");
    fmt::println(" assertion failed:\n{}", info.to_string());

    throw AssertionError{"Assertion failed"};
}
