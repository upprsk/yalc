#include "test_helpers.hpp"

#include <unistd.h>

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <memory>
#include <stdexcept>
#include <string_view>

#include "fmt/color.h"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "subprocess.hpp"
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
    while (c != '\n' && getchar() != '\n');

    return c == '\n' || c == 'y' || c == 'Y';
}

auto json_to_pretty_string(json const& j) -> std::string {
    if (j.contains("stderr")) return j.at("stderr").get<std::string>();
    return j.dump(2);
}

auto path_of_file(int fd) -> std::filesystem::path {
    return std::filesystem::read_symlink(
        std::filesystem::path{"/proc/self/fd"} / fmt::to_string(fd));
}

auto path_of_file(FILE* f) -> std::filesystem::path {
    return path_of_file(fileno(f));
}

auto get_tmp_file(std::string_view initial_contents)
    -> std::unique_ptr<FILE, void (*)(FILE*)> {
    // :(
    std::string path = "/tmp/yal-tests-XXXXXX";
    auto        fd = mkstemp(path.data());

    std::unique_ptr<FILE, void (*)(FILE*)> f = {
        fdopen(fd, "wb+"), [](FILE* f) {
            unlink(path_of_file(f).c_str());
            fclose(f);
        }};

    fwrite(initial_contents.data(), sizeof(*initial_contents.data()),
           initial_contents.size(), f.get());
    fflush(f.get());

    return f;
}

void show_diff(std::string_view name, json const& left, json const& right) {
    auto left_file = get_tmp_file(json_to_pretty_string(left));
    auto right_file = get_tmp_file(json_to_pretty_string(right));
    subprocess::call({"git", "--no-pager", "diff", "--no-index",
                      path_of_file(left_file.get()).string(),
                      path_of_file(right_file.get()).string()});
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
            fmt::println("{} got as output:\n{}", fullname,
                         json_to_pretty_string(output));

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
        if (p.use_diff) {
            show_diff(fullname, exp, output);
        } else {
            fmt::println("{} got as output:\n{}", fullname,
                         json_to_pretty_string(output));
            fmt::println("but expected:\n{}", fullname,
                         json_to_pretty_string(exp));
        }

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
