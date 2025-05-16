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
    std::ranges::replace(name, '/', '-');

    return path.parent_path() / ".outputs" / fmt::format("{}.test.json", name);
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
    if (j.contains("stdout")) return j.at("stdout").get<std::string>();
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

void show_diff(json const& left, json const& right) {
    auto left_file = get_tmp_file(json_to_pretty_string(left));
    auto right_file = get_tmp_file(json_to_pretty_string(right));
    subprocess::call({"git", "--no-pager", "diff", "--no-index",
                      path_of_file(left_file.get()).string(),
                      path_of_file(right_file.get()).string()});
}

auto run_checks_for_test_output(Context& ctx, TestParams const& p,
                                std::string name, json const& output) -> bool {
    auto name_with_tags = ctx.tags;
    name_with_tags.push_back(name);
    auto fullname = fmt::to_string(fmt::join(name_with_tags, "-"));
    auto filename = gen_filepath(fullname);

    if (ctx.has_run(fullname)) {
        fmt::print(fmt::fg(fmt::color::red) | fmt::emphasis::bold,
                   "duplicate test name:");
        fmt::println(" {:?}", fullname);
        return false;
    }

    ctx.mark_run(fullname);

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
            show_diff(exp, output);
        } else if (p.verbose) {
            fmt::println("{} got as output:\n{}", fullname,
                         json_to_pretty_string(output));
            fmt::println("but expected:\n{}", json_to_pretty_string(exp));
        }

        fmt::print(fmt::bg(fmt::color::red), "FAIL");
        fmt::println(" {} got unexpected value", fullname);

        if (p.ask_for_updates) {
            auto gen = ask_for_updates(fullname);
            if (gen) {
                yal::write_file(filename, output.dump());
                return true;
            }
        }

        return false;
    }

    if (p.show_ok) {
        fmt::print(fmt::bg(fmt::color::green), "OK");
        fmt::println(" {}", fullname);
    }

    return true;
}

void handle_assertion(libassert::assertion_info const& info) {
    fmt::print(fmt::bg(fmt::color::red), "FAIL");
    fmt::println(" assertion failed:\n{}", info.to_string());

    throw AssertionError{"Assertion failed"};
}

void print_test_results(std::string_view name, Context const& ctx) {
    print_test_results(name, ctx.total(), ctx.ok, ctx.failed, ctx.skipped);
}

void print_test_results(std::string_view name, int total, int ok, int failed,
                        int skipped) {
    fmt::print("{} tests: {} total, ", name, total);

    if (ok > 0)
        fmt::print(fmt::fg(fmt::color::green), "{} success", ok);
    else
        fmt::print("{} success", ok);

    fmt::print(", ");

    if (failed > 0)
        fmt::print(fmt::fg(fmt::color::red), "{} failed", failed);
    else
        fmt::print("{} failed", failed);

    fmt::print(", ");

    if (skipped > 0)
        fmt::print(fmt::fg(fmt::color::orange), "{} skipped", skipped);
    else
        fmt::print("{} skipped", skipped);

    fmt::println("");
}
