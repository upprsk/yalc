#include "tests.hpp"

#include <fmt/color.h>
#include <fmt/format.h>
#include <fmt/ranges.h>

#include <filesystem>
#include <libassert/assert.hpp>
#include <nlohmann/json.hpp>
#include <ranges>
#include <string_view>

#include "subprocess.hpp"
#include "utils.hpp"

using fmt::print;
using fmt::println;
using nlohmann::json;

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

/// Configure libassert to thrown an exception instead of aborting
void setup_failure_handler() {
    // void set_failure_handler(void (*handler)(assertion_info const&));
    libassert::set_failure_handler([](libassert::assertion_info const& info) {
        throw assertion_error{info};
    });
}

/// Put libassert back to default configuration.
void clear_failure_handler() {
    libassert::set_failure_handler(libassert::default_failure_handler);
}

auto get_test_fullname(Context const& ctx, Test const& t)
    -> std::vector<std::string_view> {
    std::vector<std::string_view> full_name{t.name};
    full_name.append_range(ctx.parents);

    return full_name;
}

/// Print final test results.
void print_final_results(Context const& /*unused*/, Result const& results) {
    println("{:=^50}", " results ");
    println("success: {}, failed: {}, skipped: {}, crashed: {}",
            results.success,
            fmt::styled(results.failed, results.failed > 0
                                            ? fmt::fg(fmt::color::red)
                                            : fmt::text_style{}),
            fmt::styled(results.skipped, results.skipped > 0
                                             ? fmt::fg(fmt::color::orange)
                                             : fmt::text_style{}),
            fmt::styled(results.crashed, results.crashed > 0
                                             ? fmt::fg(fmt::color::red)
                                             : fmt::text_style{}));
    println("total: {}", results.total());
}

/// Print intermediary test results.
void print_results(Context const& /*unused*/, Test const& t,
                   Result const& results) {
    println("{:=^50}", t.name);
    println("success: {}, failed: {}, skipped: {}, crashed: {}",
            results.success, results.failed, results.skipped, results.crashed);
    println("total: {}", results.total());
}

void print_crash(Context const& ctx, Test const& t,
                 libassert::assertion_info const& dump) {
    println("{}: {:?}",
            fmt::styled("crashed",
                        fmt::bg(fmt::color::red) | fmt::fg(fmt::color::white)),
            t.name);

    if (ctx.opt.show_crashes()) println("{}", dump.to_string());
}

void print_fail(Context const& /*unused*/, Test const& t) {
    println("{}: {:?}",
            fmt::styled("failed",
                        fmt::bg(fmt::color::red) | fmt::fg(fmt::color::white)),
            t.name);
}

void print_success(Context const& /*unused*/, Test const& t) {
    println("{}: {:?}",
            fmt::styled("OK",
                        fmt::bg(fmt::color::lime) | fmt::fg(fmt::color::white)),
            t.name);
}

void print_no_expectation(Context const& /*unused*/, Test const& t) {
    println("{}: {:?} has no expectation",
            fmt::styled("failed",
                        fmt::bg(fmt::color::red) | fmt::fg(fmt::color::white)),
            t.name);
}

// ----------------------------------------------------------------------------

auto json_to_pretty_string(json const& j) -> std::string {
    std::string output;

    auto const& out = j["out"];
    auto const& data = j["data"];

    if (!out.is_null() && !out.empty()) output += out;
    if (!data.is_null() && !data.empty()) output += data.dump(2);

    return output;
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

// ----------------------------------------------------------------------------

void sanitize_name(std::string& name) {
    std::ranges::replace(name, ' ', '-');
    std::ranges::replace(name, '/', '-');
    std::ranges::replace(name, '*', '-');
    std::ranges::replace(name, '?', '-');
}

auto gen_filepath(std::string name) -> std::string {
    namespace fs = std::filesystem;

    sanitize_name(name);

    std::filesystem::path path = __FILE__;

    auto outputs_dir = path.parent_path() / ".outputs";
    if (!fs::is_directory(outputs_dir)) {
        fs::create_directory(outputs_dir);
    }

    return outputs_dir / fmt::format("{}.test.json", name);
}

auto load_expectation(std::string const& filepath) -> json {
    auto filedata = yal::read_entire_file(filepath);
    if (!filedata) return json{};

    return json::parse(*filedata);
}

void save_expectation(std::string const& filepath, json const& data) {
    yal::write_file(filepath, data.dump());
}

auto ask_for_update(Context const& /*unused*/, Test const& t,
                    std::string_view message) -> bool {
    print("{:?} {}. generate? (Y/n): ", t.name, message);

    auto c = getchar();
    while (c != '\n' && getchar() != '\n');

    return c == '\n' || c == 'y' || c == 'Y';
}

auto run_test_func(Context const& ctx, Test const& t) -> Result {
    Result r;

    json received;

    try {
        auto ms = yal::MemStream{};
        auto data = t.func(ctx.with_output(ms.f));

        received["out"] = ms.flush_str();
        received["data"] = data;
    } catch (assertion_error const& err) {
        r.crashed += 1;

        if (ctx.opt.show_fails()) print_crash(ctx, t, err.info);
        return r;
    }

    auto full_name = fmt::to_string(fmt::join(get_test_fullname(ctx, t), "."));
    auto filepath = gen_filepath(full_name);

    auto expected = load_expectation(filepath);
    if (expected.is_null()) {
        if (ctx.opt.show_fails()) print_no_expectation(ctx, t);

        if (ctx.opt.ask) {
            if (ask_for_update(ctx, t, "has no expectation")) {
                save_expectation(filepath, received);

                r.success += 1;
                return r;
            }
        }

        r.failed += 1;
        return r;
    }

    if (received != expected) {
        if (ctx.opt.show_fails()) {
            print_fail(ctx, t);
        }

        if (ctx.opt.show_output()) {
            if (ctx.opt.diff) {
                show_diff(expected, received);
            } else {
                println("expected: {}", json_to_pretty_string(expected));
                println("but received: {}", json_to_pretty_string(received));
            }
        }

        if (ctx.opt.ask) {
            if (ask_for_update(ctx, t, "has no expectation")) {
                save_expectation(filepath, received);

                r.success += 1;
                return r;
            }
        }

        r.failed += 1;
        return r;
    }

    if (ctx.opt.show_success()) {
        print_success(ctx, t);
    }

    r.success += 1;
    return r;
}

auto run_test(Context const& ctx, Test const& t) -> Result {
    Result r;

    if (t.func) {
        r += run_test_func(ctx, t);
    }

    if (!t.children.empty()) {
        for (auto const& child : t.children) {
            r += run_test(ctx.with_parent(t.name), child);
        }
    }

    // in case the test has no children or no name, then there is no point in
    // showing results
    if (!t.name.empty() && !t.children.empty() && ctx.opt.show_results()) {
        print_results(ctx, t, r);
    }

    return r;
}

// ============================================================================

auto group(std::string name) -> Test {
    return {.name = std::move(name), .children = {}, .func = {}};
}

void add(Test& t, Test child) { t.children.push_back(std::move(child)); }

void add(Test& t, std::string name, Test::Func func) {
    t.children.push_back({
        .name = std::move(name),
        .children = {},
        .func = std::move(func),
    });
}

void add(Test& t, std::string name, std::function<void()> func) {
    t.children.push_back({
        .name = std::move(name),
        .children = {},
        .func = [f = std::move(func)](auto) -> json {
            f();
            return {};
        },
    });
}

auto test(std::string name, Test::Func func) -> Test {
    return {.name = std::move(name), .children = {}, .func = std::move(func)};
}

auto run(Options const& opt, Test const& t) -> Result {
    setup_failure_handler();

    auto ctx = Context{.out = nullptr, .opt = opt, .parents = {}};
    auto result = run_test(ctx, t);

    clear_failure_handler();

    print_final_results(ctx, result);
    return result;
}

}  // namespace ut
