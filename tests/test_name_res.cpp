#include "test_name_res.hpp"

#include "error_reporter.hpp"
#include "file-store.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "test_helpers.hpp"
#include "tokenizer.hpp"

using json = nlohmann::json;

auto gen_ast_resolved(std::string source) -> json {
    char*  buf = nullptr;
    size_t bufsize = 0;

    std::unique_ptr<FILE, void (*)(FILE*)> f = {open_memstream(&buf, &bufsize),
                                                [](auto f) { fclose(f); }};

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, f.get()};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);
    fflush(f.get());

    if (fer.had_error()) {
        return json{
            {"stderr", std::string_view{buf, bufsize}}
        };
    }

    auto [ast, ast_root] = yal::parse(tokens, fer);
    fflush(f.get());

    if (fer.had_error()) {
        return json{
            {"stderr", std::string_view{buf, bufsize}}
        };
    }

    auto prj_root =
        yal::resolve_names(ast, ast_root, er, fs, {.single_file = true});
    fflush(f.get());

    if (er.had_error()) {
        return json{
            {"stderr", std::string_view{buf, bufsize}}
        };
    }

    return ast.fatten(prj_root);
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name,
                        [&]() { return gen_ast_resolved(source); });
}

auto test_name_res(TestParams const& p) -> std::pair<int, int> {
    Context ctx{
        .tags = {"name resolution"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    run_test(ctx, p, "empty file", "");
    run_test(ctx, p, "minimal", "module main;");

    run_test(ctx, p, "hello world libc", R"(
module main;

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);

func main() {
    c_printf("Hello, %s!\n", "World");
}
)");

    ctx.tags.emplace_back("simple functions");
    run_test(ctx, p, "void function 1", R"(module f; func f() {})");
    run_test(ctx, p, "void function 2", R"(module f;

func f() {})");
    run_test(ctx, p, "void function with comment", R"(module f;

// hello! this is a test function
func f() {})");
    run_test(ctx, p, "with parameter", R"(module main;
func main(a: i32) i32 {})");
    run_test(ctx, p, "with parameters", R"(module main;
func main(a: i32, b: i32) i32 {})");
    run_test(ctx, p, "with parameter with no type", R"(module main;
func main(a, b: i32) {})");
    run_test(ctx, p, "with named return", R"(module main;
func main(a: i32, b: i32) (r: i32) { r = a + b; })");
    run_test(ctx, p, "with multiple return values", R"(module main;
func main(a: i32, b: i32) (i32, i32) {})");
    run_test(ctx, p, "with multiple return values 2", R"(module main;
func main(a: i32, b: i32) (i32, i32) { return a, b; })");
    run_test(ctx, p, "with multiple named return values", R"(module main;
func main(a: i32, b: i32) (x: i32, y: i32) {})");
    run_test(ctx, p, "with mixed return values", R"(module main;
func main() (i32, y: i32) {})");
    run_test(ctx, p, "bare return", R"(module f; func f() { return; })");
    run_test(ctx, p, "main function", R"(module main;

func main() i32 {
    return 0;
})");
    run_test(ctx, p, "namespaced", R"(module f; func g.f() {})");
    run_test(ctx, p, "namespaced 2", R"(module f; func g.f(a) b {})");
    run_test(ctx, p, "namespaced 3",
             R"(module f; func f.g(a, b: int) (c: int) {})");
    ctx.tags.pop_back();

    run_test(ctx, p, "hello world libc with shbang", R"(
#!/usr/local/bin/yalc run
module main;

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);

func main() {
    c_printf("Hello, %s!\n", "World");
}
)");

    run_test(ctx, p, "counter example", R"(
module main;

def Counter = struct { cnt: i32 = 0, total: i32 };
func Counter.next(
    c: *Counter,
) (i32, bool) {
    if c.cnt == c.total {
        return c.cnt, false;
    }

    var v = c.cnt;
    c.cnt += 1;
    return v, true;
}

func main() {
    var c: Counter = .{ .total = 10 };
    c_printf("c.cnt=%d, c.total=%d\n", c.cnt, c.total);

    while true {
        var it, ok = c.next();
        if !ok { break; }

        c_printf("it=%d\n", it);
    }
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    fmt::println("name resolver tests, {} tests, {} success, {} failed",
                 ctx.total(), ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
