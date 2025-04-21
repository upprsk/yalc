#include "test_name_res.hpp"

#include <cstdint>
#include <ranges>
#include <string_view>

#include "error_reporter.hpp"
#include "file-store.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "test_helpers.hpp"
#include "tokenizer.hpp"
#include "types.hpp"
#include "utils.hpp"

using json = nlohmann::json;

auto gen_ast_resolved(std::string source) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    auto ts = yal::types::TypeStore{};
    ts.add_builtins();

    auto prj_root =
        yal::resolve_names(ast, ast_root, er, fs, ts, {.single_file = true});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    json j = *prj_root;
    j["ds"] = *ast.get_decl_store();

    return j;
}

auto gen_ast_resolved_many(std::vector<std::string> sources) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    std::unordered_set<yal::FileId> filenames;
    for (uint32_t idx = 0; auto const& source : sources) {
        filenames.insert(fs.add(fmt::format(":memory-{}:", idx), source));
        idx++;
    }

    (void)fs.add_dir(":memory-dir:", filenames);

    auto root = fs.add(":memory-0:");
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    auto ts = yal::types::TypeStore{};
    ts.add_builtins();

    auto prj_root = yal::resolve_names(ast, ast_root, er, fs, ts, {});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    json j = *prj_root;
    j["ds"] = *ast.get_decl_store();

    return j;
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

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::vector<std::string> source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name,
                        [&]() { return gen_ast_resolved_many(source); });
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

    run_test(ctx, p, "redefine same name", R"(
module main;

func main() {
    var x = 0;
    var x = x + 1;
}
)");

    run_test(ctx, p, "self-referential struct 0", R"(
module main;

def S = struct {
    children: []S,
};
)");

    run_test(ctx, p, "self-referential struct", R"(
module main;

def S = struct {
    next: ?*S = nil,
    value: i32,
};

func main() {
    var a: S = .{ .value = 0 };
    var b: S = .{ .next = &a, .value = 1 };
}
)",
             true);

    run_test(ctx, p, "self-referential generic struct", R"(
module main;

def S[T] = struct {
    next: ?*S = nil,
    value: T,
};

func main() {
    var a: S[i32] = .{ .value = 0 };
    var b: S[i32] = .{ .next = &a, .value = 1 };
}
)", true);

    ctx.tags.emplace_back("multi-file");

    run_test(ctx, p, "multi-file 1",
             std::vector<std::string>{
                 R"()",
                 R"()",
             });

    run_test(ctx, p, "multi-file 2",
             std::vector<std::string>{
                 R"(module test;)",
                 R"()",
             });

    run_test(ctx, p, "multi-file 3",
             std::vector<std::string>{
                 R"()",
                 R"(module test;)",
             });

    run_test(ctx, p, "multi-file 4",
             std::vector<std::string>{
                 R"(module test;)",
                 R"(module test;)",
             });

    run_test(ctx, p, "multi-file 5",
             std::vector<std::string>{
                 R"(module test;

             func a() { b(); }
             )",
                 R"(module test;

             func b() {}
             )",
             });

    run_test(ctx, p, "function namespacing",
             std::vector<std::string>{
                 R"(module test;

             def Foo = struct {};

             func Foo.bar(f: *Foo) {}
             )",
                 R"(module test;

             func test() {
                var f: Foo = .{};
                f.bar();
             }
             )",
             });

    run_test(ctx, p, "function namespacing 2",
             std::vector<std::string>{
                 R"(module test;

             func Foo.bar(f: *Foo) {}
             )",
                 R"(module test;

             func test() {
                var f: Foo = .{};
                f.bar();
             }
             )",
             });

    run_test(ctx, p, "named returns",
             std::vector<std::string>{
                 R"(module test;

             def int = i32;

             func foo() (r: int) { r = 12; }
             )",
                 R"(module test;

             var r = 0;
             )",
             });

    run_test(ctx, p, "parameters",
             std::vector<std::string>{
                 R"(module test;

             def int = i32;

             func foo(a, b, c: int) (r: int) {
                r = c.add(int.add(a, b));
             }
             )",
                 R"(module test;

             func int.add(a, b: int) int {
                return a + b;
             }
             )",
             });

    run_test(ctx, p, "duplicate names",
             std::vector<std::string>{
                 R"(module test;
             // first
             var x = 0;)",
                 R"(module test;
             // second
             var x = 0;)",
             });

    run_test(ctx, p, "private names",
             std::vector<std::string>{
                 R"(module test; // first
             @private(.file)
             var x = 0;
             var y = x + 2;
             )",
                 R"(module test; // second
             var x = 0;
             var y = x;
             )",
             });

    run_test(ctx, p, "private names 2",
             std::vector<std::string>{
                 R"(module test; // first
             @private(.file)
             var x = 0;
             var x2 = x + 2;
             )",
                 R"(module test; // second
             var x = 0;
             var xx = x;
             )",
             });

    run_test(ctx, p, "private names 3",
             std::vector<std::string>{
                 R"(module test; // first
             @private(.file)
             var x = 0;
             var x2 = x + 2;
             )",
                 R"(module test; // second
             var x = 0;
             var xx = x;
             )",
                 R"(module test; // third
             var z = x;
             )",
             });

    run_test(ctx, p, "private names 4",
             std::vector<std::string>{
                 R"(module test; // first
             @private(.file)
             func getx() i32 { return 0; }

             var x2 = getx() + 2;
             )",
                 R"(module test; // second
             var getx = 0;
             var xx = getx;
             )",
                 R"(module test; // third
             var z = getx;
             )",
             });

    ctx.tags.pop_back();

    fmt::println("name resolver tests, {} tests, {} success, {} failed",
                 ctx.total(), ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
