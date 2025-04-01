#include "test_parser.hpp"

#include "error_reporter.hpp"
#include "file-store.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

using json = nlohmann::json;

auto gen_ast(std::string source) -> json {
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

    return ast.fatten(ast_root);
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name, [&]() { return gen_ast(source); });
}

auto test_parser(TestParams const& p) -> std::pair<int, int> {
    Context ctx{.tags = {"parser"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    run_test(ctx, p, "empty file", "");
    run_test(ctx, p, "minimal", "module main;");

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
func main(a: i32, b: i32) (r: i32) {})");
    run_test(ctx, p, "with multiple return values", R"(module main;
func main(a: i32, b: i32) (i32, i32) {})");
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

    ctx.tags.emplace_back("numbers");
    run_test(ctx, p, "normal integer",
             R"(module _; func _() { return 12345; })");
    run_test(ctx, p, "normal integer 2", R"(module _; func _() { 12345; })");
    run_test(ctx, p, "integer with separators",
             R"(module _; func _() { return 123_456; })");

    run_test(ctx, p, "normal hex", R"(module _; func _() { 0xDEAD_beaf; })");
    run_test(ctx, p, "hex with separators",
             R"(module _; func _() { 0x13_ff_10ae; })");

    run_test(ctx, p, "normal float", R"(module _; func _() { 3.14; })");
    run_test(ctx, p, "float with separators",
             R"(module _; func _() { 3.14_159; })");
    ctx.tags.pop_back();

    ctx.tags.emplace_back("strings");

    run_test(ctx, p, "simple",
             R"(module _; func _() { "this is a string!"; })");
    run_test(ctx, p, "with newline",
             R"(module _; func _() { "this is\na string!"; })");
    run_test(ctx, p, "with hex",
             R"(module _; func _() { "this is\x0Aa string!"; })");
    run_test(ctx, p, "with unterminated/invalid hex",
             R"(module _; func _() { "invalid hex: \x1"; })");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("top-level var");
    run_test(ctx, p, "simple", R"(module test; var GLOBAL = 10;)");
    run_test(ctx, p, "simple 2",
             R"(module test; var GLOBAL = something_else;)");

    run_test(ctx, p, "with types and init",
             R"(module test; var GLOBAL: u64 = 0xDEAD_BEEF;)");
    run_test(ctx, p, "with types", R"(module test; var GLOBAL: u64;)");

    run_test(ctx, p, "multiple defs",
             R"(module test; var A, B: u64, u64 = 0xDEAD_BEEF, 0xEAAA;)");
    run_test(ctx, p, "multiple defs 2",
             R"(module test;
var A, B:
    u64, u64 =
    0xDEAD_BEEF, 0xEAAA;)");
    run_test(ctx, p, "multiple defs 3",
             R"(module test;
var A, B =
    0xDEAD_BEEF,
    0xEAAA;)");
    ctx.tags.pop_back();

    ctx.tags.emplace_back("top-level def");
    run_test(ctx, p, "simple", R"(module test; def GLOBAL = 10;)");
    run_test(ctx, p, "simple 2",
             R"(module test; def GLOBAL = something_else;)");

    run_test(ctx, p, "with types and init",
             R"(module test; def GLOBAL: u64 = 0xDEAD_BEEF;)");
    run_test(ctx, p, "with types", R"(module test; def GLOBAL: u64;)");

    run_test(ctx, p, "multiple vars",
             R"(module test; def A, B: u64, u64 = 0xDEAD_BEEF, 0xEAAA;)");
    run_test(ctx, p, "multiple vars 2",
             R"(module test;
def A, B:
    u64, u64 =
    0xDEAD_BEEF, 0xEAAA;)");
    run_test(ctx, p, "multiple vars 3",
             R"(module test;
def A, B =
    0xDEAD_BEEF,
    0xEAAA;)");
    ctx.tags.pop_back();

    ctx.tags.emplace_back("local var");

    run_test(ctx, p, "simple local",
             R"(module test;

func test() {
    var x = 0;
})");

    run_test(ctx, p, "multiple local",
             R"(module test;

func test() {
    var x, y = 0, 1;
})");

    run_test(ctx, p, "multiple local with types",
             R"(module test;

func test() {
    var x, y: i32, u8 = 0, 1;
})");

    run_test(ctx, p, "operations",
             R"(module test;

func test() {
    var x = 0;
    var y = 0;
    return x;
})");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("local def");

    run_test(ctx, p, "simple local",
             R"(module test;

func test() {
    def x = 0;
})");

    run_test(ctx, p, "multiple local",
             R"(module test;

func test() {
    def x, y = 0, 1;
})");

    run_test(ctx, p, "multiple local with types",
             R"(module test;

func test() {
    def x, y: i32, u8 = 0, 1;
})");

    run_test(ctx, p, "operations",
             R"(module test;

func test() {
    def x = 0;
    def y = 0;
    return x;
})");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("decorated functions");

    run_test(ctx, p, "extern",
             R"(module test;

@extern
func test();)");

    run_test(ctx, p, "something with args",
             R"(module test; @something(arg_1=1) func test();)");
    run_test(ctx, p, "something with args 2",
             R"(module test; @something(arg_1=1, arg_2) func test();)");
    run_test(ctx, p, "something with args 3",
             R"(module test; @something(arg_1=1, arg_2, 3) func test();)");

    run_test(ctx, p, "extern with args",
             R"(module test;

@extern(link_name="c_test")
func test();)");

    run_test(ctx, p, "multiple decorators 1",
             R"(module test;

@private(file)
@something() func test();)");

    run_test(ctx, p, "multiple decorators",
             R"(module test;

@private(file)
@extern(link_name="c_test")
func test();)");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("decorated decls");

    run_test(ctx, p, "extern var",
             R"(module test; @extern var g_counter: i32;)");
    run_test(
        ctx, p, "extern var with name",
        R"(module test; @extern(link_name="g_counter") var counter: i32;)");

    run_test(ctx, p, "private def", R"(module test; @private def VALUE = 12;)");
    run_test(ctx, p, "file private def",
             R"(module test; @private(file) def THING: u64 = 69;)");

    ctx.tags.pop_back();

    fmt::println("parser tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
