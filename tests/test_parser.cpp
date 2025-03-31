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
    Context ctx{.tags = {"parser"}};

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
    run_test(ctx, p, "with parameter", R"(module f;
func main(a: i32) i32 {})");
    run_test(ctx, p, "with parameters", R"(module f;
func main(a: i32, b: i32) i32 {})");
    run_test(ctx, p, "with parameter with no type", R"(module f;
func main(a, b: i32) {})");
    run_test(ctx, p, "with named return", R"(module f;
func main(a: i32, b: i32) (r: i32) {})");
    run_test(ctx, p, "with multiple return values", R"(module f;
func main(a: i32, b: i32) (i32, i32) {})");
    run_test(ctx, p, "with multiple named return values", R"(module f;
func main(a: i32, b: i32) (x: i32, y: i32) {})");
    run_test(ctx, p, "with mixed return values", R"(module f;
func main() (i32, y: i32) {})");
    run_test(ctx, p, "main function", R"(module main;

    func main() i32 {
        return 0;
    })",
             true);
    ctx.tags.pop_back();

    fmt::println("parser tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
