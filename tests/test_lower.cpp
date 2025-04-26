#include "test_lower.hpp"

#include "error_reporter.hpp"
#include "lower.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;

auto gen_ast_lowered(std::string source) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);

    auto ts = yal::types::TypeStore{};
    auto prj_root =
        yal::resolve_names(ast, ast_root, er, fs, ts, {.single_file = true});

    yal::sema::sema_ast(ast, prj_root, er, ts, {.single_file = true});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::lower::lower_ast(ast, prj_root, er, ts, {.single_file = true});
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
                        [&]() { return gen_ast_lowered(source); });
}

auto test_lower(TestParams const& p) -> std::pair<int, int> {
    Context ctx{.tags = {"lower"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    ctx.tags.emplace_back("assigment");

    run_test(ctx, p, "discard simple expression 1", R"(
module test;
func test() {
    _ = 10 + 10 - 5;
}
)");

    run_test(ctx, p, "discard simple expression 2", R"(
module test;
func test() {
    var a = 0;
    a, _ = 10 + 10 - 5, a;
}
)");

    run_test(ctx, p, "simple decl and assigments 1", R"(
module test;
func test() {
    var x, y: i8, i32 = 0, 1;
    y, x = x, 10;
}
)");

    run_test(ctx, p, "simple decl and assigment from fn 1", R"(
module test;
func getx() i32 {}
func test() {
    _ = getx();
}
)");

    run_test(ctx, p, "simple decl and assigment from fn 2", R"(
module test;
func getx() (i32, i32) {}
func test() {
    var x: i32;
    _, x = getx();
}
)");

    run_test(ctx, p, "simple decl and assigment from fn 3", R"(
module test;
func getx() (i32, i32) {}
func test() {
    var x: i32;
    x, _ = getx();
}
)");

    run_test(ctx, p, "simple decl and assigment from fn 4", R"(
module test;
func getx() (i32, i32) {}
func test() {
    var x, y: i32, i64;
    _, _, y = getx(), x;
}
)");

    run_test(ctx, p, "hello", R"(module main;

@extern
func putchar(c: i32) i32;

func main() i32 {
    _ = putchar(0x48);
    _ = putchar(0x65);
    _ = putchar(0x6C);
    _ = putchar(0x6C);
    _ = putchar(0x6F);
    _ = putchar(0xa);

    return 0;
}
)",
             true);

    ctx.tags.pop_back();

    fmt::println("sema tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
