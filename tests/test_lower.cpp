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
namespace rv = std::ranges::views;

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

auto gen_ast_lowered_many(std::vector<std::string> sources) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    std::unordered_set<yal::FileId> filenames;
    for (auto const& [idx, source] : rv::enumerate(sources)) {
        filenames.insert(fs.add(fmt::format(":memory-{}:", idx), source));
    }

    (void)fs.add_dir(":memory-dir:", filenames);

    auto root = fs.add(":memory-0:");
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);

    auto [ast, ast_root] = yal::parse(tokens, fer);
    er.update_error_count(fer);

    auto ts = yal::types::TypeStore{};
    auto prj_root = yal::resolve_names(ast, ast_root, er, fs, ts, {});

    yal::sema::sema_ast(ast, prj_root, er, ts, {});
    ms.flush();

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::lower::lower_ast(ast, prj_root, er, ts, {});
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
    run_checks_for_test(ctx, p, name, skip,
                        [&]() { return gen_ast_lowered(source); });
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::vector<std::string> sources, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name, skip,
                        [&]() { return gen_ast_lowered_many(sources); });
}

auto test_lower(TestParams const& p) -> std::tuple<int, int, int> {
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

    run_test(ctx, p, "simple decl 1", R"(
module test;
func getx() i32 {}
func test() {
    var x = getx();
}
)");

    run_test(ctx, p, "simple decl 2", R"(
module test;
func getx() (i32, bool) {}
func test() {
    var x, _ = getx();
}
)");

    run_test(ctx, p, "simple decl 3", R"(
module test;
func getx() (i32, bool) {}
func gety() i64 {}
func test() {
    var x, _, y = getx(), gety();
}
)");

    run_test(ctx, p, "simple decl 4", R"(
module test;
func test() {
    var _, x = 0 - 1, 0 + 1;
}
)");

    run_test(ctx, p, "hello", R"(module main;

@extern
func putchar(c: i32) i32;

@export
func main() i32 {
    _ = putchar(0x48); // 'H'
    _ = putchar(0x65); // 'e'
    _ = putchar(0x6C); // 'l'
    _ = putchar(0x6C); // 'l'
    _ = putchar(0x6F); // 'o'
    _ = putchar(0x2C); // ','
    _ = putchar(0x20); // ' '
    _ = putchar(0x57); // 'W'
    _ = putchar(0x6F); // 'o'
    _ = putchar(0x72); // 'r'
    _ = putchar(0x6C); // 'l'
    _ = putchar(0x64); // 'd'
    _ = putchar(0x21); // '!'
    _ = putchar(0xa);

    return 0;
}
)");

    run_test(ctx, p, "hello string", R"(
module main;

@export
func main() {
    var v = "Hello, World!".ptr;
    c_print_cstr(v);
    return;
}

@extern(link_name="print_int")
func c_print_int(v: i32);

@extern(link_name="print_str")
func c_print_str(s: [*]const u8, len: i32);

@extern(link_name="print_cstr")
func c_print_cstr(s: [*]const u8);
)");

    run_test(ctx, p, "hello method",
             std::vector<std::string>{
                 R"(
module main;

def cstr_t = [*]const u8;
func cstr_t.print(s: cstr_t) {
    c_print_cstr(s as [*]const u8);
    return;
}

@export
func main() {
    var v = "Hello, World!".ptr as cstr_t;
    v.print();
    return;
}
)",
                 R"(
module main;

@extern(link_name="print_int")
func c_print_int(v: i32);

@extern(link_name="print_str")
func c_print_str(s: [*]const u8, len: i32);

@extern(link_name="print_cstr")
func c_print_cstr(s: [*]const u8);
)",
             });

    ctx.tags.pop_back();

    print_test_results("lower", ctx);
    return {ctx.ok, ctx.failed, ctx.skipped};
}
