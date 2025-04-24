#include "test_sema.hpp"

#include "error_reporter.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;

auto gen_ast_typed(std::string source) -> json {
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

    run_checks_for_test(ctx, p, name, [&]() { return gen_ast_typed(source); });
}

auto test_sema(TestParams const& p) -> std::pair<int, int> {
    Context ctx{.tags = {"sema"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    run_test(ctx, p, "empty file", "");
    run_test(ctx, p, "minimal", "module main;");

    run_test(ctx, p, "minimal main", R"(module main;

func main() i32 {
    return 0;
})");

    ctx.tags.emplace_back("general");

    run_test(ctx, p, "var decl 1", R"(module main;

func main() {
    var x = 12;
})");

    run_test(ctx, p, "var decl 2", R"(module main;
func main() {
    var x: u8, u8 = 0xDe,0xad;
}
)");

    run_test(ctx, p, "var decl 3", R"(module main;
func main() {
    var x: u32;
}
)");

    run_test(ctx, p, "var decl 4", R"(module main;
func main() {
    var x, y: u32, u32;
}
)");

    run_test(ctx, p, "var decl 5", R"(module main;
func main() {
    var a = 0xFF;
    var x = 0xDe, a;
}
)");

    run_test(ctx, p, "var decl 6", R"(module main;
func main() {
    var a: i32, u8;
}
)");

    run_test(ctx, p, "var decl, missing init 1", R"(module main;

func main() {
    var x, y = 12;
})");

    run_test(ctx, p, "var decl, missing init 2", R"(module main;

func main() {
    var x, _ = 12;
})");

    run_test(ctx, p, "var decl, extra init 1", R"(module main;

func main() {
    var x = 12, 13;
})");

    run_test(ctx, p, "var decl, missing type", R"(module main;
func main() {
    var x, y: u32;
}
)");

    run_test(ctx, p, "simple while", R"(module main;
func main() {
    var i: i32;
    while i < 10 {
        i = i + 1;
    }
}
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("coercion");

    run_test(ctx, p, "i32 to i8", R"(module main;

func f(x: i8) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to u8", R"(module main;

func f(x: u8) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to i16", R"(module main;

func f(x: i16) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to u16", R"(module main;

func f(x: u16) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to i32", R"(module main;

func f(x: i32) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to u32", R"(module main;

func f(x: u32) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to i64", R"(module main;

func f(x: i64) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "i32 to u64", R"(module main;

func f(x: u64) {}
func main() {
    f(13);
}
)");

    run_test(ctx, p, "in var-decl, i32 to u64", R"(module main;
func main() {
    var x      = 12;
    var y: i64 = x;
}
)");

    run_test(ctx, p, "in var-decl 1", R"(module main;
func main() {
    var x, y: i32, u32 = 0xDe,0xad;
}
)");

    run_test(ctx, p, "multiple function returns 1", R"(module main;
func check(num: i32) (i32, bool) {}
func main() i32 {
    var r, ok = check(12);
    return 0;
}
)");

    run_test(ctx, p, "multiple function returns 2", R"(module main;
func check(num: i32) (i32, bool) {}
func main() i32 {
    var r, ok: u32, u8 = check(12);
    return 0;
}
)");

    run_test(ctx, p, "multiple function returns 3", R"(module main;
func check(num: i32) (i64, bool) {
    return 0, false;
}

func main() i32 {
    var r, ok = check(12);
    return 0;
}
)");

    run_test(ctx, p, "multiple function returns 4", R"(module main;
func check(num: i32) (i64, bool) {
    return num, false;
}

func main() i32 {
    var r, ok = check(12);
    return 0;
}
)");

    run_test(ctx, p, "multiple function returns 5", R"(module main;
func check(num: i32) (i64, bool) {
    return num, false;
}

func main() i32 {
    var r, ok, x = check(12), 69;
    return 0;
}
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("examples");

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
)");

    ctx.tags.pop_back();

    fmt::println("sema tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
