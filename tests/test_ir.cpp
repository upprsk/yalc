#include "error_reporter.hpp"
#include "ir-build.hpp"
#include "ir.hpp"
#include "lower.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "test_lower.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;
namespace rv = std::ranges::views;

auto gen_ast_ir(std::string source) -> json {
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

    auto module = yal::ir::build(ast, prj_root, er, ts, {.single_file = true});
    if (er.had_error()) return 1;

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::ir::disasm_module(ms.f, module);
    ms.flush();

    return json{
        {"stdout", ms.str()}
    };
}

auto gen_ast_ir_many(std::vector<std::string> sources) -> json {
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

    auto module = yal::ir::build(ast, prj_root, er, ts, {});
    if (er.had_error()) return 1;

    if (er.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    yal::ir::disasm_module(ms.f, module);
    ms.flush();

    return json{
        {"stdout", ms.str()}
    };
}
static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name, [&]() { return gen_ast_ir(source); });
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::vector<std::string> source, bool skip = false) {
    if (skip) {
        fmt::print(fmt::bg(fmt::color::orange), "SKIP");
        fmt::println(" '{}' skipped", name);
        return;
    }

    run_checks_for_test(ctx, p, name,
                        [&]() { return gen_ast_ir_many(source); });
}

auto test_ir(TestParams const& p) -> std::pair<int, int> {
    Context ctx{.tags = {"ir"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    ctx.tags.emplace_back("minimal");

    run_test(ctx, p, "function call and return",
             R"(
module main;

func id(x: i32) i32 {
    return x;
}

@export
func main() i32 {
    var x = 68;
    return id(x);
}
)");

    run_test(ctx, p, "many calls and basic arithmetic",
             R"(
module main;

func add(x: i32, y: i32) i32 { return x + y; }
func sub(x: i32, y: i32) i32 { return x - y; }
func div(x: i32, y: i32) i32 { return x / y; }
func mul(x: i32, y: i32) i32 { return x * y; }

@export
func main() i32 {
    var x = 42;
    var y = 69;
    return div(
        add(x, sub(y, 1)),
        mul(2, 1),
    );
}
)");

    run_test(ctx, p, "print_int very simple",
             std::vector<std::string>{
                 R"(
module main;

@export
func main() i32 {
    c_print_int(69);
    return 0;
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

@extern(link_name="getchar")
func c_getchar() i32;
)",
             });

    run_test(ctx, p, "print_int in inner function very simple",
             std::vector<std::string>{
                 R"(
module main;

func print_answer() {
    c_print_int(42);
}

@export
func main() i32 {
    print_answer();
    return 0;
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

@extern(link_name="getchar")
func c_getchar() i32;
)",
             });

    ctx.tags.pop_back();
    ctx.tags.emplace_back("defer");

    run_test(ctx, p, "malloc and free",
             std::vector<std::string>{
                 R"(
module main;

func thingy() {
    c_print_int(42);
    defer c_print_int(420);

    c_print_int(69);
}

@export
func main() i32 {
    var buf = c_malloc(256);
    defer {
        c_print_int(2);
        c_free(buf);
    }

    c_print_int(0);
    defer c_print_int(1);

    {
        thingy();

        return 0;
    }
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

@extern(link_name="getchar")
func c_getchar() i32;

@extern(link_name="malloc")
func c_malloc(size: usize) [*]u8;

@extern(link_name="free")
func c_free(ptr: [*]u8);
)",
             });

    ctx.tags.pop_back();
    ctx.tags.emplace_back("big examples");

    run_test(ctx, p, "indexing",
             std::vector<std::string>{
                 R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    c_printf("Hello, World! (%s)\n".ptr, argv[0]);
    return 0;
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

@extern(link_name="getchar")
func c_getchar() i32;


@extern(link_name="malloc")
func c_malloc(size: usize) [*]u8;

@extern(link_name="free")
func c_free(ptr: [*]u8);

@extern(link_name="exit")
func c_exit(code: i32);

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)",
             });

    run_test(ctx, p, "if statement without else",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    if argc != 2 {
        c_printf("usage: %s <name>\n".ptr, argv[0]);
        return 1;
    }

    c_printf("Hello, %s!\n".ptr, argv[1]);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "if statement with else else",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var i = 1;
    while i < argc {
        defer i = i + 1;
        // c_printf("# %d: %s\n".ptr, i, argv[i]);

        if streq(argv[i], "--help".ptr) {
            c_printf("usage: %s [options]\n".ptr, argv[0]);
            c_printf("options:\n".ptr);
            c_printf("    --help: show this message\n".ptr);
        }

        else {
            c_printf("error: unknown option '%s'\n".ptr, argv[i]);
        }
    }

    return 0;
}

func streq(lhs: [*]const u8, rhs: [*]const u8) bool {
    return c_strcmp(lhs, rhs) == 0;
}

@extern(link_name="strcmp")
func c_strcmp(a: [*]const u8, b: [*]const u8) i32;

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "if statement without else 2",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    if argc != 2 {
        c_printf("usage: %s <name>\n".ptr, argv[0]);

        if argc < 2 {
            c_printf("note: missing required argument\n".ptr);
        }

        if argc > 2 {
            c_printf("note: found %d extra arguments\n".ptr, argc - 2);
        }

        return 1;
    }

    c_printf("Hello, %s!\n".ptr, argv[1]);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("assign");

    run_test(ctx, p, "simple assign",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var i = 0;
    c_printf("i=%d\n".ptr, i);
    i = i + 1;
    c_printf("i=%d\n".ptr, i);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "while statement",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var i = 0;

    while i < 10 {
        c_printf("i=%d\n".ptr, i);
        i = i + 1;
    }

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "while statement 2",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var i = 0;

    while i < 10 {
        defer i = i + 1;
        c_printf("i=%d\n".ptr, i);
    }

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "while statement 3",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var i = 0;
    while i < argc {
        defer i = i + 1;
        c_printf("- %d: %s\n".ptr, i, argv[i]);
    }

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "assign to pointer",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var x = 0;
    var y = &x;
    c_printf("x=%d, y=%d\n".ptr, x, y.*);

    x = 10;
    c_printf("x=%d, y=%d\n".ptr, x, y.*);

    y.* = 35 + 34;
    c_printf("x=%d, y=%d\n".ptr, x, y.*);

    return y.*;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "assign to pointer 2",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var x = 0;
    var y = &x;
    var z = y;
    c_printf("x=%d, y=%d, z=%d\n".ptr, x, y.*, z.*);

    x = 10;
    c_printf("x=%d, y=%d, z=%d\n".ptr, x, y.*, z.*);

    y.* = 35 + 34;
    c_printf("x=%d, y=%d, z=%d\n".ptr, x, y.*, z.*);

    z.* = 11;
    c_printf("x=%d, y=%d, z=%d\n".ptr, x, y.*, z.*);

    return y.*;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "assign to pointer 3",
             R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var x = 33 + 36;
    var y = 400 + 20;

    var p = &x;
    c_printf("x=%d, y=%d, p=%d @ %p\n".ptr, x, y, p.*, p);

    p = &y;
    c_printf("x=%d, y=%d, p=%d @ %p\n".ptr, x, y, p.*, p);

    p.* = 42;
    c_printf("x=%d, y=%d, p=%d @ %p\n".ptr, x, y, p.*, p);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "swap integers",
             R"(
module main;

func swap(lhs: *i32, rhs: *i32) {
    var tmp = lhs.*;
    lhs.* = rhs.*;
    rhs.* = tmp;
}

func main(argc: i32, argv: [*][*]u8) i32 {
    var x = 33 + 36;
    var y = 400 + 20;

    c_printf("x=%d, y=%d\n".ptr, x, y);
    swap(&x, &y);

    c_printf("x=%d, y=%d\n".ptr, x, y);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    // FIXME: the swap set does not work, as each part of the assigment is
    // processed individually
    run_test(ctx, p, "swap integers 2",
             R"(
module main;

func swap(lhs: *i32, rhs: *i32) {
    lhs.*, rhs.* = rhs.*, lhs.*;
}

func main(argc: i32, argv: [*][*]u8) i32 {
    var x = 33 + 36;
    var y = 400 + 20;

    c_printf("x=%d, y=%d\n".ptr, x, y);
    swap(&x, &y);

    c_printf("x=%d, y=%d\n".ptr, x, y);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)",
             true);

    ctx.tags.pop_back();

    fmt::println("ir tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
