#include "test_sema.hpp"

#include <vector>

#include "error_reporter.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;
namespace rv = std::ranges::views;

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

auto gen_ast_typed_many(std::vector<std::string> sources) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    std::set<yal::FileId> filenames;
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

    json j = *prj_root;
    j["ds"] = *ast.get_decl_store();

    return j;
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    run_checks_for_test(ctx, p, name, skip,
                        [&]() { return gen_ast_typed(source); });
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::vector<std::string> sources, bool skip = false) {
    run_checks_for_test(ctx, p, name, skip,
                        [&]() { return gen_ast_typed_many(sources); });
}

auto test_sema(TestParams const& p) -> std::tuple<int, int, int> {
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

    run_test(ctx, p, "extern with body", R"(module main;
@extern
func extern_func() {}
)");

    run_test(ctx, p, "extern with body 2", R"(module main;
@extern(link_name="yay")
func extern_func() {}
)");

    run_test(ctx, p, "not extern without body", R"(module main;
func extern_func();
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("coercion");

    run_test(ctx, p, "lit to i8", R"(module main;

func f(x: i8) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to u8", R"(module main;

func f(x: u8) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to i16", R"(module main;

func f(x: i16) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to u16", R"(module main;

func f(x: u16) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to i32", R"(module main;

func f(x: i32) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to u32", R"(module main;

func f(x: u32) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to i64", R"(module main;

func f(x: i64) {}
func main() {
    f(13);
})");

    run_test(ctx, p, "lit to u64", R"(module main;

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

    run_test(ctx, p, "various decls with int", R"(module main;
func main() i32 {
    var a: u32 = 32 + 1;
    var b: u64 = 10 + a;
    var c      = b + a + 1;
    var d, e   = 10, 10 + 10;
    var f      = d + e;

    return 0;
}
)");

    run_test(ctx, p, "various decls with int 2", R"(module main;
func test(x: i32) {}
func main() {
    var a: u32 = 32 + 1;
    var b: u64 = 10 + a;
    var c      = b + a + 1;
    var d, e   = 10, 10 + 10;
    var f      = d + e;

    test(a);
    test(b);
    test(c);
    test(d);
    test(e);
    test(f);
}
)");

    run_test(ctx, p, "various decls with int 3", R"(module main;
func test(x: u64) {}
func main() {
    var a: u32 = 32 + 1;
    var b: u64 = 10 + a;
    var c      = b + a + 1;
    var d, e   = 10, 10 + 10;
    var f      = d + e;

    test(a);
    test(b);
    test(c);
    test(d);
    test(e);
    test(f);
}
)");

    run_test(ctx, p, "various decls with int 4", R"(
module test;
func test() {
    var a = 10 == 01;
    var b = 34 < 10 + 1;
    var c = a != b;
}
)");

    run_test(ctx, p, "various decls with int 5", R"(
module test;
func test() {
    f(10 + 2 - 4);
    g(10 + 2 - 4);
    h(12 < 1 * 4, 4 == 33 + (100 / 2));
}
func f(x: i32) {}
func g(x: u32) {}
func h(a: bool, b: bool) {}
)");

    run_test(ctx, p, "various decls with int 6", R"(
module test;
func test() {
    f(10 + 2 - 4);
    g(10 + 2 - 4);

    var a: u16 = 4;
    h(12 < 1 * 4, a == 33 + (100 / 2));
}
func f(x: i32) {}
func g(x: u32) {}
func h(a: bool, b: bool) {}
)");

    run_test(ctx, p, "wrong parameters", R"(
module test;
func test() {
    h(f(12), g(4), 12);
}
func f(x: i32) {}
func g(x: u32) {}
func h(a: bool, b: bool) {}
)");

    run_test(ctx, p, "assignments", R"(
module main;
func swap(x: i32, y: u32) (u32, i32) {}

func main() {
    var x, y = 0, 1;
    var z, w = swap(2, 3);

    w = 20;
    z, x = swap(4, 5);
}
)");

    run_test(ctx, p, "discard assign", R"(
 module test;
func test() {
    _ = 10 + 10 - 5;
})");

    run_test(ctx, p, "multiple assign", R"(
 module test;
func test() {
    var a, b = 11, 12;
    a, b = 9, 10;
})");

    run_test(ctx, p, "multiple assign 2", R"(
module test;
func test() {
    var a, b = 11, 12;
    a, b = f();
}

func f() (i32, u32) {}
)");

    run_test(ctx, p, "multiple assign 3", R"(
module test;
func test() {
    var a: i8;
    a, _ = f();
}

func f() (i32, u32) {}
)");

    run_test(ctx, p, "multiple assign 4", R"(
module test;
func test() {
    var a: i32;
    a, _ = f();
}

func f() (i32, u32) {}
)");

    run_test(ctx, p, "multiple assign 5", R"(
module test;
func test() {
    var a: i32;
    a, _ = f();
}

func f() (i8, u32) {}
)");

    run_test(ctx, p, "missing types",
             R"(
module test;
func test() {
    var a;
}
)");

    run_test(ctx, p, "missing types 2",
             R"(
module test;
func test() {
    var a, b;
}
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

    ctx.tags.pop_back();
    ctx.tags.emplace_back("pointers");

    run_test(ctx, p, "take address of local",
             R"(
module test;
func test() {
    var x = 0;
    var y = &x;
}
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
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("casting");

    run_test(ctx, p, "cast untyped to u8",
             R"(
module test;
func test() {
    var a = 10 as u8;
}
)");

    run_test(ctx, p, "many casts on integers and pointers",
             R"(
module test;
func test() {
    var a = 10;
    var b = a as u64;
    var c = b as u8;

    var d: [*]u8;
    var e = d as usize as u32;
    var f = e as usize as [*]const u16;

    var g: *u32;
    var h = g as *u16;
    var i = (g as usize + 4) as [*]u32;
}
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("while stmt");

    run_test(ctx, p, "minimal", R"(
module test;
func test() {
    while true {
        var x = 10;
    }
}
)");

    run_test(ctx, p, "kinda-for fixed-args", R"(
module test;
func test() {
    var count = 10;
    var i     = 0;

    while i < count {
        printf("i=%d\n".ptr, i);

        i = i + 1;
    }
}

@extern
func printf(fmt: [*]const u8, arg0: i32);
)");

    run_test(ctx, p, "kinda-for var-args", R"(
module test;
func test() {
    var count = 10;
    var i     = 0;

    while i < count {
        printf("i=%d\n".ptr, i);

        i = i + 1;
    }
}

@extern
func printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "kinda-for fails", R"(
module test;
func test() {
    var count = 10;
    var i     = 0;

    while i < count {
        printf("i=%d\n".ptr, i);

        i = i + 1;
    }
}
)");

    run_test(ctx, p, "kinda-for wrong args", R"(
module test;
func test() {
    var count = 10;
    var i     = 0;

    while i < count {
        printf("i=%d\n", i);

        i = i + 1;
    }
}

@extern
func printf(fmt: [*]const u8, arg0: i32);
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("if stmt");

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

    run_test(ctx, p, "if statement with else",
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

    ctx.tags.pop_back();
    ctx.tags.emplace_back("struct");

    run_test(ctx, p, "minimal", R"(
module test;
def S = struct { a: i32 };
)");

    run_test(ctx, p, "minimal 2", R"(
module test;
def S = struct { a: i32, };
)");

    run_test(ctx, p, "instantiate", R"(
module test;
def S = struct { a: i32 };
func test() {
    var s: S = .{.a=0};
}
)");

    run_test(ctx, p, "instantiate extra fields", R"(
module test;
def S = struct { a: i32 };
func test() {
    var s: S = .{.a=0, .b=0};
}
)");

    run_test(ctx, p, "instantiate missing fields", R"(
module test;
def S = struct { a: i32 };
func test() {
    var s: S = .{};
}
)");

    run_test(ctx, p, "instantiate wrong fields", R"(
module test;
def S = struct { a: i32 };
func test() {
    var s: S = .{.b=0};
}
)");

    run_test(ctx, p, "get field", R"(
module test;
def S = struct { a: i32 };
func test() {
    var s: S = .{.a=0};
    _ = s.a;
}
)");

    run_test(ctx, p, "get wrong field", R"(
module test;
def S = struct { a: i32 };
func test() {
    var s: S = .{.a=0};
    _ = s.b;
}
)");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("methods and coercion");

    run_test(ctx, p, "requires value and gets value", R"(
module main;

def S = struct { a: u32 };
func S.print(s: S) {}
func S.print_ptr(s: *S) {}

func main(argc: i32, argv: [*][*]u8) i32 {
    var s: S = .{ .a = 0 };
    s.print();

    return 0;
}
)");

    run_test(ctx, p, "requires value and gets mut pointer", R"(
module main;

def S = struct { a: u32 };
func S.print(s: S) {}
func S.print_ptr(s: *S) {}

func main(argc: i32, argv: [*][*]u8) i32 {
    var s: S = .{ .a = 0 };
    var sp = &s;
    sp.print();

    return 0;
}
)",
             true);

    run_test(ctx, p, "requires const pointer and gets value", R"(
module main;

def S = struct { a: u32 };
func S.print(s: S) {}
func S.print_ptr(s: *S) {}

func main(argc: i32, argv: [*][*]u8) i32 {
    var s: S = .{ .a = 0 };
    s.print_ptr();

    return 0;
}
)",
             true);

    run_test(ctx, p, "requires const pointer and gets mut pointer", R"(
module main;

def S = struct { a: u32 };
func S.print(s: S) {}
func S.print_ptr(s: *S) {}

func main(argc: i32, argv: [*][*]u8) i32 {
    var s: S = .{ .a = 0 };
    var sp = &s;
    sp.print_ptr();

    return 0;
}
)",
             true);

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

    run_test(ctx, p, "indexing argv",
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

    run_test(ctx, p, "assign to mptr",
             std::vector<std::string>{
                 R"(
module main;

func main(argc: i32, argv: [*][*]u8) i32 {
    var n: usize = 4;
    var arr = c_malloc(10 * n) as [*]i32;
    defer c_free(arr);

    var i = 0;
    while i < n {
        defer i = i + 1;

        arr[i] = i * 2;
    }

    var i = 0;
    while i < n {
        defer i = i + 1;

        c_printf("arr[%d] = %d\n".ptr, i, arr[i]);
    }

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

    ctx.tags.pop_back();

    print_test_results("sema", ctx);
    return {ctx.ok, ctx.failed, ctx.skipped};
}
