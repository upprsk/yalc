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
    run_test(ctx, p, "with parameter and trailing comma", R"(module main;
func main(a: i32,) i32 {})");
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

    ctx.tags.emplace_back("binary operations");
    ctx.tags.emplace_back("arithmetic");

    run_test(ctx, p, "basic arithmetic",
             R"(module _; func _() { x + y - 8 / 2 * 2; })");
    run_test(ctx, p, "basic arithmetic 2",
             R"(module _; func _() { 10 * ( 1 + 1 ); })");
    run_test(ctx, p, "basic arithmetic 3",
             R"(module _; func _() { 10 * 10 % 2; })");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("bit");

    run_test(ctx, p, "bit operations", R"(module _; func _() { 1 << 1 * 2; })");
    run_test(ctx, p, "bit operations 2",
             R"(module _; func _() { 1 << 1 + 2; })");
    run_test(ctx, p, "bit operations 3",
             R"(module _; func _() { 0xFF00 >> 4 * (1 + 1); })");

    run_test(ctx, p, "bit operations 4",
             R"(module _; func _() { 0xFF00 | 0xF << 4 | 0xF; })");
    run_test(ctx, p, "bit operations 5",
             R"(module _; func _() { 0b00110000 | 0b0011 & 0x55; })", true);
    run_test(ctx, p, "bit operations 6",
             R"(module _; func _() { 15 & 3 | 1; })");
    run_test(ctx, p, "bit operations 7",
             R"(module _; func _() { 0x55 ^ 0xF << 4 | 0xF; })");
    run_test(ctx, p, "bit operations 8",
             R"(module _; func _() { 0x55 ^ (0xF << 4 | 0xF); })");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("comparison");

    run_test(ctx, p, "equals", R"(module _; func _() { 10 == 5 + 5; })");
    run_test(ctx, p, "not equals",
             R"(module _; func _() { 10 != 1 + 5 * 2; })");
    run_test(ctx, p, "less", R"(module _; func _() { 50 | 1 < 34 * 2; })");
    run_test(ctx, p, "less 2", R"(module _; func _() { (50 | 1) < 34 * 2; })");
    run_test(ctx, p, "less equal", R"(module _; func _() { 50 <= 34 * 2; })");
    run_test(ctx, p, "greater", R"(module _; func _() { 50 | 1 > 34 * 2; })");
    run_test(ctx, p, "greater 2",
             R"(module _; func _() { (50 | 1) > 34 * 2; })");
    run_test(ctx, p, "greater equal",
             R"(module _; func _() { 50 >= 34 * 2; })");

    ctx.tags.pop_back();
    ctx.tags.emplace_back("logic");

    run_test(ctx, p, "and",
             R"(module _; func _() { 10 + 10 < 33 and 55 > 100 / 2; })");
    run_test(ctx, p, "and/or",
             R"(module _; func _() {
    10 + 10 < 33 and 55 > 100 / 2 or magic_factor == SUPER;
})");
    run_test(ctx, p, "and/or 2",
             R"(module _; func _() {
    (10 + 10 < 33 and 55 > 100 / 2) or magic_factor == SUPER;
})");
    run_test(ctx, p, "and/or 3",
             R"(module _; func _() {
    10 + 10 < 33 and (55 > 100 / 2 or magic_factor == SUPER);
})");

    ctx.tags.pop_back();

    run_test(ctx, p, "basic cast",
             R"(module _;
func _() {
    var x = 10 as usize;
    var y = 10 as i32;
    var z = x as i32 + y;
})");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("unary operations");

    run_test(ctx, p, "neg", R"(module _; func _() { -x + 1; })");
    run_test(ctx, p, "neg 2", R"(module _; func _() { x - -1; })");
    run_test(ctx, p, "addr of",
             R"(module _; func _() { var x = 0; var y = &x; })");
    run_test(ctx, p, "addr of 2",
             R"(module _; func _() { var x = 0; var y = &x + 4; })");
    run_test(ctx, p, "not",
             R"(module _; func _() { !true and !false or ~x + 1; })");
    run_test(ctx, p, "plus", R"(module _; func _() { +x >> 1; })");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("struct type");

    run_test(ctx, p, "rather complete", R"(
module main;

def Struct = struct {
    happy:  i32,
    hungry: i32  = 10,
    yay:    bool = false
};

func main() i32 {
    var s: Struct;

    return 0;
}
)");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("pointer-ish types");

    run_test(ctx, p, "printf", R"(
module main;

@extern
func printf(fmt: [*]const u8, ...) i32;
)");

    run_test(ctx, p, "memcpy", R"(
module main;

@extern
func memcpy(dst: [*]u8, src: [*]const u8, sz: usize);
)");

    run_test(ctx, p, "struct with array type field", R"(
module test;

def macaddr_t = struct { mac: [6]u8 };
)");

    run_test(ctx, p, "function with const array param", R"(
module test;

func dump_mac(mac: [6]const u8) {
    // ...
}
)");

    run_test(ctx, p, "array literal", R"(
module test;
func test() {
    var arr = [2]i32{10, 12};
}
)");

    run_test(ctx, p, "array literal with trailing comma", R"(
module test;
func test() {
    var arr = [2]i32{10, 12,};
}
)");

    run_test(ctx, p, "array literal with inferred size", R"(
module test;
func test() {
    var arr = [_]i32{1, 2, 3, 4, 5, 6, 7, 8, 9};
}
)");

    run_test(ctx, p, "struct with slice type field", R"(
module test;

def S = struct {
    items: []const S,
};
)");

    run_test(ctx, p, "struct with slice type field 2", R"(
module test;

def S = struct {
    items: []S,
};
)");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("lit");

    run_test(ctx, p, "rather complete", R"(
module main;

def Counter = struct {
    cnt: i32 = 0,
    total: i32,
};

func main() {
    var c: Counter = .{ .total = 10 };
}
)");

    run_test(ctx, p, "init array", R"(
module test;
func test() {
    var c: [5]u8 = .{ 1, 2, 3, 4, 5 };
}
)");

    run_test(ctx, p, "init struct with array", R"(
module main;
def macaddr_t = struct { mac: [6]u8 };
func main() {
    var mac: macaddr_t = .{ .mac = .{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF} };
}
)");

    run_test(ctx, p, "init struct with array and call", R"(
module main;

def macaddr_t = struct { mac: [6]u8 };
func dump_mac(addr: macaddr_t) {
    // ...
}

func main() {
    var mac: macaddr_t = .{ .mac = .{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF} };
    dump_mac(mac);
}
)");

    run_test(ctx, p, "init struct with array in call", R"(
module main;

def macaddr_t = struct { mac: [6]u8 };
func dump_mac(addr: macaddr_t) {
    // ...
}

func main() {
    dump_mac(.{ .mac = .{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF} });
}
)");

    run_test(ctx, p, "mixed init", R"(
module test;
func test() {
    .{ .a = v0, .b = v1, .c, 0 };
}
)");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("if stmt");

    run_test(ctx, p, "minimal if", R"(
module test;
func test() {
    if false {}
}
)");

    run_test(ctx, p, "minimal if with else", R"(
module test;
func test() {
    if false {} else {}
}
)");

    run_test(ctx, p, "minimal if with else if", R"(
module test;
func test() {
    if false {} else if false {}
}
)");

    run_test(ctx, p, "minimal if with else if else", R"(
module test;
func test() {
    if false {} else if false {} else {}
}
)");

    run_test(ctx, p, "if with else", R"(
module test;
func test() {
    if false {
        var x = 1;
    } else {
        var x = 2;
    }
}
)");

    run_test(ctx, p, "if with else if", R"(
module test;
func test() {
    if false {
        var x = 1;
    } else if false {
        var x = 2;
    }
}
)");

    run_test(ctx, p, "if with else if else", R"(
module test;
func test() {
    if false {
        var x = 1;
    } else if false {
        var x = 2;
    } else {
        var x = 3;
    }
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

    ctx.tags.pop_back();

    ctx.tags.emplace_back("calls");

    run_test(ctx, p, "minimal", R"(
module test;
func test() {
    var x = f();
}

func f() i32 { return 42; }
)");

    run_test(ctx, p, "with args", R"(
module test;
func test() {
    var x = add(1, 2);
}

func add(x: i32, y: i32) i32 { return x + y; }
)");

    run_test(ctx, p, "with args and trailing comma", R"(
module test;
func test() {
    var x = add(1, 2);
}

func add(x: i32, y: i32) i32 { return x + y; }
)");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("fields");

    run_test(ctx, p, "field access", R"(
module main;

def Counter = struct { cnt: i32 = 0, total: i32 };
func main() {
    var c: Counter = .{ .total = 10 };
    c_printf("c.cnt=%d, c.total=%d\n", c.cnt, c.total);
}

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);
)");

    run_test(ctx, p, "a method", R"(
module main;

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);

def num = i32;
func num.print(n: num) {
    c_printf("%d", n);
}

func main() {
    var n = 0 as num;
    n.print();
    c_printf("\n");
}
)");

    ctx.tags.pop_back();

    ctx.tags.emplace_back("break/continue");

    run_test(ctx, p, "break", R"(module test;
func test() { break; })");
    run_test(ctx, p, "continue", R"(module test;
func test() { continue; })");

    ctx.tags.pop_back();

    run_test(ctx, p, "hello world libc", R"(
module main;

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);

func main() {
    c_printf("Hello, %s!\n", "World");
}
)");

    run_test(ctx, p, "hello world libc with shbang", R"(
#!/usr/local/bin/yalc run
module main;

@extern(link_name="printf")
func c_printf(fmt: [*]const u8, ...);

func main() {
    c_printf("Hello, %s!\n", "World");
}
)");

    fmt::println("parser tests, {} tests, {} success, {} failed", ctx.total(),
                 ctx.ok, ctx.failed);
    return {ctx.ok, ctx.failed};
}
