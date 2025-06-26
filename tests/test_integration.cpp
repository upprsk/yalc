#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <any>
#include <libassert/assert.hpp>
#include <nlohmann/json.hpp>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "file_store.hpp"
#include "parser.hpp"
#include "tests.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

namespace rv = std::ranges::views;

namespace yal::tests {
// NOLINTBEGIN(readability-function-cognitive-complexity)
// NOLINTBEGIN(readability-function-size)

auto tokenizer() -> ut::Test {
    auto tb = ut::group("tokenizer");

    std::vector<std::pair<std::string, std::string>> tests{
        {         "empty file",                                                         ""},
        {            "minimal",                                             "module main;"},
        {             "equals",                                                  "= == =>"},
        {               "less",                                              "< << <<= <="},
        {            "greater",                                              "> >> >= >>="},
        {               "plus",                                                  "+ ++ +="},
        {              "minus",                                                  "- -- -="},
        {               "star",                                                     "* *="},
        {              "slash",                                                     "/ /="},
        {               "bang",                                                     "! !="},
        {            "percent",                                                     "% %="},
        {          "ampersand",                                                     "& &="},
        {               "pipe",                                                     "| |="},
        {             "carrot",                                                     "^ ^="},
        {              "tilde",                                                     "~ ~="},
        {        "punctuation",                                "; : , . .. ... .* .= .{ ?"},
        {             "parens",                                              "( ) { } [ ]"},

        {        "identifiers",               "hello test _ _one\ntWo_tHREe YAY abc123 _1"},
        {          "decorator",                     "@test @_124 @123 @testing_this_THING"},
        {            "numbers", "1234567890 123_456_789 00 0xFF 0xbaba 2145 1.4 3.14 1.99"},

        {             "string",    R"~~("this is a string!" "a string \"with quotes\"")~~"},
        {               "char",                           R"~~('a' 'b' 'c' '\'' '\xF0')~~"},

        {            "comment",                                     "// this is a comment"},

        {"unterminated string",                              R"~~("unterminated string)~~"},
        {            "int dot",                                              R"~~(123.)~~"},
        {        "int methods",                                        R"~~(123.test())~~"},

        {             "shbang",                                   R"(#!/usr/local/bin/yalc
yay!)"},

        {"unbalanced parens 1",                                                    "(({))"},
        {"unbalanced parens 2",                                                  "(({[}))"},
        {"unbalanced parens 3",                                                  "([{[}))"},
    };

    for (auto&& [name, input] : tests) {
        ut::add(tb, name,
                [name = name, input = std::move(input)](
                    ut::Context const& ctx) -> nlohmann::json {
                    auto fs = yal::FileStore{};
                    auto er = yal::ErrorReporter{&fs, ctx.out};

                    auto fileid = fs.add_file_and_contents(
                        fmt::format(":memory:{}", name), input);

                    return tokenize(er.for_file(fileid));
                });
    }

    return tb;
}

auto parser() -> ut::Test {
    auto tb = ut::group("parser");

    std::vector<std::pair<std::string, std::string>> tests{
        {                           "empty file",""                                                 },

        {                          "module decl",    "module main;"},
        {                "invalid module decl 1",             "mod"},
        {                "invalid module decl 2",          "module"},
        {                "invalid module decl 3",      "module 134"},
        {                "invalid module decl 4",     "module 134;"},
        {                "invalid module decl 5",     "modul main;"},
        {                "invalid module decl 6",  "module 'main';"},

        {                   "invalid keywords 1", R"~~(module main;
v
)~~"},
        {                   "invalid keywords 2", R"~~(module main;
v
va
)~~"},
        {                   "invalid keywords 3", R"~~(module main;
fun
)~~"},
        {                   "invalid keywords 4", R"~~(module main;
fuc
)~~"},
        {                   "invalid keywords 5", R"~~(module main;
fuc ar
)~~"},
        {                   "invalid keywords 6", R"~~(module main;
va fun
)~~"},
        {                   "invalid keywords 7", R"~~(module main;
va fuc
)~~"},

        {                            "globals 1", R"~~(module main;
var a;
var b: usize;
var c = 42;
var d: usize = 42;

var x, y, z = 0, 1, 2;
var i, j: usize;
var k, l: usize, i32 = 12, 3;
)~~"},
        {                    "invalid globals 1", R"~~(module main;

var c

var a,: i32,  10
var b: = 

var global: usize;)~~"},
        {                    "invalid globals 2", R"~~(module main;

var 
)~~"},
        {                    "invalid globals 3", R"~~(module main;

var global
)~~"},
        {                    "invalid globals 4", R"~~(module main;

var global:
)~~"},
        {                    "invalid globals 5", R"~~(module main;

var global: i32
)~~"},
        {                    "invalid globals 6", R"~~(module main;
var
var good: i32;
)~~"},
        {                    "invalid globals 7", R"~~(module main;
var global
var good: i32;
)~~"},
        {                    "invalid globals 8", R"~~(module main;
var global: usize
var good: i32;
)~~"},
        {                    "invalid globals 9", R"~~(module main;
var global: usize =
var good: i32;
)~~"},
        {                   "invalid globals 10", R"~~(module main;
var global: usize = 10
var good: i32;
)~~"},
        {                   "invalid globals 11", R"~~(module main;
var global =
var good: i32;
)~~"},
        {                   "invalid globals 12", R"~~(module main;
var global = 10
var good: i32;
)~~"},
        {                            "globals 2", R"~~(module main;
var global: usize = 10;
var good: i32;
)~~"},
        {                            "globals 3", R"~~(module main;
var global = 10;
var good: i32;
)~~"},

        {                               "defs 1", R"~~(module main;
def TEN = 10;
var global = TEN;
)~~"},
        {                       "invalid defs 1", R"~~(module main;
def
var global = 10;
)~~"},
        {                       "invalid defs 2", R"~~(module main;
def TEN
var global = 10;
)~~"},
        {                       "invalid defs 3", R"~~(module main;
def TEN =
var global = 10;
)~~"},
        {                       "invalid defs 4", R"~~(module main;
def TEN = 10
var global = TEN;
)~~"},

        {             "global with attributes 1", R"~~(module main;
@magic(123, priority=10)
var thing = 10;
)~~"},
        {             "global with attributes 2", R"~~(module main;
@test(thing=12,)
var global;
)~~"},
        {             "global with attributes 3", R"~~(module main;
@test_1(12,)
@test_2
var global;
)~~"},

        {     "invalid global with attributes 1", R"~~(module main;
@
var thing = 10;
)~~"},
        {     "invalid global with attributes 2", R"~~(module main;
@
)~~"},
        {     "invalid global with attributes 3", R"~~(module main;
@
var
)~~"},
        {     "invalid global with attributes 4", R"~~(module main;
@test(
var global;
)~~"},
        {     "invalid global with attributes 5", R"~~(module main;
@test(
var global;
)~~"},
        {     "invalid global with attributes 6", R"~~(module main;
@test(thing
var global;
)~~"},
        {     "invalid global with attributes 7", R"~~(module main;
@test(thing=
var global;
)~~"},
        {     "invalid global with attributes 8", R"~~(module main;
@test(thing=12
var global;
)~~"},
        {     "invalid global with attributes 9", R"~~(module main;
@test(thing=12,
var global;
)~~"},
        {    "invalid global with attributes 10", R"~~(module main;
@test(thing=)
var global;
)~~"},
        {    "invalid global with attributes 11", R"~~(module main;
@test(thing=,)
var global;
)~~"},
        {    "invalid global with attributes 12", R"~~(module main;
@test_1(
@test_2
var global;
)~~"},
        {    "invalid global with attributes 13", R"~~(module main;
@test_1(
@test_2
var global;
)~~"},
        {    "invalid global with attributes 14", R"~~(module main;
@test_1(12
@test_2
var global;
)~~"},
        {    "invalid global with attributes 15", R"~~(module main;
@test_1(12,
@test_2
var global;
)~~"},
        {    "invalid global with attributes 16", R"~~(module main;
@test_1(=12,))
@test_2
var global;
)~~"},

        {                      "lone function 1", R"~~(module main;
func f();
)~~"},
        {                      "lone function 2", R"~~(module main;
func f() {}
)~~"},
        {                      "lone function 3", R"~~(module main;
func f(x) {}
)~~"},
        {                      "lone function 4", R"~~(module main;
func f(x: i32) {}
)~~"},
        {                      "lone function 5", R"~~(module main;
func f(x, y) {}
)~~"},
        {                      "lone function 6", R"~~(module main;
func f(x, y: i32) {}
)~~"},
        {                      "lone function 7", R"~~(module main;
func f(x, y: i32,) {}
)~~"},
        {                      "lone function 8", R"~~(module main;
func f(x: i32, y: i32) {}
)~~"},
        {                      "lone function 9", R"~~(module main;
func f() i32 {}
)~~"},
        {                     "lone function 10", R"~~(module main;
func f() (i32) {}
)~~"},
        {                     "lone function 11", R"~~(module main;
func f() (i32, i32) {}
)~~"},
        {                     "lone function 12", R"~~(module main;
func f() i32;
)~~"},
        {                     "lone function 13", R"~~(module main;
@export
func f();
)~~"},
        {                     "lone function 14",
         R"~~(module main;
@export
@gc
func f();
)~~"                                         },

        {            "lone function - invalid 1", R"~~(module main;
func
)~~"},
        {            "lone function - invalid 2", R"~~(module main;
func f
)~~"},
        {            "lone function - invalid 3", R"~~(module main;
func f(
)~~"},
        {            "lone function - invalid 4", R"~~(module main;
func f()
)~~"},
        {            "lone function - invalid 5", R"~~(module main;
func f() (
)~~"},
        {            "lone function - invalid 6", R"~~(module main;
func f() ()
)~~"},
        {            "lone function - invalid 7", R"~~(module main;
func f() {
)~~"},
        {            "lone function - invalid 8", R"~~(module main;
func f() ({
)~~"},
        {            "lone function - invalid 9", R"~~(module main;
func f.
)~~"},
        {           "lone function - invalid 10", R"~~(module main;
func f.(
)~~"},
        {           "lone function - invalid 11", R"~~(module main;
func f.fn(
)~~"},
        {           "lone function - invalid 12", R"~~(module main;
func f.fn()
)~~"},
        {           "lone function - invalid 13", R"~~(module main;
func .f
)~~"},
        {           "lone function - invalid 14", R"~~(module main;
func .f(
)~~"},
        {           "lone function - invalid 15", R"~~(module main;
func .f()
)~~"},
        {           "lone function - invalid 16", R"~~(module main;
func f(x
)~~"},
        {           "lone function - invalid 17", R"~~(module main;
func f(x: i32
)~~"},
        {           "lone function - invalid 18", R"~~(module main;
func f(x: i32)
)~~"},
        {           "lone function - invalid 19", R"~~(module main;
func f(x: i32,
)~~"},
        {           "lone function - invalid 20", R"~~(module main;
func f(x: i32, y
)~~"},
        {           "lone function - invalid 21", R"~~(module main;
func f(x: i32, y,)
)~~"},
        {           "lone function - invalid 22", R"~~(module main;
func f(x: , y,)
)~~"},
        {           "lone function - invalid 23", R"~~(module main;
func f(x:)
)~~"},
        {           "lone function - invalid 24", R"~~(module main;
func f(x:
)~~"},
        {           "lone function - invalid 25", R"~~(module main;
func(
)~~"},
        {           "lone function - invalid 26", R"~~(module main;
func()
)~~"},
        {           "lone function - invalid 27", R"~~(module main;
func f(x: i32 y: i32);
)~~"},
        {           "lone function - invalid 28", R"~~(module main;
func f(x: i32 y: i32, z: i32);
)~~"},

        {              "lone generic function 1", R"~~(module main;
func f[T]() T {}
)~~"},
        {              "lone generic function 2", R"~~(module main;
func f[T: integer]() T {}
)~~"},
        {              "lone generic function 3", R"~~(module main;
func f[T, U: integer]() T {}
)~~"},
        {              "lone generic function 4", R"~~(module main;
func f[T](x: T, y: T) T {}
)~~"},

        {    "lone generic function - invalid 1", R"~~(module main;
func f[(x: T);
)~~"},
        {    "lone generic function - invalid 2", R"~~(module main;
func f[T(x: T);
)~~"},
        {    "lone generic function - invalid 3", R"~~(module main;
func f[T:(x: T);
)~~"},
        {    "lone generic function - invalid 4", R"~~(module main;
func f[T:](x: T);
)~~"},
        {    "lone generic function - invalid 5", R"~~(module main;
func [)
)~~"},
        {    "lone generic function - invalid 6", R"~~(module main;
func [) i32
)~~"},
        {    "lone generic function - invalid 7", R"~~(module main;
func []) i32
)~~"},
        {    "lone generic function - invalid 8", R"~~(module main;
func [() i32
)~~"},
        {    "lone generic function - invalid 9", R"~~(module main;
func []() i32
)~~"},
        {   "lone generic function - invalid 10", R"~~(module main;
func []() i32;
)~~"},
        {   "lone generic function - invalid 11", R"~~(module main;
func f[T: integer U: testing]() T {}
)~~"},
        {   "lone generic function - invalid 12", R"~~(module main;
func f[T: integer U: testing, V: another]() T {}
)~~"},
        {   "lone generic function - invalid 13", R"~~(module main;
func f[T: integer U: testing, V: another, x: i32) T {}
)~~"},
        {   "lone generic function - invalid 14", R"~~(module main;
func f[T: integer U: testing, V: another( x: i32) T {}
)~~"},

        {         "function and var - invalid 1", R"~~(module main;
func
var x = 10;
)~~"},
        {         "function and var - invalid 2", R"~~(module main;
func f
var x = 10;
)~~"},
        {         "function and var - invalid 3", R"~~(module main;
func f(
var x = 10;
)~~"},
        {         "function and var - invalid 4", R"~~(module main;
func f()
var x = 10;
)~~"},
        {         "function and var - invalid 5", R"~~(module main;
func f() (
var x = 10;
)~~"},
        {         "function and var - invalid 6", R"~~(module main;
func f() ()
var x = 10;
)~~"},
        {         "function and var - invalid 7", R"~~(module main;
func f() {
var x = 10;
)~~"},
        {         "function and var - invalid 8", R"~~(module main;
func f() ({
var x = 10;
)~~"},
        {         "function and var - invalid 9", R"~~(module main;
func f.
var x = 10;
)~~"},
        {        "function and var - invalid 10", R"~~(module main;
func f.(
var x = 10;
)~~"},
        {        "function and var - invalid 11", R"~~(module main;
func f.fn(
var x = 10;
)~~"},
        {        "function and var - invalid 12", R"~~(module main;
func f.fn()
var x = 10;
)~~"},
        {        "function and var - invalid 13", R"~~(module main;
func .f
var x = 10;
)~~"},
        {        "function and var - invalid 14", R"~~(module main;
func .f(
var x = 10;
)~~"},
        {        "function and var - invalid 15", R"~~(module main;
func .f()
var x = 10;
)~~"},
        {        "function and var - invalid 16", R"~~(module main;
func f(x
var x = 10;
)~~"},
        {        "function and var - invalid 17", R"~~(module main;
func f(x: i32
var x = 10;
)~~"},
        {        "function and var - invalid 18", R"~~(module main;
func f(x: i32)
var x = 10;
)~~"},
        {        "function and var - invalid 19", R"~~(module main;
func f(x: i32,
var x = 10;
)~~"},
        {        "function and var - invalid 20", R"~~(module main;
func f(x: i32, y
var x = 10;
)~~"},
        {        "function and var - invalid 21", R"~~(module main;
func f(x: i32, y,)
var x = 10;
)~~"},
        {        "function and var - invalid 22", R"~~(module main;
func f(x: , y,)
var x = 10;
)~~"},
        {        "function and var - invalid 23", R"~~(module main;
func f(x:)
var x = 10;
)~~"},
        {        "function and var - invalid 24", R"~~(module main;
func f(x:
var x = 10;
)~~"},
        {        "function and var - invalid 25", R"~~(module main;
func(
var x = 10;
)~~"},
        {        "function and var - invalid 26", R"~~(module main;
func()
var x = 10;
)~~"},

        { "generic function and var - invalid 1", R"~~(module main;
func f[(x: T);
var x = 69;
)~~"},
        { "generic function and var - invalid 2", R"~~(module main;
func f[T(x: T);
var x = 69;
)~~"},
        { "generic function and var - invalid 3", R"~~(module main;
func f[T:(x: T);
var x = 69;
)~~"},
        { "generic function and var - invalid 4", R"~~(module main;
func f[T:](x: T);
var x = 69;
)~~"},
        { "generic function and var - invalid 5", R"~~(module main;
func [)
var x = 69;
)~~"},
        { "generic function and var - invalid 6", R"~~(module main;
func [) i32
var x = 69;
)~~"},
        { "generic function and var - invalid 7", R"~~(module main;
func []) i32
var x = 69;
)~~"},
        { "generic function and var - invalid 8", R"~~(module main;
func [() i32
var x = 69;
)~~"},
        { "generic function and var - invalid 9", R"~~(module main;
func []() i32
var x = 69;
)~~"},
        {"generic function and var - invalid 10", R"~~(module main;
func []() i32;
var x = 69;
)~~"},
        {"generic function and var - invalid 11", R"~~(module main;
func f[T: integer U: testing]() T {}
var x = 69;
)~~"},
        {"generic function and var - invalid 12", R"~~(module main;
func f[T: integer U: testing, V: another]() T {}
var x = 69;
)~~"},
        {"generic function and var - invalid 13", R"~~(module main;
func f[T: integer U: testing, V: another, x: i32) T {}
var x = 69;
)~~"},
        {"generic function and var - invalid 14", R"~~(module main;
func f[T: integer U: testing, V: another( x: i32) T {}
var x = 69;
)~~"},

        {              "invalid module with var",  R"~~(moule main;
var x = 10;
)~~"},
        {             "invalid module with func",  R"~~(moule main;
func f();
)~~"},

        {                      "some statements", R"~~(module main;
func test() {
    "yo!";
    abc;
    12;
}
)~~"},

        {         "some statements missing semi", R"~~(module main;
func test() {
    "yo!";
    abc
    12;
}
)~~"},

        {                     "return statement", R"~~(module main;
func test() { return x; }
)~~"},
        {                   "return statement 2", R"~~(module main;
func test() { return x, y; }
)~~"},
        {                   "return statement 3", R"~~(module main;
func f() {
    var x, y = 69, 420;
    return x, false;
}
)~~"},
        {                   "return statement 4", R"~~(module main;
func f() {
    var x = _, 12;
    return;
}
)~~"},

        {                            "local var", R"~~(module main;
func f() { var x = 10; }
)~~"},
        {                          "local var 1", R"~~(module main;
func f() { var x: i32 = 10; }
)~~"},
        {                            "local def", R"~~(module main;
func f() { def X = 10; }
)~~"},
        {                          "local def 2", R"~~(module main;
func f() { def X: u64 = 10; }
)~~"},

        {             "invalid return statement", R"~~(module main;
func test() {
    retur
}

var x;
)~~"},
        {           "invalid return statement 2", R"~~(module main;
func test() {
    return
}

var x;
)~~"},
        {           "invalid return statement 3", R"~~(module main;
func test() {
    return x
}

var x;
)~~"},
        {           "invalid return statement 4", R"~~(module main;
func test() { return
var x;
)~~"},
        {           "invalid return statement 5", R"~~(module main;
func f() {
    var x, y = 69, 420
    return x, false;
}
)~~"},
        {           "invalid return statement 6", R"~~(module main;
func f() {
    var x, y  69, 420
    return x, false;
}
)~~"},
        {           "invalid return statement 7", R"~~(module main;
func f() {
    var x, y  69, 420;
    return x, false;
}
)~~"},
        {           "invalid return statement 8", R"~~(module main;
func f() {
    var x 32
    return x false;
}
)~~"},
        {                "local var - invalid 1", R"~~(module main;
func f() {
    var x 32;
    return x, false;
}
)~~"},
        {                "local var - invalid 2", R"~~(module main;
func f() {
    var x =
    return;
}
)~~"},
        {                "local var - invalid 3", R"~~(module main;
func f() {
    var x = ,
    return;
}
)~~"},
        {                "local var - invalid 4", R"~~(module main;
func f() {
    var x = , 12;
    return;
}
)~~"},
        {                "local var - invalid 5", R"~~(module main;
func f() {
    var x y = 10;
}
)~~"},
        {                "local var - invalid 6", R"~~(module main;
func f() {
    var = 10
    var v = ;
}
)~~"},

        {                         "expression 1", R"~~(module main;
func f() { 1 + 1; }
)~~"},
        {                         "expression 2", R"~~(module main;
func f() { 1 + -1; }
)~~"},
        {                         "expression 3", R"~~(module main;
func f() { 1 - -1; }
)~~"},
        {                        "#expression 4", R"~~(module main;
func f() { 1 - 0x10; }
)~~"},
        {                         "expression 5", R"~~(module main;
func f() { (2 + 2) -1; }
)~~"},
        {                         "expression 6", R"~~(module main;
func f() { (2 + 2) --1; }
)~~"},
        {                         "expression 7", R"~~(module main;
func f() { (2 + 2) - -1; }
)~~"},
        {                         "expression 8", R"~~(module main;
func f() { (); x + 1; }
)~~"},
        {                         "expression 9", R"~~(module main;
func f() { -(x - -y); }
)~~"},
        {                        "expression 10", R"~~(module main;
func f() { -(x - +y); }
)~~"},

        {                         "#hello world", R"~~(module main;

func main() i32 {
    c_printf("Hello, %s!\n".ptr, "World".ptr);

    return 0;
}

@extern(link_name="printf")
func c_printf(fmt: c_string, ...);
)~~"},
    };

    for (auto&& [name, input] : tests) {
        ut::add(tb, name,
                [name = name, input = std::move(input)](
                    ut::Context const& ctx) -> nlohmann::json {
                    if (name.starts_with("#")) ut::skip();

                    auto fs = yal::FileStore{};
                    auto er = yal::ErrorReporter{&fs, ctx.out};

                    auto fileid = fs.add_file_and_contents(
                        fmt::format(":memory:{}", name), input);
                    auto tokens = tokenize(er.for_file(fileid));

                    auto       ast = ast::Ast{};
                    ast::Node* root =
                        parse_into_ast(tokens, ast, er.for_file(fileid));

                    return *root;
                });
    }

    return tb;
}

auto integration() -> ut::Test {
    auto tb = ut::group("integration");

    ut::add(tb, tokenizer());
    ut::add(tb, parser());

    return tb;
}

// NOLINTEND(readability-function-size)
// NOLINTEND(readability-function-cognitive-complexity)
}  // namespace yal::tests
