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
        {                       "empty file",                ""},

        {                      "module decl",    "module main;"},
        {            "invalid module decl 1",             "mod"},
        {            "invalid module decl 2",          "module"},
        {            "invalid module decl 3",      "module 134"},
        {            "invalid module decl 4",     "module 134;"},
        {            "invalid module decl 5",     "modul main;"},
        {            "invalid module decl 6",  "module 'main';"},

        {                        "globals 1", R"~~(module main;
var a;
var b: usize;
var c = 42;
var d: usize = 42;

var x, y, z = 0, 1, 2;
var i, j: usize;
var k, l: usize, i32 = 12, 3;
)~~"},
        {                "invalid globals 1", R"~~(module main;

var c

var a,: i32,  10
var b: = 

var global: usize;)~~"},
        {                "invalid globals 2", R"~~(module main;

var 
)~~"},
        {                "invalid globals 3", R"~~(module main;

var global
)~~"},
        {                "invalid globals 4", R"~~(module main;

var global:
)~~"},
        {                "invalid globals 5", R"~~(module main;

var global: i32
)~~"},
        {                "invalid globals 6", R"~~(module main;
var
var good: i32;
)~~"},
        {                "invalid globals 7", R"~~(module main;
var global
var good: i32;
)~~"},
        {                "invalid globals 8", R"~~(module main;
var global: usize
var good: i32;
)~~"},
        {                "invalid globals 9", R"~~(module main;
var global: usize =
var good: i32;
)~~"},
        {               "invalid globals 10", R"~~(module main;
var global: usize = 10
var good: i32;
)~~"},
        {               "invalid globals 11", R"~~(module main;
var global =
var good: i32;
)~~"},
        {               "invalid globals 12", R"~~(module main;
var global = 10
var good: i32;
)~~"},
        {                        "globals 2", R"~~(module main;
var global: usize = 10;
var good: i32;
)~~"},
        {                        "globals 3", R"~~(module main;
var global = 10;
var good: i32;
)~~"},

        {                           "defs 1", R"~~(module main;
def TEN = 10;
var global = TEN;
)~~"},
        {                   "invalid defs 1", R"~~(module main;
def
var global = 10;
)~~"},
        {                   "invalid defs 2", R"~~(module main;
def TEN
var global = 10;
)~~"},
        {                   "invalid defs 3", R"~~(module main;
def TEN =
var global = 10;
)~~"},
        {                   "invalid defs 4", R"~~(module main;
def TEN = 10
var global = TEN;
)~~"},

        {         "global with attributes 1", R"~~(module main;
@magic(123, priority=10)
var thing = 10;
)~~"},
        {         "global with attributes 2", R"~~(module main;
@test(thing=12,)
var global;
)~~"},
        {         "global with attributes 3", R"~~(module main;
@test_1(12,)
@test_2
var global;
)~~"},

        { "invalid global with attributes 1", R"~~(module main;
@
var thing = 10;
)~~"},
        { "invalid global with attributes 2", R"~~(module main;
@
)~~"},
        { "invalid global with attributes 3", R"~~(module main;
@
var
)~~"},
        { "invalid global with attributes 4", R"~~(module main;
@test(
var global;
)~~"},
        { "invalid global with attributes 5", R"~~(module main;
@test(
var global;
)~~"},
        { "invalid global with attributes 6", R"~~(module main;
@test(thing
var global;
)~~"},
        { "invalid global with attributes 7", R"~~(module main;
@test(thing=
var global;
)~~"},
        { "invalid global with attributes 8", R"~~(module main;
@test(thing=12
var global;
)~~"},
        { "invalid global with attributes 9", R"~~(module main;
@test(thing=12,
var global;
)~~"},
        {"invalid global with attributes 10", R"~~(module main;
@test(thing=)
var global;
)~~"},
        {"invalid global with attributes 11", R"~~(module main;
@test(thing=,)
var global;
)~~"},
        {"invalid global with attributes 12", R"~~(module main;
@test_1(
@test_2
var global;
)~~"},
        {"invalid global with attributes 13", R"~~(module main;
@test_1(
@test_2
var global;
)~~"},
        {"invalid global with attributes 14", R"~~(module main;
@test_1(12
@test_2
var global;
)~~"},
        {"invalid global with attributes 15", R"~~(module main;
@test_1(12,
@test_2
var global;
)~~"},
        {"invalid global with attributes 16", R"~~(module main;
@test_1(=12,))
@test_2
var global;
)~~"},
    };

    for (auto&& [name, input] : tests) {
        ut::add(tb, name,
                [name = name, input = std::move(input)](
                    ut::Context const& ctx) -> nlohmann::json {
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
