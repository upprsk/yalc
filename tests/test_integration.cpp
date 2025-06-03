#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/std.h>

#include <any>
#include <libassert/assert.hpp>
#include <nlohmann/json.hpp>

#include "error_reporter.hpp"
#include "file_store.hpp"
#include "tests.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

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

auto integration() -> ut::Test {
    auto tb = ut::group("integration");

    ut::add(tb, tokenizer());

    return tb;
}

// NOLINTEND(readability-function-size)
// NOLINTEND(readability-function-cognitive-complexity)
}  // namespace yal::tests
