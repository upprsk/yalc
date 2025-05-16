#include "test_tokenizer.hpp"

#include <cstdio>

#include "file-store.hpp"
#include "fmt/color.h"
#include "fmt/format.h"
#include "nlohmann/json.hpp"
#include "test_helpers.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;

auto gen_tokens(std::string source) -> json {
    yal::MemStream ms;

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, ms.f};

    auto root = fs.add(":memory:", source);
    auto fer = er.for_file(root);
    auto tokens = yal::tokenize(fer.get_source(), fer);
    ms.flush();

    if (fer.had_error()) {
        return json{
            {"stderr", ms.str()}
        };
    }

    return tokens;
}

static void run_test(Context& ctx, TestParams const& p, std::string name,
                     std::string source, bool skip = false) {
    run_checks_for_test(ctx, p, name, skip,
                        [&]() { return gen_tokens(source); });
}

auto test_tokenizer(TestParams const& p) -> std::tuple<int, int, int> {
    Context ctx{.tags = {"tokenizer"}, .filters = p.filters, .tests_ran = {}};

    fmt::println("==============================");

    run_test(ctx, p, "empty file", "");
    run_test(ctx, p, "minimal", "module main;");
    run_test(ctx, p, "equals", "= == =>");
    run_test(ctx, p, "less", "< << <<= <=");
    run_test(ctx, p, "greater", "> >> >= >>=");
    run_test(ctx, p, "plus", "+ ++ +=");
    run_test(ctx, p, "minus", "- -- -=");
    run_test(ctx, p, "star", "* *=");
    run_test(ctx, p, "slash", "/ /=");
    run_test(ctx, p, "bang", "! !=");
    run_test(ctx, p, "percent", "% %=");
    run_test(ctx, p, "ampersand", "& &=");
    run_test(ctx, p, "pipe", "| |=");
    run_test(ctx, p, "carrot", "^ ^=");
    run_test(ctx, p, "tilde", "~ ~=");
    run_test(ctx, p, "punctuation", "; : , . .. ... .* .= .{ ?");
    run_test(ctx, p, "parens", "( ) { } [ ]");

    run_test(ctx, p, "identifiers",
             "hello test _ _one\ntWo_tHREe YAY abc123 _1");
    run_test(ctx, p, "decorator", "@test @_124 @123 @testing_this_THING");
    run_test(ctx, p, "numbers",
             "1234567890 123_456_789 00 0xFF 0xbaba 2145 1.4 3.14 1.99");

    run_test(ctx, p, "string",
             R"~~("this is a string!" "a string \"with quotes\"")~~");
    run_test(ctx, p, "char", R"~~('a' 'b' 'c' '\'' '\xF0')~~");

    run_test(ctx, p, "comment", "// this is a comment");

    run_test(ctx, p, "unterminated string", R"~~("unterminated string)~~");
    run_test(ctx, p, "int dot", R"~~(123.)~~");
    run_test(ctx, p, "int methods", R"~~(123.test())~~");

    run_test(ctx, p, "shbang", R"(#!/usr/local/bin/yalc
yay!)");

    print_test_results("tokenizer", ctx);
    return {ctx.ok, ctx.failed, ctx.skipped};
}
