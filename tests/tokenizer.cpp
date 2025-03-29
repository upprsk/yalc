#include "tokenizer.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>
#include <vector>

#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "fmt/ranges.h"

using namespace yal;

using Catch::Matchers::Equals;

// NOLINTBEGIN(readability-function-cognitive-complexity)
// NOLINTBEGIN(modernize-use-designated-initializers)

TEST_CASE("empty or whitespace input", "[tokenizer]") {
    SECTION("empty input") {
        auto source = "";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path};
        auto tokens = tokenize(source, er);

        CHECK_FALSE(er.had_error());
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens.at(0) == Token{
                                    TokenType::Eof,
                                    {.begin = 0, .end = 0},
        });
    }

    SECTION("only whitespace") {
        auto source = "           \t  \n  ";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path};
        auto tokens = tokenize(source, er);

        CHECK_FALSE(er.had_error());
        REQUIRE(tokens.size() == 1);
        REQUIRE(tokens.at(0) == Token{
                                    TokenType::Eof,
                                    {.begin = 17, .end = 17},
        });
    }
}

TEST_CASE("integers", "[tokenizer]") {
    SECTION("decimal integers") {
        auto source = "123  456 789 0_0 120_000";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {TokenType::Int,   {0, 3}},
            {TokenType::Int,   {5, 8}},
            {TokenType::Int,  {9, 12}},
            {TokenType::Int, {13, 16}},
            {TokenType::Int, {17, 24}},
            {TokenType::Eof, {24, 24}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("hexadecimal integers") {
        auto source = "0x123 0xdead_BEEF 0x1E";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {TokenType::Hex,   {0, 5}},
            {TokenType::Hex,  {6, 17}},
            {TokenType::Hex, {18, 22}},
            {TokenType::Eof, {22, 22}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("octal integers") {
        SKIP("octal not implemented");

        auto source = "0o117 0o12 0o__007";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            // TODO
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("binary integers") {
        SKIP("binary not implemented");

        auto source = "0b1101 0b00110011 0b1111_1111";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            // TODO
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }
}

TEST_CASE("identifiers", "[tokenizer]") {
    auto source = "aBc _main __start__ void Main_123 A12 _12 xfF";
    auto path = ":memory:";

    auto er = ErrorReporterForFile{source, path};
    auto tokens = tokenize(source, er);

    std::vector<Token> expected{
        { TokenType::Id,   {0, 3}},
        { TokenType::Id,   {4, 9}},
        { TokenType::Id, {10, 19}},
        { TokenType::Id, {20, 24}},
        { TokenType::Id, {25, 33}},
        { TokenType::Id, {34, 37}},
        { TokenType::Id, {38, 41}},
        { TokenType::Id, {42, 45}},
        {TokenType::Eof, {45, 45}}
    };

    CHECK_FALSE(er.had_error());
    REQUIRE_THAT(tokens, Equals(expected));
}

TEST_CASE("strings", "[tokenizer]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("well formed") {
        auto source = R"~~("abc" "12" "\"" "\n")~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {TokenType::Str,   {0, 5}},
            {TokenType::Str,  {6, 10}},
            {TokenType::Str, {11, 15}},
            {TokenType::Str, {16, 20}},
            {TokenType::Eof, {20, 20}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("unterminated") {
        auto source = R"~~("abc" "\")~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {TokenType::Str, {0, 5}},
            {TokenType::Str, {6, 9}},
            {TokenType::Eof, {9, 9}}
        };

        CHECK(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }
}

TEST_CASE("symbols", "[tokenizer]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("parens braces and brackets") {
        auto source = R"~~(  ({[]}) ( { [ ] } ) )~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {  TokenType::Lparen,   {2, 3}},
            {  TokenType::Lbrace,   {3, 4}},
            {TokenType::Lbracket,   {4, 5}},
            {TokenType::Rbracket,   {5, 6}},
            {  TokenType::Rbrace,   {6, 7}},
            {  TokenType::Rparen,   {7, 8}},
            {  TokenType::Lparen,  {9, 10}},
            {  TokenType::Lbrace, {11, 12}},
            {TokenType::Lbracket, {13, 14}},
            {TokenType::Rbracket, {15, 16}},
            {  TokenType::Rbrace, {17, 18}},
            {  TokenType::Rparen, {19, 20}},
            {     TokenType::Eof, {21, 21}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("comparisons") {
        auto source = R"~~( < <= == != >= > )~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {        TokenType::Less,   {1, 2}},
            {   TokenType::LessEqual,   {3, 5}},
            {  TokenType::EqualEqual,   {6, 8}},
            {   TokenType::BangEqual,  {9, 11}},
            {TokenType::GreaterEqual, {12, 14}},
            {     TokenType::Greater, {15, 16}},
            {         TokenType::Eof, {17, 17}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("assignment") {
        auto source = R"~~( = += -= *= /= >>= <<= &= |= ^= ~= %= )~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {              TokenType::Equal,   {1, 2}},
            {          TokenType::PlusEqual,   {3, 5}},
            {         TokenType::MinusEqual,   {6, 8}},
            {          TokenType::StarEqual,  {9, 11}},
            {         TokenType::SlashEqual, {12, 14}},
            {TokenType::GreaterGreaterEqual, {15, 18}},
            {      TokenType::LessLessEqual, {19, 22}},
            {     TokenType::AmpersandEqual, {23, 25}},
            {          TokenType::PipeEqual, {26, 28}},
            {        TokenType::CarrotEqual, {29, 31}},
            {         TokenType::TildeEqual, {32, 34}},
            {       TokenType::PercentEqual, {35, 37}},
            {                TokenType::Eof, {38, 38}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("arithmetic and bitwise") {
        auto source = R"~~( + ++ - -- * ** / % & | ^ ~ << >> )~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {          TokenType::Plus,   {1, 2}},
            {      TokenType::PlusPlus,   {3, 5}},
            {         TokenType::Minus,   {6, 7}},
            {    TokenType::MinusMinus,  {8, 10}},
            {          TokenType::Star, {11, 12}},
            {      TokenType::StarStar, {13, 15}},
            {         TokenType::Slash, {16, 17}},
            {       TokenType::Percent, {18, 19}},
            {     TokenType::Ampersand, {20, 21}},
            {          TokenType::Pipe, {22, 23}},
            {        TokenType::Carrot, {24, 25}},
            {         TokenType::Tilde, {26, 27}},
            {      TokenType::LessLess, {28, 30}},
            {TokenType::GreaterGreater, {31, 33}},
            {           TokenType::Eof, {34, 34}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("punctuation") {
        auto source = R"~~( : ; , . .. .= .* )~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {   TokenType::Colon,   {1, 2}},
            {    TokenType::Semi,   {3, 4}},
            {   TokenType::Comma,   {5, 6}},
            {     TokenType::Dot,   {7, 8}},
            {  TokenType::DotDot,  {9, 11}},
            {TokenType::DotEqual, {12, 14}},
            { TokenType::DotStar, {15, 17}},
            {     TokenType::Eof, {18, 18}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }

    SECTION("logic") {
        auto source = R"~~( ! and or )~~";
        auto path = ":memory:";

        auto er = ErrorReporterForFile{source, path, devnull};
        auto tokens = tokenize(source, er);

        std::vector<Token> expected{
            {TokenType::Bang,   {1, 2}},
            {  TokenType::Id,   {3, 6}},
            {  TokenType::Id,   {7, 9}},
            { TokenType::Eof, {10, 10}}
        };

        CHECK_FALSE(er.had_error());
        REQUIRE_THAT(tokens, Equals(expected));
    }
}

TEST_CASE("comment", "[tokenizer]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = R"~~(12 + // comment to the end of the line
5;)~~";
    auto path = ":memory:";

    auto er = ErrorReporterForFile{source, path, devnull};
    auto tokens = tokenize(source, er);

    std::vector<Token> expected{
        {    TokenType::Int,   {0, 2}},
        {   TokenType::Plus,   {3, 4}},
        {TokenType::Comment,  {5, 38}},
        {    TokenType::Int, {39, 40}},
        {   TokenType::Semi, {40, 41}},
        {    TokenType::Eof, {41, 41}}
    };

    CHECK_FALSE(er.had_error());
    REQUIRE_THAT(tokens, Equals(expected));
}

TEST_CASE("print array", "[tokenizer]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source =
        R"~~(= == => < << <<= <= > >> >= >>= + ++ += - -- -= * ** *= / /= ! != % %= & &= | |= ^ ^= ~ ~= ; : , . .. .* .= ? (){}[] id 12345_67890 0xDEAD_BEEF_0000_EEEF "a string the contains $" // commenting!)~~";
    auto path = ":memory:";

    auto er = ErrorReporterForFile{source, path, devnull};
    auto tokens = tokenize(source, er);

    std::vector<Token> expected{
        {              TokenType::Equal,     {0, 1}},
        {         TokenType::EqualEqual,     {2, 4}},
        {       TokenType::EqualGreater,     {5, 7}},
        {               TokenType::Less,     {8, 9}},
        {           TokenType::LessLess,   {10, 12}},
        {      TokenType::LessLessEqual,   {13, 16}},
        {          TokenType::LessEqual,   {17, 19}},
        {            TokenType::Greater,   {20, 21}},
        {     TokenType::GreaterGreater,   {22, 24}},
        {       TokenType::GreaterEqual,   {25, 27}},
        {TokenType::GreaterGreaterEqual,   {28, 31}},
        {               TokenType::Plus,   {32, 33}},
        {           TokenType::PlusPlus,   {34, 36}},
        {          TokenType::PlusEqual,   {37, 39}},
        {              TokenType::Minus,   {40, 41}},
        {         TokenType::MinusMinus,   {42, 44}},
        {         TokenType::MinusEqual,   {45, 47}},
        {               TokenType::Star,   {48, 49}},
        {           TokenType::StarStar,   {50, 52}},
        {          TokenType::StarEqual,   {53, 55}},
        {              TokenType::Slash,   {56, 57}},
        {         TokenType::SlashEqual,   {58, 60}},
        {               TokenType::Bang,   {61, 62}},
        {          TokenType::BangEqual,   {63, 65}},
        {            TokenType::Percent,   {66, 67}},
        {       TokenType::PercentEqual,   {68, 70}},
        {          TokenType::Ampersand,   {71, 72}},
        {     TokenType::AmpersandEqual,   {73, 75}},
        {               TokenType::Pipe,   {76, 77}},
        {          TokenType::PipeEqual,   {78, 80}},
        {             TokenType::Carrot,   {81, 82}},
        {        TokenType::CarrotEqual,   {83, 85}},
        {              TokenType::Tilde,   {86, 87}},
        {         TokenType::TildeEqual,   {88, 90}},
        {               TokenType::Semi,   {91, 92}},
        {              TokenType::Colon,   {93, 94}},
        {              TokenType::Comma,   {95, 96}},
        {                TokenType::Dot,   {97, 98}},
        {             TokenType::DotDot,  {99, 101}},
        {            TokenType::DotStar, {102, 104}},
        {           TokenType::DotEqual, {105, 107}},
        {           TokenType::Question, {108, 109}},
        {             TokenType::Lparen, {110, 111}},
        {             TokenType::Rparen, {111, 112}},
        {             TokenType::Lbrace, {112, 113}},
        {             TokenType::Rbrace, {113, 114}},
        {           TokenType::Lbracket, {114, 115}},
        {           TokenType::Rbracket, {115, 116}},
        {                 TokenType::Id, {117, 119}},
        {                TokenType::Int, {120, 131}},
        {                TokenType::Hex, {132, 153}},
        {                TokenType::Str, {154, 179}},
        {            TokenType::Comment, {180, 194}},
        {                TokenType::Eof, {194, 194}}
    };

    CHECK_FALSE(er.had_error());
    REQUIRE_THAT(tokens, Equals(expected));
    REQUIRE(
        fmt::format("{}", tokens) ==
        "[{Equal, {0, 1}}, {EqualEqual, {2, 4}}, {EqualGreater, {5, 7}}, "
        "{Less, {8, 9}}, {LessLess, {10, 12}}, {LessLessEqual, {13, 16}}, "
        "{LessEqual, {17, 19}}, {Greater, {20, 21}}, {GreaterGreater, {22, "
        "24}}, {GreaterEqual, {25, 27}}, {GreaterGreaterEqual, {28, 31}}, "
        "{Plus, {32, 33}}, {PlusPlus, {34, 36}}, {PlusEqual, {37, 39}}, "
        "{Minus, {40, 41}}, {MinusMinus, {42, 44}}, {MinusEqual, {45, 47}}, "
        "{Star, {48, 49}}, {StarStar, {50, 52}}, {StarEqual, {53, 55}}, "
        "{Slash, {56, 57}}, {SlashEqual, {58, 60}}, {Bang, {61, 62}}, "
        "{BangEqual, {63, 65}}, {Percent, {66, 67}}, {PercentEqual, {68, 70}}, "
        "{Ampersand, {71, 72}}, {AmpersandEqual, {73, 75}}, {Pipe, {76, 77}}, "
        "{PipeEqual, {78, 80}}, {Carrot, {81, 82}}, {CarrotEqual, {83, 85}}, "
        "{Tilde, {86, 87}}, {TildeEqual, {88, 90}}, {Semi, {91, 92}}, {Colon, "
        "{93, 94}}, {Comma, {95, 96}}, {Dot, {97, 98}}, {DotDot, {99, 101}}, "
        "{DotStar, {102, 104}}, {DotEqual, {105, 107}}, {Question, {108, "
        "109}}, {Lparen, {110, 111}}, {Rparen, {111, 112}}, {Lbrace, {112, "
        "113}}, {Rbrace, {113, 114}}, {Lbracket, {114, 115}}, {Rbracket, {115, "
        "116}}, {Id, {117, 119}}, {Int, {120, 131}}, {Hex, {132, 153}}, {Str, "
        "{154, 179}}, {Comment, {180, 194}}, {Eof, {194, 194}}]");
}

TEST_CASE("invalid character", "[tokenizer]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = R"~~( 12$ )~~";
    auto path = ":memory:";

    auto er = ErrorReporterForFile{source, path, devnull};
    auto tokens = tokenize(source, er);

    std::vector<Token> expected{
        {TokenType::Int, {1, 3}},
        {TokenType::Err, {3, 4}},
        {TokenType::Eof, {5, 5}}
    };

    CHECK(er.had_error());
    REQUIRE_THAT(tokens, Equals(expected));
    CHECK(fmt::format("{}", tokens) ==
          "[{Int, {1, 3}}, {Err, {3, 4}}, {Eof, {5, 5}}]");
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-cognitive-complexity)
