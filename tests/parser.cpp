#include "parser.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <catch2/matchers/catch_matchers_vector.hpp>

using namespace yal;

using Catch::Matchers::ContainsSubstring;
using Catch::Matchers::Equals;

// NOLINTBEGIN(readability-function-cognitive-complexity)
// NOLINTBEGIN(modernize-use-designated-initializers)

TEST_CASE("empty source", "[ast]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = "";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(fmt::to_string(ast.fatten(root)) == "File([])");
}

TEST_CASE("just comments", "[ast]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = "// this is a test comment\n\n// more comments!";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(fmt::to_string(ast.fatten(root)) == "File([])");
}

TEST_CASE("expressions", "[ast]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("single integer") {
        auto source = "12";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) == "Int(12)");
    }

    SECTION("single integer with underscores") {
        auto source = "120_000";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) == "Int(120000)");
    }

    SECTION("single id") {
        auto source = "the_id_of_things";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) == "Id(the_id_of_things)");
    }

    SECTION("unary") {
        SECTION("unary 1") {
            auto source = "!false";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) == "LogicNot(Id(false))");
        }

        SECTION("unary 2") {
            auto source = "~(1 << 8) & 0x0FF0";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "BinAnd(BinNot(ShftLeft(Int(1), Int(8))), Int(0))");
        }

        SECTION("unary 3") {
            auto source = "+10 - -10";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "Sub(Plus(Int(10)), Neg(Int(10)))");
        }

        SECTION("unary 4") {
            auto source = "&abc + -(10 * 2)";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "Add(AddrOf(Id(abc)), Neg(Mul(Int(10), Int(2))))");
        }

        SECTION("unary 5") {
            auto source = "?SomeStruct";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "Optional(Id(SomeStruct))");
        }
    }

    SECTION("deref") {
        SECTION("deref 1") {
            auto source = "&something.*";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "AddrOf(Deref(Id(something)))");
        }
    }

    SECTION("binary") {
        SECTION("and") {
            auto source = "ab & 1 << 8";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "BinAnd(Id(ab), ShftLeft(Int(1), Int(8)))");
        }

        SECTION("or") {
            auto source = "FLAGS | a - b ^ testing";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "BinOr(Id(FLAGS), BinXor(Sub(Id(a), Id(b)), Id(testing)))");
        }

        SECTION("binary 1") {
            auto source = "(a << FLAGS_TESTING) | MASK";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "BinOr(ShftLeft(Id(a), Id(FLAGS_TESTING)), Id(MASK))");
        }
    }

    SECTION("shifts") {
        auto source = "abc << 12 + 1 * 2 - (b >> S)";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "ShftLeft(Id(abc), Sub(Add(Int(12), Mul(Int(1), Int(2))), "
                "ShftRight(Id(b), Id(S))))");
    }

    SECTION("comparisons") {
        SECTION("comparisons 1") {
            auto source = "a < 12 and 0 >= 12 - 1";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "LogicAnd(Smaller(Id(a), Int(12)), GreaterEqual(Int(0), "
                    "Sub(Int(12), Int(1))))");
        }

        SECTION("comparisons 2") {
            auto source = "a == b + 1";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "Equal(Id(a), Add(Id(b), Int(1)))");
        }

        SECTION("comparisons 3") {
            auto source = "a == b >> 1 or a != 0";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "LogicOr(Equal(Id(a), ShftRight(Id(b), Int(1))), "
                    "NotEqual(Id(a), Int(0)))");
        }
    }

    SECTION("arithmetic") {
        auto source = "1 + 1 * (2 - 4 / param)";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(
            fmt::to_string(ast.fatten(root)) ==
            "Add(Int(1), Mul(Int(1), Sub(Int(2), Div(Int(4), Id(param)))))");
    }

    SECTION("logic") {
        SECTION("logic 1") {
            auto source = "1 + 2 and (5 - 2 * 2)";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "LogicAnd(Add(Int(1), Int(2)), Sub(Int(5), Mul(Int(2), "
                    "Int(2))))");
        }

        SECTION("logic 2") {
            auto source = "0 and 2 or 5 and (test or a)";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) ==
                    "LogicOr(LogicAnd(Int(0), Int(2)), LogicAnd(Int(5), "
                    "LogicOr(Id(test), Id(a))))");
        }

        SECTION("logic with call") {
            SKIP("call not implemented");

            auto source = "test1() and test2()";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) == "");
        }

        SECTION("logic with unary") {
            SKIP("unary not implemented");

            auto source = "!test and &12";
            auto path = ":memory:";

            auto er = ErrorReporter{source, path, devnull};
            auto tokens = tokenize(source, er);

            REQUIRE_FALSE(er.had_error());

            auto [ast, root] = parse_expr(tokens, source, er);
            REQUIRE_FALSE(er.had_error());
            REQUIRE(fmt::to_string(ast.fatten(root)) == "");
        }
    }
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-cognitive-complexity)
