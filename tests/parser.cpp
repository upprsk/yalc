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
// NOLINTBEGIN(readability-function-size)
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

TEST_CASE("one function with nothing", "[ast][func]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = R"~~(func main() {})~~";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(fmt::to_string(ast.fatten(root)) ==
            "File([Func(Id(main), Nil, [], Block([])])])");
}

TEST_CASE("one function with nothing but returns i32", "[ast][func]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = R"~~(func main() i32 {})~~";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(fmt::to_string(ast.fatten(root)) ==
            "File([Func(Id(main), Id(i32), [], Block([])])])");
}

TEST_CASE("one function with expression statements", "[ast][func][stmt]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = R"~~(func main() {
        1 + 2;
        some_function();
    })~~";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse(tokens, source, er);

    REQUIRE_FALSE(er.had_error());
    REQUIRE(fmt::to_string(ast.fatten(root)) ==
            "File([Func(Id(main), Nil, [], Block(["
            "ExprStmt(Add(Int(1), Int(2))), "
            "ExprStmt(Call(Id(some_function), []))"
            "])])])");
}

TEST_CASE("integer expresions", "[ast][expr]") {
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
}

TEST_CASE("id expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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
}

TEST_CASE("pack expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("expr pack") {
        auto source = "(a, b, c)";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "ExprPack([Id(a), Id(b), Id(c)])");
    }

    SECTION("expr pack 2") {
        auto source = "(1 + 2 * 4, abc or_else 4 + 5, extra, false)";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "ExprPack([Add(Int(1), Mul(Int(2), Int(4))), OrElse(Id(abc), "
                "Add(Int(4), Int(5))), Id(extra), Id(false)])");
    }
}

TEST_CASE("enum literals", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    auto source = ".test";
    auto path = ":memory:";

    auto er = ErrorReporter{source, path, devnull};
    auto tokens = tokenize(source, er);

    REQUIRE_FALSE(er.had_error());

    auto [ast, root] = parse_expr(tokens, source, er);
    REQUIRE_FALSE(er.had_error());
    REQUIRE(fmt::to_string(ast.fatten(root)) == "EnumLit(.test)");
}

TEST_CASE("fields", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("one field") {
        auto source = "abc.test";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) == "Field(Id(abc), test)");
    }

    SECTION("two fields") {
        auto source = "abc.test.value";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Field(Field(Id(abc), test), value)");
    }

    SECTION("bound function") {
        auto source = "v.next()";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Call(Field(Id(v), next), [])");
    }

    SECTION("bound function 2") {
        auto source = "1 + 12.next(&some_data) - -56";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Sub(Add(Int(1), Call(Field(Int(12), next), "
                "[AddrOf(Id(some_data))])), Neg(Int(56)))");
    }
}

TEST_CASE("unary expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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
        REQUIRE(fmt::to_string(ast.fatten(root)) == "Optional(Id(SomeStruct))");
    }
}

TEST_CASE("deref expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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

    SECTION("deref 2") {
        auto source = "&abc";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) == "AddrOf(Id(abc))");
    }
}

TEST_CASE("cast expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("cast to i32") {
        auto source = "1 + 2 as i32";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Add(Int(1), Cast(Int(2), Id(i32)))");
    }

    SECTION("cast to call") {
        auto source = "42 as abc(12)";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Cast(Int(42), Call(Id(abc), [Int(12)]))");
    }

    SECTION("or_else") {
        auto source = "124 or_else abc + 1";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "OrElse(Int(124), Add(Id(abc), Int(1)))");
    }

    SECTION("or_else 2") {
        auto source = "(call_to_thing() or_else 1) + 2";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Add(OrElse(Call(Id(call_to_thing), []), Int(1)), Int(2))");
    }

    SECTION("or_return") {
        auto source = "abc or_return";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) == "OrReturn(Id(abc))");
    }

    SECTION("or_return 2") {
        auto source = "some_call() or_return";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "OrReturn(Call(Id(some_call), []))");
    }
}

TEST_CASE("binary expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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

TEST_CASE("shift expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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

TEST_CASE("comparison expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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

TEST_CASE("arithmetic expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

    SECTION("all") {
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

    SECTION("chain") {
        auto source = "1 + 1 + 1";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "Add(Add(Int(1), Int(1)), Int(1))");
    }
}

TEST_CASE("logic expresions", "[ast][expr]") {
    auto                                   devnull = fopen("/dev/null", "w+");
    std::unique_ptr<FILE, void (*)(FILE*)> _{devnull,
                                             [](auto f) { fclose(f); }};

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
        auto source = "test1() and test2()";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "LogicAnd(Call(Id(test1), []), Call(Id(test2), []))");
    }

    SECTION("logic with unary") {
        auto source = "!test and &12";
        auto path = ":memory:";

        auto er = ErrorReporter{source, path, devnull};
        auto tokens = tokenize(source, er);

        REQUIRE_FALSE(er.had_error());

        auto [ast, root] = parse_expr(tokens, source, er);
        REQUIRE_FALSE(er.had_error());
        REQUIRE(fmt::to_string(ast.fatten(root)) ==
                "LogicAnd(LogicNot(Id(test)), AddrOf(Int(12)))");
    }
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-size)
// NOLINTEND(readability-function-cognitive-complexity)
