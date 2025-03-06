#include "ast.hpp"

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

TEST_CASE("empty ast", "[ast]") {
    auto ast = Ast{};

    CHECK(ast.size() == 0);
    CHECK(ast.refs_size() == 0);
}

TEST_CASE("invalid handles", "[ast]") {
    auto ast = Ast{};

    SECTION("invalid handle") {
        REQUIRE_THROWS_WITH(ast.get({}), Equals("NodeHandle is not an index"));
    }

    SECTION("array handle in normal") {
        REQUIRE_THROWS_WITH(ast.get(NodeHandle::init_array(0)),
                            Equals("NodeHandle is not an index"));
    }

    SECTION("invalid handle in array") {
        REQUIRE_THROWS_WITH(ast.get_array(NodeHandle::init_invalid(0), 1),
                            Equals("NodeHandle is not an array"));
    }

    SECTION("normal handle in array") {
        REQUIRE_THROWS_WITH(ast.get_array(NodeHandle::init(0), 1),
                            Equals("NodeHandle is not an array"));
    }
}

TEST_CASE("creating nodes", "[ast]") {
    auto ast = Ast{};

    SECTION("empty nodes") {
        auto hdl1 = ast.new_node();
        REQUIRE(ast.get(hdl1) != nullptr);
        REQUIRE(hdl1.is_valid());
        REQUIRE(hdl1.value() == 0);
        REQUIRE_FALSE(hdl1.is_array());
        REQUIRE(ast.get(hdl1)->kind == NodeKind::Err);

        auto hdl2 = ast.new_node();
        REQUIRE(ast.get(hdl2) != nullptr);
        REQUIRE(hdl2.is_valid());
        REQUIRE(hdl2.value() == 1);
        REQUIRE_FALSE(hdl2.is_array());
        REQUIRE(ast.get(hdl2)->kind == NodeKind::Err);

        CHECK(ast.size() == 2);
        CHECK(ast.refs_size() == 0);
    }

    SECTION("create err node") {
        auto hdl = ast.new_node_err({});
        REQUIRE(hdl.is_valid());
        REQUIRE(hdl.value() == 0);
        REQUIRE_FALSE(hdl.is_array());
        REQUIRE(ast.get(hdl)->kind == NodeKind::Err);

        CHECK(ast.size() == 1);
        CHECK(ast.refs_size() == 0);
    }

    SECTION("create nil node") {
        auto hdl = ast.new_node_nil({});
        REQUIRE(hdl.is_valid());
        REQUIRE(hdl.value() == 0);
        REQUIRE_FALSE(hdl.is_array());
        REQUIRE(ast.get(hdl)->kind == NodeKind::Nil);

        CHECK(ast.size() == 1);
        CHECK(ast.refs_size() == 0);
    }

    // TODO: add tests for more node types
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-cognitive-complexity)
