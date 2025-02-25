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
        REQUIRE_THROWS_WITH(ast.get({}),
                            ContainsSubstring(fmt::format(
                                "invalid node handle: {:x}", 0xffffffff)));
    }

    SECTION("array handle in normal") {
        REQUIRE_THROWS_WITH(ast.get(NodeHandle::from_idx(0).to_array()),
                            ContainsSubstring(fmt::format(
                                "node handle is an array: {:x}", 0x40000000)));
    }

    SECTION("normal handle in array") {
        REQUIRE_THROWS_WITH(ast.get_array(NodeHandle::from_idx(0), 1),
                            ContainsSubstring(fmt::format(
                                "node handle is not an array: {:x}", 0)));
    }
}

TEST_CASE("creating nodes", "[ast]") {
    auto ast = Ast{};

    SECTION("empty nodes") {
        auto [ptr1, hdl1] = ast.new_node();
        REQUIRE(ptr1 != nullptr);
        REQUIRE(hdl1.is_valid());
        REQUIRE(hdl1.value() == 0);
        REQUIRE_FALSE(hdl1.is_array());
        REQUIRE(ptr1->kind == NodeKind::Err);

        auto [ptr2, hdl2] = ast.new_node();
        REQUIRE(ptr2 != nullptr);
        REQUIRE(hdl2.is_valid());
        REQUIRE(hdl2.value() == 1);
        REQUIRE_FALSE(hdl2.is_array());
        REQUIRE(ptr2->kind == NodeKind::Err);
    }

    SECTION("create err node") {
        auto hdl = ast.new_node_err();
        REQUIRE(hdl.is_valid());
        REQUIRE(hdl.value() == 0);
        REQUIRE_FALSE(hdl.is_array());
        REQUIRE(ast.get(hdl)->kind == NodeKind::Err);
    }

    SECTION("create nil node") {
        auto hdl = ast.new_node_nil();
        REQUIRE(hdl.is_valid());
        REQUIRE(hdl.value() == 0);
        REQUIRE_FALSE(hdl.is_array());
        REQUIRE(ast.get(hdl)->kind == NodeKind::Nil);
    }
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-cognitive-complexity)
