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

        CHECK(ast.size() == 2);
        CHECK(ast.refs_size() == 0);
    }

    SECTION("create err node") {
        auto hdl = ast.new_node_err();
        REQUIRE(hdl.is_valid());
        REQUIRE(hdl.value() == 0);
        REQUIRE_FALSE(hdl.is_array());
        REQUIRE(ast.get(hdl)->kind == NodeKind::Err);

        CHECK(ast.size() == 1);
        CHECK(ast.refs_size() == 0);
    }

    SECTION("create nil node") {
        auto hdl = ast.new_node_nil();
        REQUIRE(hdl.is_valid());
        REQUIRE(hdl.value() == 0);
        REQUIRE_FALSE(hdl.is_array());
        REQUIRE(ast.get(hdl)->kind == NodeKind::Nil);

        CHECK(ast.size() == 1);
        CHECK(ast.refs_size() == 0);
    }

    SECTION("create file node") {
        SECTION("empty file node") {
            auto hdl = ast.new_node_file({});
            REQUIRE(hdl.is_valid());
            REQUIRE(hdl.value() == 0);
            REQUIRE_FALSE(hdl.is_array());

            auto node = ast.get(hdl);
            REQUIRE(node->kind == NodeKind::File);
            REQUIRE(node->children().is_valid());
            REQUIRE(node->children().is_array());

            auto children = ast.get_children(hdl);
            REQUIRE(children.size() == 0);

            CHECK(ast.size() == 1);
            CHECK(ast.refs_size() == 0);
        }

        SECTION("file node with single nil child") {
            std::array top_children{ast.new_node_nil()};
            auto       hdl = ast.new_node_file(top_children);
            REQUIRE(hdl.is_valid());
            REQUIRE(hdl.value() == 1);
            REQUIRE_FALSE(hdl.is_array());

            auto node = ast.get(hdl);
            REQUIRE(node->kind == NodeKind::File);
            REQUIRE(node->children().is_valid());
            REQUIRE(node->children().is_array());

            auto children = ast.get_children(hdl);
            REQUIRE(top_children.size() == 1);
            REQUIRE(top_children.at(0).is_valid());
            REQUIRE_FALSE(top_children.at(0).is_array());
            REQUIRE(ast.get(top_children.at(0))->kind == NodeKind::Nil);

            CHECK(ast.size() == 2);
            CHECK(ast.refs_size() == 1);
        }

        SECTION("access file node not as array") {
            std::array top_children{ast.new_node_nil()};
            auto       hdl = ast.new_node_file(top_children);
            REQUIRE(hdl.is_valid());
            REQUIRE(hdl.value() == 1);
            REQUIRE_FALSE(hdl.is_array());

            auto node = ast.get(hdl);
            REQUIRE(node->kind == NodeKind::File);
            REQUIRE(node->children().is_valid());
            REQUIRE(node->children().is_array());

            REQUIRE_THROWS_WITH(
                ast.get(node->children()),
                ContainsSubstring(
                    fmt::format("node handle is an array: {:x}", 0x40000000)));
        }
    }
}

// NOLINTEND(modernize-use-designated-initializers)
// NOLINTEND(readability-function-cognitive-complexity)
