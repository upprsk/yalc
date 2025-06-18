#pragma once

#include <array>
#include <cstdint>
#include <span>
#include <string_view>

#include "location.hpp"
#include "macros.hpp"

namespace yal::ast {

enum class NodeKind {
    Err,

    File,
    TopVar,
    TopDef,

    Id,
    Int,

    NodePack,
};

// Base interface for all AST nodes.
class Node {
    NodeKind kind{};
    Location loc{};

    // Used for union-find
    Node* forward{};

public:
    constexpr Node(NodeKind kind, Location loc) : kind{kind}, loc{loc} {}

    [[nodiscard]] constexpr auto get_kind() const -> NodeKind { return kind; }
    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }

    // In case this node has a forwarding pointer, i.e was set to another using
    // union-find, process the chain and return the latest. In case the node is
    // not forwarded, returns itself.
    [[nodiscard]] constexpr auto find() const -> Node const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    [[nodiscard]] constexpr auto find() -> Node* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    // Set the forwarding pointer to the given node.
    //
    // NOTE: this can cause cycles, so duplicate the node when in doubt.
    constexpr void make_equal_to(Node* n) {
        auto found = find();
        if (found != n) found->forward = n;
    }

    // Get all children of a node. The default implementation returns an empty
    // span.
    [[nodiscard]] virtual auto get_children() const -> std::span<Node* const> {
        return {};
    }

    // This is to allow formatting from the base class. There is a default
    // implementation that just prints kind and location.
    virtual auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator;

    // Convert the node to json.
    virtual void to_json(nlohmann::json& j) const;
};

// Error node.
class NodeErr : public Node {
public:
    constexpr NodeErr(Location loc) : Node{NodeKind::Err, loc} {}
};

// ----------------------------------------------------------------------------

class NodePack : public Node {
    std::span<Node*> children;

public:
    constexpr NodePack(Location loc, std::span<Node*> children)
        : Node{NodeKind::NodePack, loc}, children{children} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// ============================================================================

// The top-level of a parsed file, with all declarations and the module name.
class NodeFile : public Node {
    std::span<Node*> children;
    std::string_view module_name;

public:
    constexpr NodeFile(Location loc, std::span<Node*> children,
                       std::string_view module_name)
        : Node{NodeKind::File, loc},
          children{children},
          module_name{module_name} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// ============================================================================

// Top-level (global) variable declaration
class NodeTopVar : public Node {
    std::array<Node*, 3> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeTopVar(Location loc, Node* names, Node* types, Node* inits)
        : Node{NodeKind::TopVar, loc}, children{names, types, inits} {}

    // Get the names that are declared. This is a pack of ids, one for each
    // value declared. May be null (in case of parse errors).
    //
    //     var a, b, c: ta, tb, tc = va, vb, vc;
    //         ^^^^^^^
    [[nodiscard]] auto get_names() const -> NodePack*;

    // Get the types of the values declared. This is a pack of exprs, one for
    // each explicit type. May be null.
    //
    //     var a, b, c: ta, tb, tc = va, vb, vc;
    //                  ^^^^^^^^^^
    [[nodiscard]] auto get_types() const -> NodePack*;

    // Get the initializers of the values declared. This is a pack of exprs, one
    // for each initializer. May be null.
    //
    //     var a, b, c: ta, tb, tc = va, vb, vc;
    //                               ^^^^^^^^^^
    [[nodiscard]] auto get_inits() const -> NodePack*;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// ============================================================================

// A node for an identifier.
class NodeId : public Node {
    std::string_view id;

public:
    constexpr NodeId(Location loc, std::string_view id)
        : Node{NodeKind::Id, loc}, id{id} {}

    [[nodiscard]] constexpr auto get_value() const -> std::string_view {
        return id;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// A node for an integer literal of any base.
class NodeInt : public Node {
    uint64_t value{};

public:
    constexpr NodeInt(Location loc, uint64_t value)
        : Node{NodeKind::Int, loc}, value{value} {}

    [[nodiscard]] constexpr auto get_value() const -> uint64_t { return value; }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

void to_json(nlohmann::json& j, NodeKind const& n);
void to_json(nlohmann::json& j, Node const& t);

}  // namespace yal::ast

define_formatter_from_string_view(yal::ast::NodeKind);

#define define_formatter_from_string_view_for_virtual(T)   \
                                                           \
    template <>                                            \
    struct fmt ::formatter<T> : formatter<string_view> {   \
        auto format(T const& p, format_context& ctx) const \
            -> format_context ::iterator {                 \
            return p.format_to(ctx);                       \
        }                                                  \
    }

define_formatter_from_string_view_for_virtual(yal::ast::Node);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFile);

#undef define_formatter_from_string_view_for_virtual
