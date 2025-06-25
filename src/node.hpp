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
    Attribute,
    AttributeKV,
    TopVar,
    TopDef,
    Func,

    Block,
    Return,
    ExprStmt,
    Var,
    Def,

    Id,
    Int,
    String,

    NodePack,
    FuncArg,
};

// ----------------------------------------------------------------------------

// fwd

class NodeErr;
class NodeFile;
class NodeAttribute;
class NodeAttributeKV;
class NodeTopVar;
class NodeTopDef;
class NodeFunc;
class NodeBlock;
class NodeId;
class NodeInt;
class NodePack;
class NodeFuncArg;

// ----------------------------------------------------------------------------

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

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == NodeKind::Err;
    }

    [[nodiscard]] constexpr auto is_node_pack() const -> bool {
        return kind == NodeKind::NodePack;
    }

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

    // Convert just common values to json (this overwrites the JSON object).
    void to_json_common_values(nlohmann::json& j) const;

    // Convert just children to json (this does not overwrite the JSON object,
    // just adds the 'children' key.
    void to_json_children(nlohmann::json& j) const;
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

// ----------------------------------------------------------------------------

class NodeFuncArg : public Node {
    std::string_view name;
    Node*            type;

public:
    constexpr NodeFuncArg(Location loc, std::string_view name, Node* type)
        : Node{NodeKind::FuncArg, loc}, name{name}, type{type} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_type() const -> Node* { return type; }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&type, 1};
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

// An attribute that is attached to some declaration.
class NodeAttribute : public Node {
    std::string_view name;

    // A list of all arguments given in the attribute. Key-Value pairs are
    // represented by `AttributeKV`.
    std::span<Node*> args;

public:
    constexpr NodeAttribute(Location loc, std::string_view name,
                            std::span<Node*> args)
        : Node{NodeKind::Attribute, loc}, name{name}, args{args} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return args;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

class NodeAttributeKV : public Node {
    std::string_view key;
    Node*            value{};

public:
    constexpr NodeAttributeKV(Location loc, std::string_view key, Node* value)
        : Node{NodeKind::AttributeKV, loc}, key{key}, value{value} {}

    [[nodiscard]] constexpr auto get_value() const -> Node* { return value; }
    [[nodiscard]] constexpr auto get_key() const -> std::string_view {
        return key;
    }

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&value, 1};
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// ============================================================================

// Top-level (global) variable declaration
class NodeTopVar : public Node {
    std::array<Node*, 4> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeTopVar(Location loc, Node* attributes, Node* names,
                         Node* types, Node* inits)
        : Node{NodeKind::TopVar, loc},
          children{names, types, inits, attributes} {}

    // Get attributes attached to the variable. May be null (in case of no
    // attributes).
    //
    //     @attribute var a, b, c: ta, tb, tc = va, vb, vc;
    //     ^^^^^^^^^^
    [[nodiscard]] auto get_attributes() const -> NodePack*;

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

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// Top-level (global) constant declaration
class NodeTopDef : public Node {
    std::array<Node*, 4> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeTopDef(Location loc, Node* attributes, Node* names,
                         Node* types, Node* inits)
        : Node{NodeKind::TopDef, loc},
          children{names, types, inits, attributes} {}

    // Get attributes attached to the definition. May be null (in case of no
    // attributes).
    //
    //     @attribute def a, b, c: ta, tb, tc = va, vb, vc;
    //     ^^^^^^^^^^
    [[nodiscard]] auto get_attributes() const -> NodePack*;

    // Get the names that are declared. This is a pack of ids, one for each
    // value declared. May be null (in case of parse errors).
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //         ^^^^^^^
    [[nodiscard]] auto get_names() const -> NodePack*;

    // Get the types of the values declared. This is a pack of exprs, one for
    // each explicit type. May be null.
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //                  ^^^^^^^^^^
    [[nodiscard]] auto get_types() const -> NodePack*;

    // Get the initializers of the values declared. This is a pack of exprs, one
    // for each initializer. May be null.
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //                               ^^^^^^^^^^
    [[nodiscard]] auto get_inits() const -> NodePack*;

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// ============================================================================

// A direct function definition (witout namespacing or generics).
//
//     @attribute
//     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
//          ^~~~^~~~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~ ^~~~~~~~~~~~ ^~
//             |          |                          |            |  |
//             |          |                          |            |  \_ body
//             |          |                          |            |
//             |          |                          |            \_ ret
//             |          |                          |
//             |          |                          \_ args
//             |          |
//             |          \_ gargs
//             \_ name
class NodeFunc : public Node {
    std::string_view     name;
    std::string_view     attached_type;
    std::array<Node*, 5> children;

    bool is_c_varargs;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeFunc(Location loc, Node* attributes, std::string_view name,
                       std::string_view attached_type, Node* gargs, Node* args,
                       Node* ret, Node* body, bool is_c_varargs)
        : Node{NodeKind::Func, loc},
          name{name},
          attached_type{attached_type},
          children{attributes, gargs, args, ret, body},
          is_c_varargs{is_c_varargs} {}

    // Get the name of the function.
    //
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //          ^^^^
    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    // Get the namespace name of the function, for when it is attached to a
    // type.
    //
    //     func test.test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //          ^^^^
    [[nodiscard]] constexpr auto get_attached_type() const -> std::string_view {
        return attached_type;
    }

    // Get attributes attached to the definition. May be null (in case of no
    // attributes).
    //
    //     @attribute
    //     ^^^^^^^^^^
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    [[nodiscard]] auto get_attributes() const -> NodePack*;

    // Get generic arguments. May be null (in case of no generic arguments).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //              ^^^^^^^^^^^
    [[nodiscard]] auto get_gargs() const -> NodePack*;

    // Get arguments. May be null (in case of no arguments).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    [[nodiscard]] auto get_args() const -> NodePack*;

    // Get the return values. May be null (in case of no return values).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //                                                     ^^^^^^^^^^^^
    [[nodiscard]] auto get_ret() const -> NodePack*;

    // Get the function body. May be null (in case of no return values).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //                                                     ^^^^^^^^^^^^
    [[nodiscard]] auto get_body() const -> NodeBlock*;

    // If the function has the '...' for c-style variable length arguments.
    [[nodiscard]] constexpr auto get_is_c_varargs() const -> bool {
        return is_c_varargs;
    }

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// ============================================================================

// A node for a code block.
class NodeBlock : public Node {
    std::span<Node*> children;

public:
    constexpr NodeBlock(Location loc, std::span<Node*> children)
        : Node{NodeKind::Block, loc}, children{children} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// A node for a return statement.
class NodeExprStmt : public Node {
    Node* child;

public:
    constexpr NodeExprStmt(Location loc, Node* child)
        : Node{NodeKind::ExprStmt, loc}, child{child} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&child, 1};
    }
};

// A node for a return statement.
class NodeReturn : public Node {
    std::span<Node*> values;

public:
    constexpr NodeReturn(Location loc, std::span<Node*> values)
        : Node{NodeKind::Return, loc}, values{values} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return values;
    }
};

// Local variable declaration
class NodeVar : public Node {
    std::array<Node*, 3> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeVar(Location loc, Node* names, Node* types, Node* inits)
        : Node{NodeKind::Var, loc}, children{names, types, inits} {}

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

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// Local constant declaration
class NodeDef : public Node {
    std::array<Node*, 3> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeDef(Location loc, Node* names, Node* types, Node* inits)
        : Node{NodeKind::Def, loc}, children{names, types, inits} {}

    // Get the names that are declared. This is a pack of ids, one for each
    // value declared. May be null (in case of parse errors).
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //         ^^^^^^^
    [[nodiscard]] auto get_names() const -> NodePack*;

    // Get the types of the values declared. This is a pack of exprs, one for
    // each explicit type. May be null.
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //                  ^^^^^^^^^^
    [[nodiscard]] auto get_types() const -> NodePack*;

    // Get the initializers of the values declared. This is a pack of exprs, one
    // for each initializer. May be null.
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //                               ^^^^^^^^^^
    [[nodiscard]] auto get_inits() const -> NodePack*;

    void to_json(nlohmann::json& j) const override;

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

// A node for a string literal.
class NodeString : public Node {
    std::string_view value;

public:
    constexpr NodeString(Location loc, std::string_view value)
        : Node{NodeKind::String, loc}, value{value} {}

    [[nodiscard]] constexpr auto get_value() const -> std::string_view {
        return value;
    }

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

define_formatter_from_string_view_for_virtual(yal::ast::NodeErr);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFile);
define_formatter_from_string_view_for_virtual(yal::ast::NodeAttribute);
define_formatter_from_string_view_for_virtual(yal::ast::NodeAttributeKV);
define_formatter_from_string_view_for_virtual(yal::ast::NodeTopVar);
define_formatter_from_string_view_for_virtual(yal::ast::NodeTopDef);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFunc);
define_formatter_from_string_view_for_virtual(yal::ast::NodeBlock);
define_formatter_from_string_view_for_virtual(yal::ast::NodeId);
define_formatter_from_string_view_for_virtual(yal::ast::NodeInt);
define_formatter_from_string_view_for_virtual(yal::ast::NodePack);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFuncArg);

#undef define_formatter_from_string_view_for_virtual
