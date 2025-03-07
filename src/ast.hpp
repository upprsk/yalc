#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "fmt/format.h"
#include "span.hpp"
#include "types.hpp"

namespace yal {

// A handle to an AST node. A typed index into the AST nodes buffer.
//
// 30 bits are used for the index, which means an AST can have up to about 1
// billion nodes (2^30=1'073'741'824). This should be enough.
//
// The 2 remaining bits are used for sanity checking what the handle points (or
// not) to.
//
// - Index: Points to a single node in the AST.
// - Array: Points to an array of AST node handles (does not store the length).
// - Count: Is not an index, but a plain count. Normally used to determine the
//   size of an array.
struct NodeHandle {
    enum class Discriminator {
        Index = 0b00,
        Array = 0b01,
        Count = 0b10,
        Invalid = 0b11,
    };

    // Default constructor creates an invalid handle (all bits set).
    constexpr NodeHandle() : idx{0xFFFF'FFFF} {}

    constexpr auto operator=(NodeHandle const&) -> NodeHandle& = default;
    constexpr auto operator=(NodeHandle&&) -> NodeHandle& = default;
    constexpr NodeHandle(NodeHandle&& o) = default;
    constexpr NodeHandle(NodeHandle const& o) = default;

    // create a node handle to the given node index.
    [[nodiscard]] static constexpr auto init(uint32_t idx) -> NodeHandle {
        auto h = NodeHandle{idx};
        return h.with_discriminator(Discriminator::Index);
    }

    // create a handle to the given node array.
    [[nodiscard]] static constexpr auto init_array(uint32_t idx) -> NodeHandle {
        auto h = NodeHandle{idx};
        return h.with_discriminator(Discriminator::Array);
    }

    // create a handle to the given node array.
    [[nodiscard]] static constexpr auto init_count(uint32_t idx) -> NodeHandle {
        auto h = NodeHandle{idx};
        return h.with_discriminator(Discriminator::Count);
    }

    // create an invalid handle.
    [[nodiscard]] static constexpr auto init_invalid(uint32_t idx = -1)
        -> NodeHandle {
        auto h = NodeHandle{idx};
        return h.with_discriminator(Discriminator::Invalid);
    }

    // -----------------------------------------------------------------------

    // Create a new handle with the given discriminator.
    [[nodiscard]] constexpr auto with_discriminator(Discriminator d) const
        -> NodeHandle {
        return idx | static_cast<uint32_t>(d) << 30;
    }

    // Get the discriminator for the handle.
    [[nodiscard]] constexpr auto get_discriminator() const -> Discriminator {
        return static_cast<Discriminator>(idx >> 30);
    }

    [[nodiscard]] constexpr auto is_index() const -> bool {
        return get_discriminator() == Discriminator::Index;
    }
    [[nodiscard]] constexpr auto is_array() const -> bool {
        return get_discriminator() == Discriminator::Array;
    }
    [[nodiscard]] constexpr auto is_count() const -> bool {
        return get_discriminator() == Discriminator::Count;
    }
    [[nodiscard]] constexpr auto is_invalid() const -> bool {
        return get_discriminator() == Discriminator::Invalid;
    }
    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return !is_invalid();
    }

    // -----------------------------------------------------------------------

    [[nodiscard]] constexpr auto as_index() const -> uint32_t {
        if (!is_index()) throw std::runtime_error{"NodeHandle is not an index"};
        return as_raw_idx();
    }

    [[nodiscard]] constexpr auto as_array() const -> uint32_t {
        if (!is_array()) throw std::runtime_error{"NodeHandle is not an array"};
        return as_raw_idx();
    }

    [[nodiscard]] constexpr auto as_count() const -> uint32_t {
        if (!is_count()) throw std::runtime_error{"NodeHandle is not an count"};
        return as_raw_idx();
    }

    // Use the handle as an index. This removes the discriminator from the top
    // bits so that this can be used for indexing into an array directly.
    [[nodiscard]] constexpr auto as_raw_idx() const -> uint32_t {
        return idx & 0x3FFF'FFFF;
    }

    // Get the internal value.
    [[nodiscard]] constexpr auto value() const -> uint32_t { return idx; }

private:
    constexpr NodeHandle(uint32_t idx) : idx{idx} {}

private:
    uint32_t idx;
};

enum class NodeKind : uint16_t {
    // no children
    Err,
    Break,
    Nil,

    // - first: points to array of children
    // - second: number of children
    //
    // > Uses `WithChildren`.
    Block,
    ExprPack,
    File,
    FuncRetPack,
    IdPack,

    // - first: points to array of children
    // - second: number of children
    //
    // The children are:
    // - [0]: name (which may be a single Id or an IdPack)
    // - [1]: return type
    // - [2]: body
    // - [3:]: argument list
    //
    // > Uses `Func`.
    Func,
    FuncExtern,

    // - first: points to the type of the argument
    // - value: the name of the argument
    //
    // > Uses `Named`
    FuncArg,

    // - first: names of declared variables
    // - second: points to array with type and initializer ({type, init})
    //
    // > Uses `Decl`
    DefDecl,
    VarDecl,

    // - first: points to child
    //
    // > Uses `WithChild`
    AddrOf,
    BinNot,
    Defer,
    Deref,
    ExprStmt,
    LogicNot,
    MultiPtr,
    MultiPtrConst,
    Neg,
    Optional,
    OrReturn,
    Plus,
    Ptr,
    PtrConst,
    ReturnStmt,
    SlicePtr,
    SlicePtrConst,

    // - first: the condition
    // - second: the _then_ branch
    //
    // > Uses `IfStmt`
    IfStmt,

    // - first: the condition
    // - second: points to array with _then_ and _else_ branches ({when_true,
    //   when_false})
    //
    // > Uses `IfStmt`
    IfStmtWithElse,

    // - first: the condition
    // - second: points to array with _decl_ and _then_ branch ({decl,
    //   when_true})
    //
    // > Uses `IfStmt`
    IfStmtWithDecl,

    // - first: the condition
    // - second: points to array with _decl_, _then_ and _else_ branches ({decl,
    //   when_true, when_false})
    //
    // > Uses `IfStmt`
    IfStmtWithDeclAndElse,

    // - first: the condition
    // - second: the loop's block
    //
    // > Uses `WithChildPair`
    WhileStmt,

    // - first: the lhs of the assignment
    // - second: the rhs of the assignment
    //
    // > Uses `WithChildPair`
    Assign,

    // - first: the lhs of operation
    // - second: the rhs of operation
    //
    // > Uses `WithChildPair`
    Add,
    BinAnd,
    BinOr,
    BinXor,
    Div,
    Equal,
    Greater,
    GreaterEqual,
    LogicAnd,
    LogicOr,
    Mod,
    Mul,
    NotEqual,
    OrElse,
    ShftLeft,
    ShftRight,
    Smaller,
    SmallerEqual,
    Sub,

    // - first: the expression to cast
    // - second: the type to cast to
    //
    // > Uses `WithChildPair`
    Cast,

    // - first: points to array of children
    // - second: number of children
    //
    // The children are:
    // - [0]: size of array
    // - [1]: type of array
    // - [2:]: argument list
    //
    // > Uses `Array`
    Array,

    // - first: points to array of children
    // - second: number of children
    //
    // The children are:
    // - [0]: type of array
    // - [1:]: argument list
    //
    // > Uses `Array`
    ArrayAutoLen,

    // - first: size of array
    // - second: type of array
    //
    // > Uses `Array`
    ArrayType,

    // - first: points to array of children
    // - second: number of children
    //
    // The children are:
    // - [0]: callee
    // - [1:]: argument list
    //
    // > Uses `Call`
    Call,

    // - first: the receiver
    // - value: the field name
    //
    // > Uses `Named`
    Field,

    // - value: the field name
    EnumLit,

    // - value: the integer literal value
    Int,

    // - value: the identifier
    Id,

    // - value: the string literal value, with escape sequences already applied.
    Str,
};

struct Ast;

struct Node {
    NodeKind                                            kind = NodeKind::Err;
    Span                                                span;
    NodeHandle                                          first;
    NodeHandle                                          second;
    TypeHandle                                          type;
    std::variant<std::monostate, std::string, uint64_t> value;

    constexpr auto set_type(TypeHandle ty) -> TypeHandle { return type = ty; }

    [[nodiscard]] constexpr auto value_uint64() const -> uint64_t {
        return std::get<uint64_t>(value);
    }

    [[nodiscard]] constexpr auto value_string() const -> std::string const& {
        return std::get<std::string>(value);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] constexpr auto is_lvalue() const -> bool {
        return kind == NodeKind::Deref || kind == NodeKind::Field ||
               kind == NodeKind::Id;
    }

    [[nodiscard]] constexpr auto is_id() const -> bool {
        return kind == NodeKind::Id;
    }

    [[nodiscard]] constexpr auto is_int() const -> bool {
        return kind == NodeKind::Int;
    }

    [[nodiscard]] constexpr auto is_enum_lit() const -> bool {
        return kind == NodeKind::EnumLit;
    }

    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == NodeKind::Func;
    }

    [[nodiscard]] constexpr auto is_func_arg() const -> bool {
        return kind == NodeKind::FuncArg;
    }

    [[nodiscard]] constexpr auto is_block() const -> bool {
        return kind == NodeKind::Block;
    }

    [[nodiscard]] constexpr auto is_field() const -> bool {
        return kind == NodeKind::Field;
    }

    [[nodiscard]] constexpr auto is_str() const -> bool {
        return kind == NodeKind::Str;
    }

    [[nodiscard]] constexpr auto is_id_pack() const -> bool {
        return kind == NodeKind::IdPack;
    }

    [[nodiscard]] constexpr auto is_nil() const -> bool {
        return kind == NodeKind::Nil;
    }

    [[nodiscard]] constexpr auto is_oneof(auto&&... kinds) const -> bool {
        return ((kind == kinds) || ...);
    }

    // ------------------------------------------------------------------------

    struct WithChild {
        NodeHandle child;
    };

    struct WithChildPair {
        NodeHandle first;
        NodeHandle second;
    };

    struct WithChildren {
        std::span<NodeHandle const> children;
    };

    struct Func {
        NodeHandle                  name;
        NodeHandle                  ret;
        NodeHandle                  body;
        std::span<NodeHandle const> args;
        bool                        is_extern;
    };

    struct Named {
        std::string_view name;
        NodeHandle       child;
    };

    struct Decl {
        NodeHandle ids;
        NodeHandle type;
        NodeHandle init;
    };

    struct IfStmt {
        NodeHandle decl;
        NodeHandle cond;
        NodeHandle when_true;
        NodeHandle when_false;
    };

    struct Array {
        NodeHandle                  type;
        NodeHandle                  size;
        std::span<NodeHandle const> items;
    };

    struct Call {
        NodeHandle                  callee;
        std::span<NodeHandle const> args;
    };
};

struct FatNodeHandle {
    Ast const*       ast;
    NodeHandle       node;
    TypeStore const* ts = nullptr;
};

struct Ast {
    [[nodiscard]] auto node_with_children(Node const& n) const
        -> Node::WithChildren;
    [[nodiscard]] auto node_with_child(Node const& n) const -> Node::WithChild;
    [[nodiscard]] auto node_with_child_pair(Node const& n) const
        -> Node::WithChildPair;
    [[nodiscard]] auto node_func(Node const& n) const -> Node::Func;
    [[nodiscard]] auto node_named(Node const& n) const -> Node::Named;
    [[nodiscard]] auto node_decl(Node const& n) const -> Node::Decl;
    [[nodiscard]] auto node_if_stmt(Node const& n) const -> Node::IfStmt;
    [[nodiscard]] auto node_array(Node const& n) const -> Node::Array;
    [[nodiscard]] auto node_call(Node const& n) const -> Node::Call;

    // ------------------------------------------------------------------------

    [[nodiscard]] auto node_func(NodeHandle h) const -> Node::Func {
        return node_func(*get(h));
    }

    // ------------------------------------------------------------------------

    void assert_is_oneof(std::string_view name, Node const& n,
                         auto&&... kinds) const;

    // ------------------------------------------------------------------------

    [[nodiscard]] auto new_node_err(Span span) -> NodeHandle {
        return new_node(NodeKind::Err, span);
    }

    [[nodiscard]] auto new_node_nil(Span span) -> NodeHandle {
        return new_node(NodeKind::Nil, span);
    }

    [[nodiscard]] auto new_node_break(Span span) -> NodeHandle {
        return new_node(NodeKind::Break, span);
    }

    [[nodiscard]] auto new_node_with_children(
        NodeKind kind, Span span, std::span<NodeHandle const> children)
        -> NodeHandle {
        return new_node(kind, span, new_array(children),
                        NodeHandle::init_count(children.size()));
    }

    [[nodiscard]] auto new_node_with_child(NodeKind kind, Span span,
                                           NodeHandle first,
                                           NodeHandle second = {})
        -> NodeHandle {
        return new_node(kind, span, first, second);
    }

    [[nodiscard]] auto new_node_func(NodeKind kind, Span span, NodeHandle name,
                                     std::span<NodeHandle const> args,
                                     NodeHandle ret, NodeHandle body)
        -> NodeHandle {
        return new_node(kind, span, new_array_prepend(args, name, ret, body),
                        NodeHandle::init_count(args.size() + 3));
    }

    [[nodiscard]] auto new_node_named(NodeKind kind, Span span,
                                      NodeHandle child, std::string name)
        -> NodeHandle {
        return new_node(kind, span, child, NodeHandle{}, TypeHandle{}, name);
    }

    [[nodiscard]] auto new_node_named(NodeKind kind, Span span,
                                      std::string name, NodeHandle child)
        -> NodeHandle {
        return new_node(kind, span, child, NodeHandle{}, TypeHandle{}, name);
    }

    [[nodiscard]] auto new_node_decl(NodeKind kind, Span span, NodeHandle ids,
                                     NodeHandle type, NodeHandle init)
        -> NodeHandle {
        return new_node(kind, span, ids, new_array_of(type, init));
    }

    [[nodiscard]] auto new_node_if(NodeKind kind, Span span,
                                   Node::IfStmt params) -> NodeHandle;

    [[nodiscard]] auto new_node_array(NodeKind kind, Span span,
                                      Node::Array params) -> NodeHandle;

    [[nodiscard]] auto new_node_call(Span span, NodeHandle callee,
                                     std::span<NodeHandle const> args)
        -> NodeHandle {
        return new_node(NodeKind::Call, span, new_array_prepend(args, callee),
                        NodeHandle::init_count(args.size() + 1));
    }

    [[nodiscard]] auto new_node_with_str(NodeKind kind, Span span,
                                         std::string value) -> NodeHandle {
        return new_node(kind, span, NodeHandle{}, NodeHandle{}, TypeHandle{},
                        value);
    }

    [[nodiscard]] auto new_node_with_int(NodeKind kind, Span span,
                                         uint64_t value) -> NodeHandle {
        return new_node(kind, span, NodeHandle{}, NodeHandle{}, TypeHandle{},
                        value);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto new_node_id(Span span, std::string value) -> NodeHandle {
        return new_node_with_str(NodeKind::Id, span, value);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto new_node(auto&&... args) -> NodeHandle {
        auto sz = nodes.size();
        nodes.emplace_back(std::forward<decltype(args)>(args)...);
        return NodeHandle::init(sz);
    }

    [[nodiscard]] auto new_array(std::span<NodeHandle const> handles)
        -> NodeHandle {
        auto sz = node_refs.size();
        node_refs.insert(node_refs.end(), handles.begin(), handles.end());
        return NodeHandle::init_array(sz);
    }

    [[nodiscard]] auto new_array_prepend(std::span<NodeHandle const> handles,
                                         auto&&... prepend) -> NodeHandle {
        auto sz = node_refs.size();
        (node_refs.push_back(prepend), ...);
        node_refs.insert(node_refs.end(), handles.begin(), handles.end());
        return NodeHandle::init_array(sz);
    }

    [[nodiscard]] auto new_array_of(auto&&... prepend) -> NodeHandle {
        auto sz = node_refs.size();
        (node_refs.push_back(prepend), ...);
        return NodeHandle::init_array(sz);
    }

    // ------------------------------------------------------------------------

    // Get a reference to a node from it's handle. The pointer is invalid after
    // any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get(NodeHandle h) const -> Node const* {
        return &nodes.at(h.as_index());
    }

    // Get a mutable reference to a node from it's handle. The pointer is
    // invalid after any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get_mut(NodeHandle h) -> Node* {
        return &nodes.at(h.as_index());
    }

    // Get a reference to a node array from it's handle. The pointer is invalid
    // after any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get_array(NodeHandle h, size_t count) const
        -> std::span<NodeHandle const> {
        std::span s = node_refs;
        return s.subspan(h.as_array(), count);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] constexpr auto fatten(NodeHandle h) const -> FatNodeHandle {
        return {.ast = this, .node = h};
    }

    // get the total number of nodes
    [[nodiscard]] constexpr auto size() const -> size_t { return nodes.size(); }

    // get the total number of node references (arrays)
    [[nodiscard]] constexpr auto refs_size() const -> size_t {
        return node_refs.size();
    }

    auto dump(fmt::format_context& ctx, NodeHandle n,
              TypeStore const* ts = nullptr) const
        -> fmt::format_context::iterator;

    void dump_dot(FILE* f, NodeHandle n, TypeStore const* ts = nullptr) const;
    void dump_dot_node(FILE* f, NodeHandle n,
                       TypeStore const* ts = nullptr) const;

private:
    std::vector<Node>       nodes;
    std::vector<NodeHandle> node_refs;
};

}  // namespace yal

template <>
struct fmt::formatter<yal::NodeHandle> {
    bool is_escaped = false;

    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        auto it = ctx.begin();
        if (it != ctx.end()) {
            if (*it == '#') {
                is_escaped = true;
                it++;
            }
        }

        return it;
    }

    auto format(yal::NodeHandle n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::NodeKind> : formatter<string_view> {
    auto format(yal::NodeKind n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::Node> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::Node n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::FatNodeHandle> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::FatNodeHandle n, format_context& ctx) const
        -> format_context::iterator;
};
