#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "fmt/format.h"
#include "span.hpp"

namespace yal {

// A handle to an AST node. 30 bits are used as index. Bit 31 is used as a check
// for if the handle is valid. Bit 30 is used as a check for if the handle
// points to an array.
struct NodeHandle {
    constexpr NodeHandle(uint32_t idx = 0xFFFF'FFFF) : idx{idx} {}
    constexpr auto operator=(NodeHandle const&) -> NodeHandle& = default;
    constexpr auto operator=(NodeHandle&&) -> NodeHandle& = default;
    constexpr NodeHandle(NodeHandle&& o) = default;
    constexpr NodeHandle(NodeHandle const& o) = default;

    static constexpr auto from_idx(size_t idx) -> NodeHandle {
        return {static_cast<uint32_t>(idx)};
    }

    [[nodiscard]] constexpr auto as_idx() const -> uint32_t {
        return idx & 0x3FFF'FFFF;
    }

    [[nodiscard]] constexpr auto as_count() const -> uint32_t {
        return as_idx();
    }

    // check that this handle has not been invalidated
    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return (idx & (1 << 31)) == 0;
    }

    // check that this handle refers to an array
    [[nodiscard]] constexpr auto is_array() const -> bool {
        return idx & (1 << 30);
    }

    // return a copy of the handle marked as invalid
    [[nodiscard]] constexpr auto to_invalid() const -> NodeHandle {
        return {idx | (1 << 31)};
    }

    // return a copy of the handle marked as an array
    [[nodiscard]] constexpr auto to_array() const -> NodeHandle {
        return {idx | (1 << 30)};
    }

    [[nodiscard]] constexpr auto value() const -> uint32_t { return idx; }

private:
    uint32_t idx;
};

enum class NodeKind : uint16_t {
    Err,
    Nil,
    File,
    Func,
    Block,
    ExprStmt,
    LogicOr,
    LogicAnd,
    BinOr,
    BinXor,
    BinAnd,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Smaller,
    SmallerEqual,
    ShftLeft,
    ShftRight,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Cast,
    OrElse,
    OrReturn,
    AddrOf,
    LogicNot,
    BinNot,
    Plus,
    Neg,
    Optional,
    Deref,
    Call,
    Field,
    ExprPack,
    EnumLit,
    Int,
    Id,
};

struct Node {
    NodeKind                                            kind = NodeKind::Err;
    Span                                                span;
    NodeHandle                                          first;
    NodeHandle                                          second;
    std::variant<std::monostate, std::string, uint64_t> value;

    [[nodiscard]] constexpr auto children() const -> NodeHandle {
        return first;
    }

    [[nodiscard]] constexpr auto count() const -> uint32_t {
        return second.as_count();
    }

    [[nodiscard]] constexpr auto value_uint64() const -> uint64_t {
        return std::get<uint64_t>(value);
    }

    [[nodiscard]] constexpr auto value_string() const -> std::string const& {
        return std::get<std::string>(value);
    }
};

struct Ast;

struct FatNodeHandle {
    Ast const* ast;
    NodeHandle node;
};

struct Ast {
    [[nodiscard]] auto new_node_err(Span span) -> NodeHandle {
        return new_node(NodeKind::Err, span).second;
    }

    [[nodiscard]] auto new_node_nil(Span span) -> NodeHandle {
        return new_node(NodeKind::Nil, span).second;
    }

    [[nodiscard]] auto new_node_file(Span const&                 span,
                                     std::span<NodeHandle const> children)
        -> NodeHandle {
        return new_node(NodeKind::File, span, new_array(children),
                        children.size())
            .second;
    }

    [[nodiscard]] auto new_node_func(Span const& span, NodeHandle name,
                                     std::span<NodeHandle const> args,
                                     NodeHandle ret, NodeHandle body)
        -> NodeHandle {
        return new_node(NodeKind::Func, span,
                        new_array_plus_three(name, ret, body, args),
                        args.size() + 3)
            .second;
    }

    [[nodiscard]] auto new_node_block(Span const&                 span,
                                      std::span<NodeHandle const> children)
        -> NodeHandle {
        return new_node(NodeKind::Block, span, new_array(children),
                        children.size())
            .second;
    }

    [[nodiscard]] auto new_node_expr_pack(Span const&                 span,
                                          std::span<NodeHandle const> children)
        -> NodeHandle {
        return new_node(NodeKind::ExprPack, span, new_array(children),
                        children.size())
            .second;
    }

    [[nodiscard]] auto new_node_call(Span const& span, NodeHandle callee,
                                     std::span<NodeHandle const> parts)
        -> NodeHandle {
        return new_node(NodeKind::Call, span, new_array_plus_one(callee, parts),
                        parts.size() + 1)
            .second;
    }

    [[nodiscard]] auto new_node_int(Span span, uint64_t v) -> NodeHandle {
        return new_node(NodeKind::Int, span, NodeHandle{}, NodeHandle{}, v)
            .second;
    }

    [[nodiscard]] auto new_node_id(Span span, std::string v) -> NodeHandle {
        return new_node(NodeKind::Id, span, NodeHandle{}, NodeHandle{}, v)
            .second;
    }

    [[nodiscard]] auto new_node_enumlit(Span span, std::string v)
        -> NodeHandle {
        return new_node(NodeKind::EnumLit, span, NodeHandle{}, NodeHandle{}, v)
            .second;
    }

    [[nodiscard]] auto new_node_field(Span span, NodeHandle recv,
                                      std::string field) -> NodeHandle {
        return new_node(NodeKind::Field, span, recv, NodeHandle{}, field)
            .second;
    }

    [[nodiscard]] auto new_node_binary(NodeKind kind, Span span, NodeHandle lhs,
                                       NodeHandle rhs) -> NodeHandle {
        return new_node(kind, span, lhs, rhs).second;
    }

    [[nodiscard]] auto new_node_unary(NodeKind kind, Span span, NodeHandle lhs)
        -> NodeHandle {
        return new_node(kind, span, lhs).second;
    }

    [[nodiscard]] auto new_node(auto&&... args)
        -> std::pair<Node*, NodeHandle> {
        auto sz = nodes.size();
        nodes.emplace_back(std::forward<decltype(args)>(args)...);
        auto node = &nodes.at(sz);

        return {node, NodeHandle::from_idx(sz)};
    }

    [[nodiscard]] auto new_array(std::span<NodeHandle const> handles)
        -> NodeHandle {
        auto sz = node_refs.size();
        node_refs.insert(node_refs.end(), handles.begin(), handles.end());

        return NodeHandle::from_idx(sz).to_array();
    }

    [[nodiscard]] auto new_array_plus_one(NodeHandle                  first,
                                          std::span<NodeHandle const> handles)
        -> NodeHandle {
        auto sz = node_refs.size();
        node_refs.push_back(first);
        node_refs.insert(node_refs.end(), handles.begin(), handles.end());

        return NodeHandle::from_idx(sz).to_array();
    }

    [[nodiscard]] auto new_array_plus_three(NodeHandle first, NodeHandle second,
                                            NodeHandle                  third,
                                            std::span<NodeHandle const> handles)
        -> NodeHandle {
        auto sz = node_refs.size();
        node_refs.push_back(first);
        node_refs.push_back(second);
        node_refs.push_back(third);
        node_refs.insert(node_refs.end(), handles.begin(), handles.end());

        return NodeHandle::from_idx(sz).to_array();
    }

    [[nodiscard]] constexpr auto get_children(NodeHandle h) const
        -> std::span<NodeHandle const> {
        return get_children(get(h));
    }

    [[nodiscard]] constexpr auto get_children(Node const* h) const
        -> std::span<NodeHandle const> {
        return get_children(*h);
    }

    [[nodiscard]] constexpr auto get_children(Node const& h) const
        -> std::span<NodeHandle const> {
        return get_array(h.children(), h.count());
    }

    // Get a reference to a node from it's handle. The pointer is invalid after
    // any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get(NodeHandle h) const -> Node const* {
        if (!h.is_valid())
            throw fmt::system_error(6, "invalid node handle: {:x}", h.value());
        if (h.is_array())
            throw fmt::system_error(1, "node handle is an array: {:x}",
                                    h.value());

        return &nodes.at(h.as_idx());
    }

    // Get a reference to a node array from it's handle. The pointer is invalid
    // after any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get_array(NodeHandle h, size_t count) const
        -> std::span<NodeHandle const> {
        if (!h.is_valid())
            throw fmt::system_error(0, "invalid node handle: {:x}", h.value());
        if (!h.is_array())
            throw fmt::system_error(0, "node handle is not an array: {:x}",
                                    h.value());

        std::span s = node_refs;
        return s.subspan(h.as_idx(), count);
    }

    [[nodiscard]] constexpr auto fatten(NodeHandle h) const -> FatNodeHandle {
        return {.ast = this, .node = h};
    }

    // get the total number of nodes
    [[nodiscard]] constexpr auto size() const -> size_t { return nodes.size(); }

    // get the total number of node references (arrays)
    [[nodiscard]] constexpr auto refs_size() const -> size_t {
        return node_refs.size();
    }

    auto dump(fmt::format_context& ctx, NodeHandle n) const
        -> fmt::format_context::iterator;

private:
    std::vector<Node>       nodes;
    std::vector<NodeHandle> node_refs;
};

}  // namespace yal

template <>
struct fmt::formatter<yal::NodeHandle> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
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
