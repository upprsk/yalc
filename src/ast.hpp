#pragma once

#include <algorithm>
#include <bit>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "ast-node-id.hpp"
#include "ast-node.hpp"
#include "file-store.hpp"
#include "fmt/base.h"
#include "nlohmann/json_fwd.hpp"

namespace yal::ast {
using json = nlohmann::json;

struct FatNodeId;
struct FatNodeArray;

/// Container for an entire AST.
class Ast {
public:
    // -----------------
    // Node Constructors
    // -----------------

    auto new_err(Location loc, std::string s) -> NodeId {
        return new_node(NodeKind::Err, loc, new_bytes(s), NodeId::invalid());
    }

    auto new_err(Location loc) -> NodeId {
        return new_node(NodeKind::Err, loc, NodeId::invalid(),
                        NodeId::invalid());
    }

    // --------
    // Literals
    // --------

    auto new_id(Location loc, std::string_view s) -> NodeId {
        return new_node(NodeKind::Id, loc, new_identifier(s),
                        NodeId::invalid());
    }

    auto new_kw_lit(Location loc, std::string_view s) -> NodeId {
        return new_node(NodeKind::KwLit, loc, new_identifier(s),
                        NodeId::invalid());
    }

    auto new_int(Location loc, uint64_t v) -> NodeId {
        auto hi = NodeId::from_raw_data(v >> 32);
        auto lo = NodeId::from_raw_data(v);
        return new_node(NodeKind::Int, loc, hi, lo);
    }

    auto new_double(Location loc, double d) -> NodeId {
        auto v = std::bit_cast<uint64_t>(d);
        auto hi = NodeId::from_raw_data(v >> 32);
        auto lo = NodeId::from_raw_data(v);
        return new_node(NodeKind::Double, loc, hi, lo);
    }

    auto new_float(Location loc, float d) -> NodeId {
        auto v = std::bit_cast<uint32_t>(d);
        return new_node(NodeKind::Float, loc, NodeId::from_raw_data(v),
                        NodeId::invalid());
    }

    auto new_str(Location loc, std::string s) -> NodeId {
        return new_node(NodeKind::Str, loc, new_bytes(s), NodeId::invalid());
    }

    auto new_char(Location loc, uint32_t c) -> NodeId {
        return new_node(NodeKind::Char, loc, NodeId::from_raw_data(c),
                        NodeId::invalid());
    }

    // ---------
    // Top-Level
    // ---------

    auto new_module(Location loc, std::string name,
                    std::span<NodeId const> children) -> NodeId {
        auto second = new_ref_array_sized(children);
        return new_node(NodeKind::Module, loc, new_identifier(name), second);
    }

    auto new_source_file(Location loc, NodeId mdecl,
                         std::span<NodeId const> children) -> NodeId {
        auto second = new_ref_array_sized(children);
        return new_node(NodeKind::SourceFile, loc, mdecl, second);
    }

    auto new_module_decl(Location loc, std::string_view s) -> NodeId {
        return new_node(NodeKind::ModuleDecl, loc, new_identifier(s),
                        NodeId::invalid());
    }

    auto new_func_decl(Location loc, std::span<NodeId const> decorators,
                       NodeId name, std::span<NodeId const> gargs,
                       std::span<NodeId const> args, NodeId ret, NodeId body,
                       bool is_c_varags) -> NodeId {
        return new_node(
            is_c_varags ? NodeKind::FuncDeclWithCVarArgs : NodeKind::FuncDecl,
            loc, name,
            new_ref_array_with(NodeId::from_raw_data(decorators.size()),
                               decorators, NodeId::from_raw_data(gargs.size()),
                               gargs, NodeId::from_raw_data(args.size()), args,
                               ret, body));
    }

    auto new_top_var_decl(Location loc, std::span<NodeId const> decorators,
                          NodeId decl) -> NodeId {
        return new_node(NodeKind::TopVarDecl, loc, decl,
                        new_ref_array_sized(decorators));
    }

    auto new_top_def_decl(Location loc, std::span<NodeId const> decorators,
                          NodeId decl) -> NodeId {
        return new_node(NodeKind::TopDefDecl, loc, decl,
                        new_ref_array_sized(decorators));
    }

    auto new_id_pack(Location loc, std::span<NodeId const> ids) -> NodeId {
        return new_node(NodeKind::IdPack, loc,
                        NodeId::from_raw_data(ids.size()),
                        new_ref_array_with(ids));
    }

    auto new_func_param(Location loc, std::string_view name, NodeId type)
        -> NodeId {
        return new_node(NodeKind::FuncParam, loc, new_identifier(name), type);
    }

    /// Create a new FuncRetPack node. If name is an empty string, then an
    /// invalid node is used for it.
    auto new_func_ret_pack(Location                                       loc,
                           std::span<std::pair<std::string_view, NodeId>> items)
        -> NodeId {
        return new_node(NodeKind::FuncRetPack, loc,
                        NodeId::from_raw_data(items.size()),
                        new_ref_array_of_idpair(items));
    }

    /// Create a new Decorator node. If name is an empty string, then an
    /// invalid node is used for it.
    auto new_decorator(Location loc, std::string_view name,
                       std::span<std::pair<std::string_view, NodeId>> items)
        -> NodeId {
        return new_node(NodeKind::Decorator, loc, new_identifier(name),
                        new_ref_array_of_idpair_with_size(items));
    }

    // -----------
    // Expressions
    // -----------

    auto new_expr_pack(Location loc, std::span<NodeId const> children)
        -> NodeId {
        return new_node(NodeKind::ExprPack, loc,
                        NodeId::from_raw_data(children.size()),
                        new_ref_array_with(children));
    }

    auto new_binary_expr(Location loc, NodeKind kind, NodeId left, NodeId right)
        -> NodeId {
        return new_node(kind, loc, left, right);
    }

    auto new_unary_expr(Location loc, NodeKind kind, NodeId child) -> NodeId {
        return new_node(kind, loc, child, NodeId::invalid());
    }

    auto new_struct_type(Location loc, std::span<NodeId const> fields)
        -> NodeId {
        return new_node(NodeKind::StructType, loc,
                        NodeId::from_raw_data(fields.size()),
                        new_ref_array_with(fields));
    }

    auto new_struct_field(Location loc, std::string_view name, NodeId type,
                          NodeId init) -> NodeId {
        return new_node(NodeKind::StructField, loc, new_identifier(name),
                        new_ref_array_with(type, init));
    }

    auto new_ptr(Location loc, bool is_const, NodeId inner) -> NodeId {
        return new_node(is_const ? NodeKind::PtrConst : NodeKind::Ptr, loc,
                        inner, NodeId::invalid());
    }

    auto new_mptr(Location loc, bool is_const, NodeId inner) -> NodeId {
        return new_node(is_const ? NodeKind::MultiPtrConst : NodeKind::MultiPtr,
                        loc, inner, NodeId::invalid());
    }

    auto new_slice(Location loc, bool is_const, NodeId inner) -> NodeId {
        return new_node(is_const ? NodeKind::SliceConst : NodeKind::Slice, loc,
                        inner, NodeId::invalid());
    }

    auto new_array_type(Location loc, bool is_const, NodeId size, NodeId inner)
        -> NodeId {
        return new_node(
            is_const ? NodeKind::ArrayTypeConst : NodeKind::ArrayType, loc,
            inner, size);
    }

    auto new_array(Location loc, NodeId size, NodeId inner,
                   std::span<NodeId const> items) -> NodeId {
        return new_node(NodeKind::Array, loc, inner,
                        new_ref_array_with(
                            size, NodeId::from_raw_data(items.size()), items));
    }

    auto new_lit(Location loc, std::span<std::pair<NodeId, NodeId> const> items)
        -> NodeId {
        return new_node(NodeKind::Lit, loc, NodeId::from_raw_data(items.size()),
                        new_ref_array_with(items));
    }

    auto new_call(Location loc, NodeId callee, std::span<NodeId const> args)
        -> NodeId {
        return new_node(NodeKind::Call, loc, callee, new_ref_array_sized(args));
    }

    auto new_field(Location loc, NodeId receiver, std::string_view name)
        -> NodeId {
        return new_node(NodeKind::Field, loc, receiver, new_identifier(name));
    }

    // ----------
    // Statements
    // ----------

    auto new_block(Location loc, std::span<NodeId const> children) -> NodeId {
        return new_node(NodeKind::Block, loc,
                        NodeId::from_raw_data(children.size()),
                        new_ref_array_with(children));
    }

    auto new_expr_stmt(Location loc, NodeId child) -> NodeId {
        return new_node(NodeKind::ExprStmt, loc, child, NodeId::invalid());
    }

    auto new_return_stmt(Location loc, NodeId child) -> NodeId {
        return new_node(NodeKind::ReturnStmt, loc, child, NodeId::invalid());
    }

    auto new_if_stmt(Location loc, NodeId cond, NodeId wt, NodeId wf)
        -> NodeId {
        if (wf.is_valid())
            return new_node(NodeKind::IfStmtWithElse, loc, cond,
                            new_ref_array_with(wt, wf));

        return new_node(NodeKind::IfStmt, loc, cond, wt);
    }

    auto new_while_stmt(Location loc, NodeId cond, NodeId body) -> NodeId {
        return new_node(NodeKind::WhileStmt, loc, cond, body);
    }

    auto new_break(Location loc) -> NodeId {
        return new_node(NodeKind::Break, loc, NodeId::invalid(),
                        NodeId::invalid());
    }

    auto new_continue(Location loc) -> NodeId {
        return new_node(NodeKind::Continue, loc, NodeId::invalid(),
                        NodeId::invalid());
    }

    auto new_var_decl(Location loc, NodeId ids, NodeId types, NodeId inits)
        -> NodeId {
        return new_node(NodeKind::VarDecl, loc, ids,
                        new_ref_array_with(types, inits));
    }

    auto new_def_decl(Location loc, NodeId ids, NodeId types, NodeId inits)
        -> NodeId {
        return new_node(NodeKind::DefDecl, loc, ids,
                        new_ref_array_with(types, inits));
    }

    auto new_assign_stmt(Location loc, NodeKind kind, NodeId left, NodeId right)
        -> NodeId {
        return new_node(kind, loc, left, right);
    }

public:
    [[nodiscard]] constexpr auto fatten(NodeId id) -> FatNodeId;
    [[nodiscard]] constexpr auto fatten(std::span<NodeId const> ids)
        -> FatNodeArray;

public:
    [[nodiscard]] constexpr auto get_node_kind(NodeIdOfRef h) const
        -> NodeKind {
        return nodes.at(h.value).get_kind();
    }

    [[nodiscard]] constexpr auto get_node_loc(NodeIdOfRef h) const -> Location {
        return nodes.at(h.value).get_loc();
    }

    [[nodiscard]] constexpr auto get_node_span(NodeIdOfRef h) const -> Span {
        return nodes.at(h.value).get_loc().span;
    }

    [[nodiscard]] constexpr auto get_node(NodeIdOfRef h) const -> Node {
        return nodes.at(h.value);
    }

    // get an array of nodes from the given length and id.
    [[nodiscard]] constexpr auto get_array(NodeIdOfCount length,
                                           NodeIdOfArray h) const
        -> std::span<NodeId const> {
        std::span view = refs_array;
        return view.subspan(h.value, length.value);
    }

    // get an array of nodes from the given id. The length is stored at the
    // first pointed-to location.
    [[nodiscard]] constexpr auto get_array(NodeIdOfArray h) const
        -> std::span<NodeId const> {
        auto      length = refs_array.at(h.value).as_count();
        std::span view = refs_array;

        return view.subspan(h.value + 1, length.value);
    }

    // get an array of key-value nodes from the given id. The length is stored
    // at the first pointed-to location.
    [[nodiscard]] constexpr auto get_array_of_kv(NodeIdOfArray h) const
        -> std::span<NodeId const> {
        auto      length = refs_array.at(h.value).as_count();
        std::span view = refs_array;

        return view.subspan(h.value + 1, length.of_kv().value);
    }

    // get an array starting at the given id, but without a set size.
    [[nodiscard]] constexpr auto get_array_unbounded(NodeIdOfArray h) const
        -> std::span<NodeId const> {
        std::span view = refs_array;
        return view.subspan(h.value);
    }

    // get an identifier from `identifiers`
    [[nodiscard]] constexpr auto get_identifier(NodeIdOfId h) const
        -> std::string const& {
        return identifiers.at(h.value);
    }

    // get a byte buffer from `bytes`.
    [[nodiscard]] constexpr auto get_bytes(NodeIdOfBytes h) const
        -> std::vector<char> const& {
        return bytes.at(h.value);
    }

    // get a byte buffer from `bytes` as a string_view.
    [[nodiscard]] constexpr auto get_bytes_as_string_view(NodeIdOfBytes h) const
        -> std::string_view {
        auto const& b = bytes.at(h.value);
        return {b.data(), b.size()};
    }

    // -----------------------------------------------------------------------

    // allocate a new identifier from `identifiers`
    [[nodiscard]] auto new_identifier(std::string_view s) -> NodeId {
        auto sz = identifiers.size();
        identifiers.emplace_back(s);
        return NodeId::from_raw_data(sz);
    }

private:
    [[nodiscard]] auto new_node(NodeKind kind, Location loc, NodeId first,
                                NodeId second) -> NodeId {
        auto sz = nodes.size();
        auto id = NodeId::from_raw_data(sz);
        nodes.push_back(Node::from_parts(kind, id, loc, first, second));
        return id;
    }

    /// Create a new array where the first element contains the size of the
    /// array.
    [[nodiscard]] auto new_ref_array_sized(std::span<NodeId const> items)
        -> NodeId {
        return new_ref_array_with(NodeId::from_raw_data(items.size()), items);
    }

    [[nodiscard]] auto new_ref_array_with(auto&&... items) -> NodeId {
        auto sz = refs_array.size();
        push_to_array(items...);
        return NodeId::from_raw_data(sz);
    }

    /// Create an array made of pairs of identifiers and regular NodeIds. If the
    /// string for the id is empty, then an invalid NodeId is added in it's
    /// place.
    [[nodiscard]] auto new_ref_array_of_idpair(
        std::span<std::pair<std::string_view, NodeId> const> items) -> NodeId {
        auto sz = refs_array.size();
        for (auto const& [identifier, node] : items) {
            if (identifier.empty()) {
                refs_array.push_back(NodeId::invalid());
                refs_array.push_back(node);
            } else {
                auto id = new_identifier(identifier);
                refs_array.push_back(id);
                refs_array.push_back(node);
            }
        }

        return NodeId::from_raw_data(sz);
    }

    /// Create an array made of pairs of identifiers and regular NodeIds, but
    /// add the size as the first element. If the string for the id is empty,
    /// then an invalid NodeId is added in it's place.
    [[nodiscard]] auto new_ref_array_of_idpair_with_size(
        std::span<std::pair<std::string_view, NodeId> const> items) -> NodeId {
        auto sz = refs_array.size();

        refs_array.push_back(NodeId::from_raw_data(items.size()));
        for (auto const& [identifier, node] : items) {
            if (identifier.empty()) {
                refs_array.push_back(NodeId::invalid());
                refs_array.push_back(node);
            } else {
                auto id = new_identifier(identifier);
                refs_array.push_back(id);
                refs_array.push_back(node);
            }
        }

        return NodeId::from_raw_data(sz);
    }

    // allocate a new string buffer from `bytes`
    [[nodiscard]] auto new_bytes(std::string_view s) -> NodeId {
        auto sz = bytes.size();
        bytes.emplace_back(s.data(), s.data() + s.size());
        return NodeId::from_raw_data(sz);
    }

    void push_to_array(NodeId item, auto&&... rest) {
        refs_array.push_back(item);
        push_to_array(rest...);
    }

    void push_to_array(std::span<NodeId const> items, auto&&... rest) {
        refs_array.insert(refs_array.end(), items.begin(), items.end());
        push_to_array(rest...);
    }

    void push_to_array(std::span<std::pair<NodeId, NodeId> const> items,
                       auto&&... rest) {
        for (auto const& [first, second] : items) {
            refs_array.push_back(first);
            refs_array.push_back(second);
        }

        push_to_array(rest...);
    }

    void push_to_array() {}

private:
    std::vector<Node>   nodes;
    std::vector<NodeId> refs_array;

    // NOTE: Should use a more efficient storage for strings
    std::vector<std::string>       identifiers;
    std::vector<std::vector<char>> bytes;
};

struct FatNodeId {
    Ast*   ast;
    NodeId id;

    auto dump_to_ctx(fmt::format_context& ctx) const
        -> fmt::format_context::iterator;
};

struct FatNodeArray {
    Ast*                    ast;
    std::span<NodeId const> ids;
};

constexpr auto Ast::fatten(NodeId id) -> FatNodeId {
    return {.ast = this, .id = id};
}

constexpr auto Ast::fatten(std::span<NodeId const> ids) -> FatNodeArray {
    return {.ast = this, .ids = ids};
}

void to_json(json& j, FatNodeId const& n);
void to_json(json& j, FatNodeArray const& n);

}  // namespace yal::ast

template <>
struct fmt::formatter<yal::ast::FatNodeId> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ast::FatNodeId n, format_context& ctx) const
        -> format_context::iterator;
};
