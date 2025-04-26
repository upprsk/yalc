#pragma once

#include <algorithm>
#include <bit>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "arena.hpp"
#include "ast-node.hpp"
#include "decl-store.hpp"
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

    auto new_err(Location loc) -> Node* { return new_node(NodeKind::Err, loc); }

    // --------
    // Literals
    // --------

    auto new_id(Location loc, std::string_view s) -> Node* {
        return new_node(NodeKind::Id, loc, new_string(s));
    }

    auto new_kw_lit(Location loc, std::string_view s) -> Node* {
        return new_node(NodeKind::KwLit, loc, new_string(s));
    }

    auto new_int(Location loc, uint64_t v) -> Node* {
        return new_node(NodeKind::Int, loc, v);
    }

    auto new_double(Location loc, double d) -> Node* {
        return new_node(NodeKind::Double, loc, d);
    }

    auto new_float(Location loc, float d) -> Node* {
        return new_node(NodeKind::Float, loc, d);
    }

    auto new_str(Location loc, std::string_view s) -> Node* {
        return new_node(NodeKind::Str, loc, new_string(s));
    }

    auto new_char(Location loc, uint32_t c) -> Node* {
        return new_node(NodeKind::Char, loc, c);
    }

    // ---------
    // Top-Level
    // ---------

    auto new_module(Location loc, std::string_view name,
                    std::span<Node* const> children) -> Node* {
        return new_node(NodeKind::Module, loc, new_node_array_with(children),
                        new_string(name));
    }

    auto new_source_file(Location loc, Node* mdecl,
                         std::span<Node* const> children) -> Node* {
        return new_node(NodeKind::SourceFile, loc,
                        new_node_array_with(mdecl, children));
    }

    auto new_module_decl(Location loc, std::string_view s) -> Node* {
        return new_node(NodeKind::ModuleDecl, loc, new_string(s));
    }

    auto new_func_decl(Location loc, Node* decorators, Node* name, Node* gargs,
                       Node* args, Node* ret, Node* body, bool is_c_varags)
        -> Node* {
        return new_node(
            is_c_varags ? NodeKind::FuncDeclWithCVarArgs : NodeKind::FuncDecl,
            loc, new_node_array_with(decorators, name, gargs, args, ret, body));
    }

    auto new_top_var_decl(Location loc, Node* decorators, Node* decl) -> Node* {
        return new_node(NodeKind::TopVarDecl, loc,
                        new_node_array_with(decorators, decl));
    }

    auto new_top_def_decl(Location loc, Node* decorators, Node* decl) -> Node* {
        return new_node(NodeKind::TopDefDecl, loc,
                        new_node_array_with(decorators, decl));
    }

    auto new_id_pack(Location loc, std::span<Node* const> ids) -> Node* {
        return new_node(NodeKind::IdPack, loc, new_node_array_with(ids));
    }

    auto new_func_param(Location loc, std::string_view name, Node* type)
        -> Node* {
        return new_node(NodeKind::FuncParam, loc, new_node_array_with(type),
                        new_string(name));
    }

    auto new_func_params(Location loc, std::span<Node* const> params) -> Node* {
        return new_node(NodeKind::FuncParams, loc, new_node_array_with(params));
    }

    auto new_func_ret_pack(Location loc, std::span<Node* const> items)
        -> Node* {
        return new_node(NodeKind::FuncRetPack, loc, new_node_array_with(items));
    }

    auto new_named_ret(Location loc, std::string_view name, Node* type)
        -> Node* {
        return new_node(NodeKind::NamedRet, loc, new_node_array_with(type),
                        new_string(name));
    }

    /// Create a new Decorator node. If name is an empty string, then an
    /// invalid node is used for it.
    auto new_decorator(Location loc, std::string_view name,
                       std::span<Node* const> items) -> Node* {
        return new_node(NodeKind::Decorator, loc, new_node_array_with(items),
                        new_string(name));
    }

    auto new_decorator_param(Location loc, std::string_view name, Node* value)
        -> Node* {
        return new_node(NodeKind::DecoratorParam, loc,
                        new_node_array_with(value), new_string(name));
    }

    auto new_decorators(Location loc, std::span<Node* const> items) -> Node* {
        return new_node(NodeKind::Decorators, loc, new_node_array_with(items));
    }

    auto new_import_stmt(Location loc, std::string_view path) -> Node* {
        return new_node(NodeKind::ImportStmt, loc, new_string(path));
    }

    // -----------
    // Expressions
    // -----------

    auto new_expr_pack(Location loc, std::span<Node* const> children) -> Node* {
        return new_node(NodeKind::ExprPack, loc, new_node_array_with(children));
    }

    auto new_binary_expr(Location loc, NodeKind kind, Node* left, Node* right)
        -> Node* {
        return new_node(kind, loc, new_node_array_with(left, right));
    }

    auto new_unary_expr(Location loc, NodeKind kind, Node* child) -> Node* {
        return new_node(kind, loc, new_node_array_with(child));
    }

    auto new_struct_type(Location loc, std::span<Node* const> fields) -> Node* {
        return new_node(NodeKind::StructType, loc, new_node_array_with(fields));
    }

    auto new_struct_field(Location loc, std::string_view name, Node* type,
                          Node* init) -> Node* {
        return new_node(NodeKind::StructField, loc,
                        new_node_array_with(type, init), new_string(name));
    }

    auto new_ptr(Location loc, bool is_const, Node* inner) -> Node* {
        return new_node(is_const ? NodeKind::PtrConst : NodeKind::Ptr, loc,
                        new_node_array_with(inner));
    }

    auto new_mptr(Location loc, bool is_const, Node* inner) -> Node* {
        return new_node(is_const ? NodeKind::MultiPtrConst : NodeKind::MultiPtr,
                        loc, new_node_array_with(inner));
    }

    auto new_slice(Location loc, bool is_const, Node* inner) -> Node* {
        return new_node(is_const ? NodeKind::SliceConst : NodeKind::Slice, loc,
                        new_node_array_with(inner));
    }

    auto new_array_type(Location loc, bool is_const, Node* size, Node* inner)
        -> Node* {
        return new_node(
            is_const ? NodeKind::ArrayTypeConst : NodeKind::ArrayType, loc,
            new_node_array_with(inner, size));
    }

    auto new_array(Location loc, Node* size, Node* inner,
                   std::span<Node* const> items) -> Node* {
        return new_node(NodeKind::Array, loc,
                        new_node_array_with(inner, size, items));
    }

    auto new_lit(Location loc, std::span<Node* const> items) -> Node* {
        return new_node(NodeKind::Lit, loc, new_node_array_with(items));
    }

    auto new_lit_param(Location loc, std::string_view key, Node* init)
        -> Node* {
        return new_node(NodeKind::LitParam, loc, new_node_array_with(init),
                        new_string(key));
    }

    auto new_call(Location loc, Node* callee, std::span<Node* const> args)
        -> Node* {
        return new_node(NodeKind::Call, loc, new_node_array_with(callee, args));
    }

    auto new_field(Location loc, Node* receiver, std::string_view name)
        -> Node* {
        return new_node(NodeKind::Field, loc, new_node_array_with(receiver),
                        new_string(name));
    }

    // ----------
    // Statements
    // ----------

    auto new_block(Location loc, std::span<Node* const> children) -> Node* {
        return new_node(NodeKind::Block, loc, new_node_array_with(children));
    }

    auto new_expr_stmt(Location loc, Node* child) -> Node* {
        return new_node(NodeKind::ExprStmt, loc, new_node_array_with(child));
    }

    auto new_return_stmt(Location loc, Node* child) -> Node* {
        return new_node(NodeKind::ReturnStmt, loc, new_node_array_with(child));
    }

    auto new_if_stmt(Location loc, Node* cond, Node* wt, Node* wf) -> Node* {
        return new_node(NodeKind::IfStmt, loc,
                        new_node_array_with(cond, wt, wf));
    }

    auto new_while_stmt(Location loc, Node* cond, Node* body) -> Node* {
        return new_node(NodeKind::WhileStmt, loc,
                        new_node_array_with(cond, body));
    }

    auto new_break(Location loc) -> Node* {
        return new_node(NodeKind::Break, loc);
    }

    auto new_continue(Location loc) -> Node* {
        return new_node(NodeKind::Continue, loc);
    }

    auto new_defer_stmt(Location loc, Node* stmt) -> Node* {
        return new_node(NodeKind::DeferStmt, loc, new_node_array_with(stmt));
    }

    auto new_var_decl(Location loc, Node* ids, Node* types, Node* inits)
        -> Node* {
        return new_node(NodeKind::VarDecl, loc,
                        new_node_array_with(ids, types, inits));
    }

    auto new_def_decl(Location loc, Node* ids, Node* types, Node* inits)
        -> Node* {
        return new_node(NodeKind::DefDecl, loc,
                        new_node_array_with(ids, types, inits));
    }

    auto new_assign_stmt(Location loc, NodeKind kind, Node* left, Node* right)
        -> Node* {
        return new_node(kind, loc, new_node_array_with(left, right));
    }

    auto new_flat_module(Location loc, std::string_view name,
                         std::span<Node* const> children) -> Node* {
        return new_node(NodeKind::FlatModule, loc,
                        new_node_array_with(children), new_string(name));
    }

    auto new_coerce(Location loc, Node* child, types::Type* target) -> Node* {
        auto c =
            new_node(NodeKind::Coerce, loc, new_node_array_with(child), target);
        c->set_type(target);

        return c;
    }

    auto new_discard(Location loc, Node* child) -> Node* {
        return new_node(NodeKind::Discard, loc, new_node_array_with(child));
    }

    // ------------------------------------------------------------------------

    auto shallow_dupe(Node const& node) -> Node* {
        return nodes_arena.create<Node>(node);
    }

public:
    [[nodiscard]] constexpr auto get_decl_store() const -> DeclStore const* {
        return &ds;
    }

    [[nodiscard]] constexpr auto get_decl_store() -> DeclStore* { return &ds; }

private:
    [[nodiscard]] auto new_node(NodeKind kind, Location loc) -> Node* {
        return nodes_arena.create<Node>(kind, loc, std::span<Node*>{},
                                        std::monostate{});
    }

    [[nodiscard]] auto new_node(
        NodeKind kind, Location loc,
        std::variant<std::monostate, float, double, uint64_t, std::string_view,
                     types::Type*> data) -> Node* {
        return nodes_arena.create<Node>(kind, loc, std::span<Node*>{}, data);
    }

    [[nodiscard]] auto new_node(NodeKind kind, Location loc,
                                std::span<Node*> children) -> Node* {
        return nodes_arena.create<Node>(kind, loc, children, std::monostate{});
    }

    [[nodiscard]] auto new_node(
        NodeKind kind, Location loc, std::span<Node*> children,
        std::variant<std::monostate, float, double, uint64_t, std::string_view,
                     types::Type*> data) -> Node* {
        return nodes_arena.create<Node>(kind, loc, children, data);
    }

    [[nodiscard]] auto new_node_array_with(auto&&... items)
        -> std::span<Node*> {
        auto sz = args_size(std::forward<decltype(items)>(items)...);
        if (sz == 0) return {};  // empty, with no allocation when empty

        auto arr = arrays_arena.alloc_size<Node*>(sz);

        insert_into_array(arr.begin(), std::forward<decltype(items)>(items)...);

        return arr;
    }

    void insert_into_array(auto&& it, Node* item, auto&&... rest) {
        *it = item;
        insert_into_array(it + 1, std::forward<decltype(rest)>(rest)...);
    }

    void insert_into_array(auto&& it, std::span<Node* const> items,
                           auto&&... rest) {
        insert_into_array(std::ranges::copy(items, it),
                          std::forward<decltype(rest)>(rest)...);
    }

    void insert_into_array(auto&& /*unused*/) {}

    [[nodiscard]] auto args_size(std::span<Node* const> node, auto&&... rest)
        -> size_t {
        return node.size() + args_size(std::forward<decltype(rest)>(rest)...);
    }

    [[nodiscard]] auto args_size(Node* /*unused*/, auto&&... rest) -> size_t {
        return 1 + args_size(std::forward<decltype(rest)>(rest)...);
    }

    [[nodiscard]] auto args_size() -> size_t { return 0; }

    // allocate a new string buffer from `bytes`
    [[nodiscard]] auto new_string(std::string_view s) -> std::string_view {
        return string_arena.alloc_string_view(s);
    }

private:
    mem::Arena nodes_arena;
    mem::Arena arrays_arena;
    mem::Arena string_arena;

    DeclStore ds;
};

}  // namespace yal::ast
