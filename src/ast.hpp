#pragma once

#include <cstdint>
#include <string_view>

#include "arena.hpp"
#include "location.hpp"
#include "node.hpp"

namespace yal::ast {

class File {
    mem::Arena node_arena;
    mem::Arena strings_arena;

public:
    // Expressions
    // ----------
    auto expr_neg(Location loc, Expr* child) -> Expr* {
        return node_arena.create<Expr>(Expr{
            .kind = ExprKind::Neg,
            .flags = {ExprFlags::ChildrenSingle},
            .loc = loc,
            .children_as = {.single = {child}},
        });
    }

    auto expr_arith(Location loc, ExprKind kind, Expr* lhs, Expr* rhs)
        -> Expr* {
        return node_arena.create<Expr>(Expr{
            .kind = kind,
            .flags = {ExprFlags::ChildrenPair},
            .loc = loc,
            .children_as = {.pair = {lhs, rhs}},
        });
    }

    auto expr_id(Location loc, std::string_view value) -> Expr* {
        return node_arena.create<Expr>(Expr{
            .kind = ExprKind::Id,
            .flags = {},
            .loc = loc,
            .string_value = strings_arena.alloc_string_view(value),
        });
    }

    auto expr_int(Location loc, uint64_t value) -> Expr* {
        return node_arena.create<Expr>(Expr{
            .kind = ExprKind::Int,
            .flags = {},
            .loc = loc,
            .int_value = value,
        });
    }

    auto expr_string(Location loc, std::string_view value) -> Expr* {
        return node_arena.create<Expr>(Expr{
            .kind = ExprKind::String,
            .flags = {},
            .loc = loc,
            .string_value = strings_arena.alloc_string_view(value),
        });
    }

    // Statements
    // ----------

    auto stmt_block(Location loc, std::span<Stmt* const> children) -> Stmt* {
        auto flags = StmtFlags::from_size(children.size());
        auto node = node_arena.create<Stmt>(Stmt{
            .kind = StmtKind::Block,
            .flags = flags,
            .loc = loc,
        });

        if (flags.is_single()) {
            node->children_as.single.stmts = {children[0]};
        } else if (flags.is_pair()) {
            node->children_as.pair.stmts = {children[0], children[1]};
        } else {
            node->children_as.ref.stmts = node_arena.alloc<Stmt*>(children);
        }

        return node;
    }

    auto stmt_return(Location loc, std::span<Expr* const> children) -> Stmt* {
        return stmt_with_exprs(loc, StmtKind::Return, children);
    }

    auto stmt_expr(Location loc, Expr* child) -> Stmt* {
        auto node = node_arena.create<Stmt>(Stmt{
            .kind = StmtKind::Expr,
            .flags = {StmtFlags::ChildrenSingle},
            .loc = loc,
            .children_as = {.single = {.exprs = {child}}},
        });

        return node;
    }

    auto stmt_var(Location loc, std::string_view name, Location name_loc,
                  Expr* type, Expr* init) -> Stmt* {
        auto node = node_arena.create<Stmt>(Stmt{
            .kind = StmtKind::Var,
            .flags = {StmtFlags::ChildrenPair},
            .loc = loc,
            .value_loc = name_loc,
            .children_as = {.pair = {.exprs = {type, init}}},
            .string_value = strings_arena.alloc_string_view(name),
        });

        return node;
    }

    auto stmt_multi_var(Location loc, std::span<Expr* const> names,
                        std::span<Expr* const> types,
                        std::span<Expr* const> inits) -> Stmt* {
        auto var_stmt = node_arena.create<MultiVarStmt>(MultiVarStmt{
            .names = node_arena.alloc<Expr*>(names),
            .types = node_arena.alloc<Expr*>(types),
            .inits = node_arena.alloc<Expr*>(inits),
        });

        auto node = node_arena.create<Stmt>(Stmt{
            .kind = StmtKind::MultiVar,
            .flags = {},
            .loc = loc,
            .children_as = {.var_stmt = var_stmt},
        });

        return node;
    }

private:
    auto stmt_with_exprs(Location loc, StmtKind kind,
                         std::span<Expr* const> children) -> Stmt* {
        auto flags = StmtFlags::from_size(children.size());
        auto node = node_arena.create<Stmt>(Stmt{
            .kind = kind,
            .flags = flags,
            .loc = loc,
        });

        if (flags.is_single()) {
            node->children_as.single.exprs = {children[0]};
        } else if (flags.is_pair()) {
            node->children_as.pair.exprs = {children[0], children[1]};
        } else {
            node->children_as.ref.exprs = node_arena.alloc<Expr*>(children);
        }

        return node;
    }
};

class Ast {
    mem::Arena node_arena;
    mem::Arena strings_arena;

public:
    auto new_node_err(Location loc) -> NodeErr* {
        return new_node<NodeErr>(loc);
    }

    auto new_node_flat_module(Location loc, std::span<Node* const> children,
                              std::string_view module_name) -> NodeFlatModule* {
        return new_node<NodeFlatModule>(loc, dupe_span(children),
                                        dupe_string(module_name));
    }

    auto new_node_file(Location loc, std::span<Node* const> children,
                       std::string_view module_name) -> NodeFile* {
        return new_node<NodeFile>(loc, dupe_span(children),
                                  dupe_string(module_name));
    }

    auto new_attribute(Location loc, std::string_view qualified_name,
                       std::string_view name, std::span<Node* const> args)
        -> NodeAttribute* {
        return new_node<NodeAttribute>(loc, dupe_string(qualified_name),
                                       dupe_string(name), dupe_span(args));
    }

    auto new_attributekv(Location loc, std::string_view key, Node* value)
        -> NodeAttributeKV* {
        return new_node<NodeAttributeKV>(loc, dupe_string(key), value);
    }

    auto new_node_top_def(Location loc, Node* attributes, Node* gargs,
                          Node* names, Node* types, Node* inits) -> NodeDef* {
        return new_node<NodeDef>(loc, attributes, gargs, names, types, inits);
    }

    auto new_node_top_var(Location loc, Node* attributes, Node* names,
                          Node* types, Node* inits) -> NodeVar* {
        return new_node<NodeVar>(loc, attributes, names, types, inits);
    }

    auto new_node_func(Location loc, Node* attributes, std::string_view name,
                       Span name_span, std::string_view attached_type,
                       Span attached_type_span, Node* gargs, Node* args,
                       Node* ret, Node* body, bool is_c_varargs) -> NodeFunc* {
        return new_node<NodeFunc>(loc, attributes, name, name_span,
                                  attached_type, attached_type_span, gargs,
                                  args, ret, body, is_c_varargs);
    }

    auto new_node_block(Location loc, std::span<Node* const> children)
        -> NodeBlock* {
        return new_node<NodeBlock>(loc, dupe_span(children));
    }

    auto new_node_return(Location loc, std::span<Node* const> values)
        -> NodeReturn* {
        return new_node<NodeReturn>(loc, dupe_span(values));
    }

    auto new_node_expr_stmt(Location loc, Node* child) -> NodeExprStmt* {
        return new_node<NodeExprStmt>(loc, child);
    }

    auto new_node_unary(NodeKind kind, Location loc, Node* child)
        -> NodeUnaryExpr* {
        return new_node<NodeUnaryExpr>(kind, loc, child);
    }

    auto new_node_binary(NodeKind kind, Location loc, Node* lhs, Node* rhs)
        -> NodeBinaryExpr* {
        return new_node<NodeBinaryExpr>(kind, loc, lhs, rhs);
    }

    auto new_node_id(Location loc, std::string_view value) -> NodeId* {
        return new_node<NodeId>(loc, dupe_string(value));
    }

    auto new_node_int(Location loc, uint64_t value) -> NodeInt* {
        return new_node<NodeInt>(loc, value);
    }

    auto new_node_string(Location loc, std::string_view value) -> NodeString* {
        return new_node<NodeString>(loc, dupe_string(value));
    }

    auto new_node_func_arg(Location loc, std::string_view name, Node* type)
        -> NodeFuncArg* {
        return new_node<NodeFuncArg>(loc, dupe_string(name), type);
    }

    auto new_node_func_named_ret(Location loc, std::string_view name,
                                 Node* type) -> NodeFuncNamedRet* {
        return new_node<NodeFuncNamedRet>(loc, dupe_string(name), type);
    }

    auto new_node_pack(Location loc, std::span<Node* const> children)
        -> NodePack* {
        return new_node<NodePack>(loc, dupe_span(children));
    }

    // ========================================================================

    template <typename T, typename... Args>
    auto new_node(Args&&... args) -> T* {
        return node_arena.create<T>(std::forward<Args>(args)...);
    }

    template <typename T>
    auto dupe_node(T const& n) -> T* {
        // invoke copy-constuctor
        return new_node<T>(n);
    }

    template <typename T>
    auto dupe_node(T const* n) -> T* {
        return dupe_node(*n);
    }

    auto dupe_string(std::string_view source) -> std::string_view {
        return strings_arena.alloc_string_view(source);
    }

    auto dupe_span(std::span<Node* const> nodes) -> std::span<Node*> {
        // NOTE: using the nodes arena, should use something else?
        return node_arena.alloc<Node*>(nodes);
    }
};

}  // namespace yal::ast
