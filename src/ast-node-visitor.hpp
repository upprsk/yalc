#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "ast-node-id.hpp"
#include "ast.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"

namespace yal::ast {

struct Visitor {
    void visit(Ast& ast, NodeId node_id);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

    virtual void visit_before(Ast& ast, Node const& node) {}

    virtual void visit_err(Ast& ast, Node const& node) {}
    virtual void visit_id(Ast& ast, Node const& node, std::string_view id) {}
    virtual void visit_kw_lit(Ast& ast, Node const& node, std::string_view id) {
    }
    virtual void visit_int(Ast& ast, Node const& node, uint64_t value) {}
    virtual void visit_double(Ast& ast, Node const& node, double value) {}
    virtual void visit_float(Ast& ast, Node const& node, float value) {}
    virtual void visit_str(Ast& ast, Node const& node, std::string_view s) {}
    virtual void visit_char(Ast& ast, Node const& node, uint32_t character) {}

    // -----------------------------------------------------------------------

    virtual void visit_module(Ast& ast, Node const& node, std::string_view name,
                              std::span<NodeId const> children) {
        visit_before_module(ast, node, name, children);
        for (auto const& child : children) visit(ast, child);
        visit_after_module(ast, node, name, children);
    }

    virtual void visit_before_module(Ast& ast, Node const& node,
                                     std::string_view        name,
                                     std::span<NodeId const> children) {}
    virtual void visit_after_module(Ast& ast, Node const& node,
                                    std::string_view        name,
                                    std::span<NodeId const> children) {}

    // -----------------------------------------------------------------------

    virtual void visit_source_file(Ast& ast, Node const& node, NodeId mod,
                                   std::span<NodeId const> children) {
        visit_before_source_file(ast, node, mod, children);
        visit(ast, mod);
        for (auto const& child : children) visit(ast, child);
        visit_after_source_file(ast, node, mod, children);
    }

    virtual void visit_before_source_file(Ast& ast, Node const& node,
                                          NodeId                  mod,
                                          std::span<NodeId const> children) {}
    virtual void visit_after_source_file(Ast& ast, Node const& node, NodeId mod,
                                         std::span<NodeId const> children) {}

    // -----------------------------------------------------------------------

    virtual void visit_module_decl(Ast& ast, Node const& node,
                                   std::string_view name) {}

    // -----------------------------------------------------------------------

    virtual void visit_func_decl(Ast& ast, Node const& node,
                                 std::span<NodeId const> decorators,
                                 NodeId name, std::span<NodeId const> gargs,
                                 std::span<NodeId const> args, NodeId ret,
                                 NodeId body, bool is_c_varargs) {
        visit_before_func_decl(ast, node, decorators, name, gargs, args, ret,
                               body, is_c_varargs);

        for (auto const& dec : decorators) visit(ast, dec);
        visit(ast, name);
        for (auto const& garg : gargs) visit(ast, garg);
        for (auto const& arg : args) visit(ast, arg);
        if (ret.is_valid()) visit(ast, ret);
        if (body.is_valid()) visit(ast, body);

        visit_after_func_decl(ast, node, decorators, name, gargs, args, ret,
                              body, is_c_varargs);
    }

    virtual void visit_before_func_decl(Ast& ast, Node const& node,
                                        std::span<NodeId const> decorators,
                                        NodeId                  name,
                                        std::span<NodeId const> gargs,
                                        std::span<NodeId const> args,
                                        NodeId ret, NodeId body,
                                        bool is_c_varargs) {}
    virtual void visit_after_func_decl(Ast& ast, Node const& node,
                                       std::span<NodeId const> decorators,
                                       NodeId                  name,
                                       std::span<NodeId const> gargs,
                                       std::span<NodeId const> args, NodeId ret,
                                       NodeId body, bool is_c_varargs) {}

    // -----------------------------------------------------------------------

    virtual void visit_top_var_decl(Ast& ast, Node const& node,
                                    std::span<NodeId const> decorators,
                                    NodeId                  child) {
        for (auto const& dec : decorators) visit(ast, dec);
        visit(ast, child);
    }

    virtual void visit_top_def_decl(Ast& ast, Node const& node,
                                    std::span<NodeId const> decorators,
                                    NodeId                  child) {
        for (auto const& dec : decorators) visit(ast, dec);
        visit(ast, child);
    }

    // NOTE: each id in `ids` points to an identifier, not an AST node
    virtual void visit_id_pack(Ast& ast, Node const& node,
                               std::span<NodeId const> ids) {}

    virtual void visit_func_param(Ast& ast, Node const& node,
                                  std::string_view name, NodeId type) {}

    // NOTE: each id in `ret` points to either an identifier or an AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs of return value name and type. The name may be an invalid
    // id, for when the return value is not named.
    virtual void visit_func_ret_pack(Ast& ast, Node const& node,
                                     std::span<NodeId const> ret) {}

    // NOTE: each id in `params` points to either an identifier or an AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs. The name may be an invalid id, for when a raw value is
    // passed. The value may be an invalid id, for when no value is passed.
    virtual void visit_decorator(Ast& ast, Node const& node,
                                 std::string_view        name,
                                 std::span<NodeId const> params) {}

    // =======================================================================

    virtual void visit_expr_pack(Ast& ast, Node const& node,
                                 std::span<NodeId const> children) {
        for (auto const& child : children) visit(ast, child);
    }

#define VISIT_BIN(name)                                               \
    virtual void visit_##name(Ast& ast, Node const& node, NodeId lhs, \
                              NodeId rhs) {                           \
        visit(ast, lhs);                                              \
        visit(ast, rhs);                                              \
    }

    VISIT_BIN(add);
    VISIT_BIN(sub);
    VISIT_BIN(mul);
    VISIT_BIN(div);
    VISIT_BIN(mod);
    VISIT_BIN(left_shift);
    VISIT_BIN(right_shift);
    VISIT_BIN(equal);
    VISIT_BIN(not_equal);
    VISIT_BIN(less);
    VISIT_BIN(less_equal);
    VISIT_BIN(greater);
    VISIT_BIN(greater_equal);
    VISIT_BIN(band);
    VISIT_BIN(bor);
    VISIT_BIN(bxor);
    VISIT_BIN(land);
    VISIT_BIN(lor);
    VISIT_BIN(cast);

#undef VISIT_BIN

#define VISIT_UNARY(name)                                               \
    virtual void visit_##name(Ast& ast, Node const& node, NodeId lhs) { \
        visit(ast, lhs);                                                \
    }

    VISIT_UNARY(addrof);
    VISIT_UNARY(lnot);
    VISIT_UNARY(bnot);
    VISIT_UNARY(neg);

#undef VISIT_UNARY

    virtual void visit_struct_type(Ast& ast, Node const& node,
                                   std::span<NodeId const> fields) {
        for (auto const& field : fields) visit(ast, field);
    }

    virtual void visit_struct_field(Ast& ast, Node const& node,
                                    std::string_view name, NodeId type,
                                    NodeId init) {
        visit(ast, type);
        if (init.is_valid()) visit(ast, init);
    }

    virtual void visit_ptr(Ast& ast, Node const& node, bool is_const,
                           NodeId inner) {
        visit(ast, inner);
    }

    virtual void visit_mptr(Ast& ast, Node const& node, bool is_const,
                            NodeId inner) {
        visit(ast, inner);
    }

    virtual void visit_slice(Ast& ast, Node const& node, bool is_const,
                             NodeId inner) {
        visit(ast, inner);
    }

    virtual void visit_array_type(Ast& ast, Node const& node, bool is_const,
                                  NodeId size, NodeId inner) {
        visit(ast, size);
        visit(ast, inner);
    }

    virtual void visit_array(Ast& ast, Node const& node, NodeId size,
                             NodeId inner, std::span<NodeId const> items) {
        visit(ast, size);
        visit(ast, inner);
        for (auto const& item : items) visit(ast, item);
    }

    // NOTE: each id in `params` points to either the key or the value AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs. The key may be an invalid id, for when a value is given
    // without a key.
    virtual void visit_lit(Ast& ast, Node const& node,
                           std::span<NodeId const> items) {
        for (auto const& item : items) visit(ast, item);
    }

    virtual void visit_call(Ast& ast, Node const& node, NodeId callee,
                            std::span<NodeId const> args) {
        visit(ast, callee);
        for (auto const& arg : args) visit(ast, arg);
    }

    virtual void visit_field(Ast& ast, Node const& node, NodeId receiver,
                             std::string_view name) {
        visit(ast, receiver);
    }

    // =======================================================================

    virtual void visit_block(Ast& ast, Node const& node,
                             std::span<NodeId const> children) {
        visit_before_block(ast, node, children);
        for (auto const& child : children) visit(ast, child);
        visit_after_block(ast, node, children);
    }

    virtual void visit_before_block(Ast& ast, Node const& node,
                                    std::span<NodeId const> children) {}
    virtual void visit_after_block(Ast& ast, Node const& node,
                                   std::span<NodeId const> children) {}

    // -----------------------------------------------------------------------

    virtual void visit_expr_stmt(Ast& ast, Node const& node, NodeId child) {
        visit(ast, child);
    }

    virtual void visit_return_stmt(Ast& ast, Node const& node, NodeId child) {
        if (child.is_valid()) visit(ast, child);
    }

    // NOTE: `wf` may be an invalid id in case the if does not have an else
    virtual void visit_if_stmt(Ast& ast, Node const& node, NodeId cond,
                               NodeId wt, NodeId wf) {
        visit(ast, cond);
        visit(ast, wt);
        if (wf.is_valid()) visit(ast, wf);
    }

    virtual void visit_while_stmt(Ast& ast, Node const& node, NodeId cond,
                                  NodeId body) {
        visit(ast, cond);
        visit(ast, body);
    }

    virtual void visit_var_decl(Ast& ast, Node const& node, NodeId ids,
                                NodeId types, NodeId inits) {
        visit(ast, ids);
        if (types.is_valid()) visit(ast, types);
        if (inits.is_valid()) visit(ast, inits);
    }

    virtual void visit_def_decl(Ast& ast, Node const& node, NodeId ids,
                                NodeId types, NodeId inits) {
        visit(ast, ids);
        if (types.is_valid()) visit(ast, types);
        visit(ast, inits);
    }

#pragma GCC diagnostic pop
};

}  // namespace yal::ast
