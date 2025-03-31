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
                                 NodeId body) {
        visit_before_func_decl(ast, node, decorators, name, gargs, args, ret,
                               body);

        for (auto const& dec : decorators) visit(ast, dec);
        visit(ast, name);
        for (auto const& garg : gargs) visit(ast, garg);
        for (auto const& arg : args) visit(ast, arg);
        if (ret.is_valid()) visit(ast, ret);
        if (body.is_valid()) visit(ast, body);

        visit_after_func_decl(ast, node, decorators, name, gargs, args, ret,
                              body);
    }

    virtual void visit_before_func_decl(Ast& ast, Node const& node,
                                        std::span<NodeId const> decorators,
                                        NodeId                  name,
                                        std::span<NodeId const> gargs,
                                        std::span<NodeId const> args,
                                        NodeId ret, NodeId body) {}
    virtual void visit_after_func_decl(Ast& ast, Node const& node,
                                       std::span<NodeId const> decorators,
                                       NodeId                  name,
                                       std::span<NodeId const> gargs,
                                       std::span<NodeId const> args, NodeId ret,
                                       NodeId body) {}

    // -----------------------------------------------------------------------

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

    // =======================================================================

    virtual void visit_expr_pack(Ast& ast, Node const& node,
                                 std::span<NodeId const> children) {
        for (auto const& child : children) visit(ast, child);
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

#pragma GCC diagnostic pop
};

}  // namespace yal::ast
