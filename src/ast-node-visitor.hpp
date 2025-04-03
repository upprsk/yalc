#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "ast-node-id.hpp"
#include "ast.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"

namespace yal::ast {

template <typename AstT = Ast const>
struct Visitor {
    explicit Visitor(AstT& ast) : ast{&ast} {}

    AstT* ast;

    void visit(NodeId node_id) {
        ASSERT(node_id.is_valid());

        auto node = ast->get_node(node_id.as_ref());
        visit_before(node);

        switch (node.get_kind()) {
            case NodeKind::Err: visit_err(node); break;

            case NodeKind::Id:
                visit_id(node, ast->get_identifier(node.get_first().as_id()));
                break;
            case NodeKind::KwLit:
                visit_kw_lit(node,
                             ast->get_identifier(node.get_first().as_id()));
                break;
            case NodeKind::Int: visit_int(node, node.cast_u64()); break;
            case NodeKind::Double:
                visit_double(node, node.cast_double());
                break;
            case NodeKind::Float: visit_float(node, node.cast_float()); break;
            case NodeKind::Str:
                visit_str(node, ast->get_bytes_as_string_view(
                                    node.get_first().as_bytes()));
                break;
            case NodeKind::Char: visit_char(node, node.cast_u32()); break;

            case NodeKind::Module:
                visit_module(
                    node,
                    ast->get_bytes_as_string_view(node.get_first().as_bytes()),
                    ast->get_array(node.get_second().as_array()));
                break;
            case NodeKind::SourceFile:
                visit_source_file(node, node.get_first(),
                                  ast->get_array(node.get_second().as_array()));
                break;

            case NodeKind::ModuleDecl:
                visit_module_decl(
                    node, ast->get_identifier(node.get_first().as_id()));
                break;

            case NodeKind::FuncDecl: {
                auto second =
                    ast->get_array_unbounded(node.get_second().as_array());
                uint32_t idx{};

                // NOTE: no bounds check because of span missing the `.at`
                // method. :(
                auto dlen = second[idx++].as_count().value;
                auto decorators = second.subspan(idx, dlen);
                idx += dlen;

                auto glen = second[idx++].as_count().value;
                auto gargs = second.subspan(idx, glen);
                idx += glen;

                auto alen = second[idx++].as_count().value;
                auto args = second.subspan(idx, alen);
                idx += alen;

                auto ret = second[idx++];
                auto body = second[idx++];

                visit_func_decl(node, decorators, node.get_first(), gargs, args,
                                ret, body, false);
            } break;

            case NodeKind::FuncDeclWithCVarArgs: {
                auto second =
                    ast->get_array_unbounded(node.get_second().as_array());
                uint32_t idx{};

                // NOTE: no bounds check because of span missing the `.at`
                // method. :(
                auto dlen = second[idx++].as_count().value;
                auto decorators = second.subspan(idx, dlen);
                idx += dlen;

                auto glen = second[idx++].as_count().value;
                auto gargs = second.subspan(idx, glen);
                idx += glen;

                auto alen = second[idx++].as_count().value;
                auto args = second.subspan(idx, alen);
                idx += alen;

                auto ret = second[idx++];
                auto body = second[idx++];

                visit_func_decl(node, decorators, node.get_first(), gargs, args,
                                ret, body, true);
            } break;

            case NodeKind::TopVarDecl:
                visit_top_var_decl(node,
                                   ast->get_array(node.get_second().as_array()),
                                   node.get_first());
                break;

            case NodeKind::TopDefDecl:
                visit_top_def_decl(node,
                                   ast->get_array(node.get_second().as_array()),
                                   node.get_first());
                break;

            case NodeKind::IdPack:
                visit_id_pack(node,
                              ast->get_array(node.get_first().as_count(),
                                             node.get_second().as_array()));
                break;
            case NodeKind::FuncParam:
                visit_func_param(node,
                                 ast->get_identifier(node.get_first().as_id()),
                                 node.get_second());
                break;
            case NodeKind::FuncRetPack:
                visit_func_ret_pack(
                    node, ast->get_array(node.get_first().as_count().of_kv(),
                                         node.get_second().as_array()));
                break;
            case NodeKind::Decorator:
                visit_decorator(
                    node, ast->get_identifier(node.get_first().as_id()),
                    ast->get_array_of_kv(node.get_second().as_array()));
                break;

            case NodeKind::ExprPack:
                visit_expr_pack(node,
                                ast->get_array(node.get_first().as_count(),
                                               node.get_second().as_array()));
                break;

            case NodeKind::Add:
                visit_add(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Sub:
                visit_sub(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Mul:
                visit_mul(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Div:
                visit_div(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Mod:
                visit_mod(node, node.get_first(), node.get_second());
                break;

            case NodeKind::LeftShift:
                visit_left_shift(node, node.get_first(), node.get_second());
                break;

            case NodeKind::RightShift:
                visit_right_shift(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Equal:
                visit_equal(node, node.get_first(), node.get_second());
                break;

            case NodeKind::NotEqual:
                visit_not_equal(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Less:
                visit_less(node, node.get_first(), node.get_second());
                break;

            case NodeKind::LessEqual:
                visit_less_equal(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Greater:
                visit_greater(node, node.get_first(), node.get_second());
                break;

            case NodeKind::GreaterEqual:
                visit_greater_equal(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Band:
                visit_band(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Bor:
                visit_bor(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Bxor:
                visit_bxor(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Land:
                visit_land(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Lor:
                visit_lor(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Cast:
                visit_cast(node, node.get_first(), node.get_second());
                break;

            case NodeKind::AddrOf: visit_addrof(node, node.get_first()); break;
            case NodeKind::Lnot: visit_lnot(node, node.get_first()); break;
            case NodeKind::Bnot: visit_bnot(node, node.get_first()); break;
            case NodeKind::Neg: visit_neg(node, node.get_first()); break;

            case NodeKind::StructType:
                visit_struct_type(node,
                                  ast->get_array(node.get_first().as_count(),
                                                 node.get_second().as_array()));
                break;
            case NodeKind::StructField: {
                auto parts = ast->get_array({2}, node.get_second().as_array());
                visit_struct_field(
                    node, ast->get_identifier(node.get_first().as_id()),
                    parts[0], parts[1]);
            } break;

            case NodeKind::PtrConst:
                visit_ptr(node, true, node.get_first());
                break;
            case NodeKind::Ptr: visit_ptr(node, false, node.get_first()); break;

            case NodeKind::MultiPtrConst:
                visit_mptr(node, true, node.get_first());
                break;
            case NodeKind::MultiPtr:
                visit_mptr(node, false, node.get_first());
                break;

            case NodeKind::SliceConst:
                visit_slice(node, true, node.get_first());
                break;
            case NodeKind::Slice:
                visit_slice(node, false, node.get_first());
                break;

            case NodeKind::ArrayTypeConst:
                visit_array_type(node, true, node.get_second(),
                                 node.get_first());
                break;
            case NodeKind::ArrayType:
                visit_array_type(node, false, node.get_second(),
                                 node.get_first());
                break;

            case NodeKind::Array: {
                auto second =
                    ast->get_array_unbounded(node.get_second().as_array());
                auto size = second[0];
                auto items = second.subspan(2, second[1].as_count().value);
                visit_array(node, size, node.get_first(), items);
            } break;

            case NodeKind::Lit:
                visit_lit(node,
                          ast->get_array(node.get_first().as_count().of_kv(),
                                         node.get_second().as_array()));
                break;

            case NodeKind::Call:
                visit_call(node, node.get_first(),
                           ast->get_array(node.get_second().as_array()));
                break;

            case NodeKind::Field:
                visit_field(node, node.get_first(),
                            ast->get_identifier(node.get_second().as_id()));
                break;

            case NodeKind::Block:
                visit_block(node, ast->get_array(node.get_first().as_count(),
                                                 node.get_second().as_array()));
                break;

            case NodeKind::ExprStmt:
                visit_expr_stmt(node, node.get_first());
                break;

            case NodeKind::ReturnStmt:
                visit_return_stmt(node, node.get_first());
                break;

            case NodeKind::IfStmt:
                visit_if_stmt(node, node.get_first(), node.get_second(),
                              NodeId::invalid());
                break;

            case NodeKind::IfStmtWithElse: {
                auto second = ast->get_array({2}, node.get_second().as_array());
                visit_if_stmt(node, node.get_first(), second[0], second[1]);
            } break;

            case NodeKind::WhileStmt:
                visit_while_stmt(node, node.get_first(), node.get_second());
                break;

            case NodeKind::Break: visit_break(node); break;
            case NodeKind::Continue: visit_continue(node); break;

            case NodeKind::VarDecl: {
                auto parts = ast->get_array({2}, node.get_second().as_array());
                visit_var_decl(node, node.get_first(), parts[0], parts[1]);
            } break;

            case NodeKind::DefDecl: {
                auto parts = ast->get_array({2}, node.get_second().as_array());
                visit_def_decl(node, node.get_first(), parts[0], parts[1]);
            } break;

            case NodeKind::Assign:
                visit_assign(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignAdd:
                visit_assign_add(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignSub:
                visit_assign_sub(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignMul:
                visit_assign_mul(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignDiv:
                visit_assign_div(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignMod:
                visit_assign_mod(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignShiftLeft:
                visit_assign_left_shift(node, node.get_first(),
                                        node.get_second());
                break;
            case NodeKind::AssignShiftRight:
                visit_assign_right_shift(node, node.get_first(),
                                         node.get_second());
                break;
            case NodeKind::AssignBand:
                visit_assign_band(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignBxor:
                visit_assign_bxor(node, node.get_first(), node.get_second());
                break;
            case NodeKind::AssignBor:
                visit_assign_bor(node, node.get_first(), node.get_second());
                break;
        }
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

    virtual void visit_before(Node const& node) {}

    virtual void visit_err(Node const& node) {}
    virtual void visit_id(Node const& node, std::string_view id) {}
    virtual void visit_kw_lit(Node const& node, std::string_view id) {}
    virtual void visit_int(Node const& node, uint64_t value) {}
    virtual void visit_double(Node const& node, double value) {}
    virtual void visit_float(Node const& node, float value) {}
    virtual void visit_str(Node const& node, std::string_view s) {}
    virtual void visit_char(Node const& node, uint32_t character) {}

    // -----------------------------------------------------------------------

    virtual void visit_module(Node const& node, std::string_view name,
                              std::span<NodeId const> children) {
        visit_before_module(node, name, children);
        for (auto const& child : children) visit(child);
        visit_after_module(node, name, children);
    }

    virtual void visit_before_module(Node const& node, std::string_view name,
                                     std::span<NodeId const> children) {}
    virtual void visit_after_module(Node const& node, std::string_view name,
                                    std::span<NodeId const> children) {}

    // -----------------------------------------------------------------------

    virtual void visit_source_file(Node const& node, NodeId mod,
                                   std::span<NodeId const> children) {
        visit_before_source_file(node, mod, children);
        visit(mod);
        for (auto const& child : children) visit(child);
        visit_after_source_file(node, mod, children);
    }

    virtual void visit_before_source_file(Node const& node, NodeId mod,
                                          std::span<NodeId const> children) {}
    virtual void visit_after_source_file(Node const& node, NodeId mod,
                                         std::span<NodeId const> children) {}

    // -----------------------------------------------------------------------

    virtual void visit_module_decl(Node const& node, std::string_view name) {}

    // -----------------------------------------------------------------------

    virtual void visit_func_decl(Node const&             node,
                                 std::span<NodeId const> decorators,
                                 NodeId name, std::span<NodeId const> gargs,
                                 std::span<NodeId const> args, NodeId ret,
                                 NodeId body, bool is_c_varargs) {
        visit_before_func_decl(node, decorators, name, gargs, args, ret, body,
                               is_c_varargs);

        for (auto const& dec : decorators) visit(dec);
        visit(name);
        for (auto const& garg : gargs) visit(garg);
        for (auto const& arg : args) visit(arg);
        if (ret.is_valid()) visit(ret);
        if (body.is_valid()) visit(body);

        visit_after_func_decl(node, decorators, name, gargs, args, ret, body,
                              is_c_varargs);
    }

    virtual void visit_before_func_decl(
        Node const& node, std::span<NodeId const> decorators, NodeId name,
        std::span<NodeId const> gargs, std::span<NodeId const> args, NodeId ret,
        NodeId body, bool is_c_varargs) {}
    virtual void visit_after_func_decl(Node const&             node,
                                       std::span<NodeId const> decorators,
                                       NodeId                  name,
                                       std::span<NodeId const> gargs,
                                       std::span<NodeId const> args, NodeId ret,
                                       NodeId body, bool is_c_varargs) {}

    // -----------------------------------------------------------------------

    virtual void visit_top_var_decl(Node const&             node,
                                    std::span<NodeId const> decorators,
                                    NodeId                  child) {
        visit_before_top_var_decl(node, decorators, child);
        for (auto const& dec : decorators) visit(dec);
        visit(child);
        visit_after_top_var_decl(node, decorators, child);
    }

    virtual void visit_before_top_var_decl(Node const&             node,
                                           std::span<NodeId const> decorators,
                                           NodeId                  child) {}

    virtual void visit_after_top_var_decl(Node const&             node,
                                          std::span<NodeId const> decorators,
                                          NodeId                  child) {}

    // -----------------------------------------------------------------------

    virtual void visit_top_def_decl(Node const&             node,
                                    std::span<NodeId const> decorators,
                                    NodeId                  child) {
        visit_before_top_def_decl(node, decorators, child);
        for (auto const& dec : decorators) visit(dec);
        visit(child);
        visit_after_top_def_decl(node, decorators, child);
    }

    virtual void visit_before_top_def_decl(Node const&             node,
                                           std::span<NodeId const> decorators,
                                           NodeId                  child) {}

    virtual void visit_after_top_def_decl(Node const&             node,
                                          std::span<NodeId const> decorators,
                                          NodeId                  child) {}

    // -----------------------------------------------------------------------

    // NOTE: each id in `ids` points to an identifier, not an AST node
    virtual void visit_id_pack(Node const& node, std::span<NodeId const> ids) {}

    virtual void visit_func_param(Node const& node, std::string_view name,
                                  NodeId type) {}

    // NOTE: each id in `ret` points to either an identifier or an AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs of return value name and type. The name may be an invalid
    // id, for when the return value is not named.
    virtual void visit_func_ret_pack(Node const&             node,
                                     std::span<NodeId const> ret) {}

    // NOTE: each id in `params` points to either an identifier or an AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs. The name may be an invalid id, for when a raw value is
    // passed. The value may be an invalid id, for when no value is passed.
    virtual void visit_decorator(Node const& node, std::string_view name,
                                 std::span<NodeId const> params) {}

    // =======================================================================

    virtual void visit_expr_pack(Node const&             node,
                                 std::span<NodeId const> children) {
        for (auto const& child : children) visit(child);
    }

#define VISIT_BIN(name)                                                   \
    virtual void visit_##name(Node const& node, NodeId lhs, NodeId rhs) { \
        visit(lhs);                                                       \
        visit(rhs);                                                       \
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

#define VISIT_UNARY(name) \
    virtual void visit_##name(Node const& node, NodeId lhs) { visit(lhs); }

    VISIT_UNARY(addrof);
    VISIT_UNARY(lnot);
    VISIT_UNARY(bnot);
    VISIT_UNARY(neg);

#undef VISIT_UNARY

    virtual void visit_struct_type(Node const&             node,
                                   std::span<NodeId const> fields) {
        for (auto const& field : fields) visit(field);
    }

    virtual void visit_struct_field(Node const& node, std::string_view name,
                                    NodeId type, NodeId init) {
        visit(type);
        if (init.is_valid()) visit(init);
    }

    virtual void visit_ptr(Node const& node, bool is_const, NodeId inner) {
        visit(inner);
    }

    virtual void visit_mptr(Node const& node, bool is_const, NodeId inner) {
        visit(inner);
    }

    virtual void visit_slice(Node const& node, bool is_const, NodeId inner) {
        visit(inner);
    }

    virtual void visit_array_type(Node const& node, bool is_const, NodeId size,
                                  NodeId inner) {
        visit(size);
        visit(inner);
    }

    virtual void visit_array(Node const& node, NodeId size, NodeId inner,
                             std::span<NodeId const> items) {
        visit(size);
        visit(inner);
        for (auto const& item : items) visit(item);
    }

    // NOTE: each id in `params` points to either the key or the value AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs. The key may be an invalid id, for when a value is given
    // without a key.
    virtual void visit_lit(Node const& node, std::span<NodeId const> items) {
        for (auto const& item : items) visit(item);
    }

    virtual void visit_call(Node const& node, NodeId callee,
                            std::span<NodeId const> args) {
        visit(callee);
        for (auto const& arg : args) visit(arg);
    }

    virtual void visit_field(Node const& node, NodeId receiver,
                             std::string_view name) {
        visit(receiver);
    }

    // =======================================================================

    virtual void visit_block(Node const&             node,
                             std::span<NodeId const> children) {
        visit_before_block(node, children);
        for (auto const& child : children) visit(child);
        visit_after_block(node, children);
    }

    virtual void visit_before_block(Node const&             node,
                                    std::span<NodeId const> children) {}
    virtual void visit_after_block(Node const&             node,
                                   std::span<NodeId const> children) {}

    // -----------------------------------------------------------------------

    virtual void visit_expr_stmt(Node const& node, NodeId child) {
        visit(child);
    }

    virtual void visit_return_stmt(Node const& node, NodeId child) {
        if (child.is_valid()) visit(child);
    }

    // NOTE: `wf` may be an invalid id in case the if does not have an else
    virtual void visit_if_stmt(Node const& node, NodeId cond, NodeId wt,
                               NodeId wf) {
        visit(cond);
        visit(wt);
        if (wf.is_valid()) visit(wf);
    }

    virtual void visit_while_stmt(Node const& node, NodeId cond, NodeId body) {
        visit(cond);
        visit(body);
    }

    virtual void visit_break(Node const& node) {}
    virtual void visit_continue(Node const& node) {}

    virtual void visit_var_decl(Node const& node, NodeId ids, NodeId types,
                                NodeId inits) {
        visit(ids);
        if (types.is_valid()) visit(types);
        if (inits.is_valid()) visit(inits);
    }

    virtual void visit_def_decl(Node const& node, NodeId ids, NodeId types,
                                NodeId inits) {
        visit(ids);
        if (types.is_valid()) visit(types);
        visit(inits);
    }

#define VISIT_ASSIGN(name)                                                \
    virtual void visit_##name(Node const& node, NodeId lhs, NodeId rhs) { \
        visit(lhs);                                                       \
        visit(rhs);                                                       \
    }

    VISIT_ASSIGN(assign);
    VISIT_ASSIGN(assign_add);
    VISIT_ASSIGN(assign_sub);
    VISIT_ASSIGN(assign_mul);
    VISIT_ASSIGN(assign_div);
    VISIT_ASSIGN(assign_mod);
    VISIT_ASSIGN(assign_left_shift);
    VISIT_ASSIGN(assign_right_shift);
    VISIT_ASSIGN(assign_band);
    VISIT_ASSIGN(assign_bxor);
    VISIT_ASSIGN(assign_bor);

#undef VISIT_ASSIGN

#pragma GCC diagnostic pop
};

}  // namespace yal::ast
