#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "ast-node-conv.hpp"
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
                visit_module(node, conv::module(*ast, node));
                break;
            case NodeKind::SourceFile:
                visit_source_file(node, conv::source_file(*ast, node));
                break;

            case NodeKind::ModuleDecl:
                visit_module_decl(node, conv::module_decl(*ast, node).name);
                break;

            case NodeKind::FuncDecl:
            case NodeKind::FuncDeclWithCVarArgs:
                visit_func_decl(node, conv::func_decl(*ast, node));
                break;

            case NodeKind::TopVarDecl:
                visit_top_var_decl(node, conv::top_var_decl(*ast, node));
                break;

            case NodeKind::TopDefDecl:
                visit_top_def_decl(node, conv::top_def_decl(*ast, node));
                break;

            case NodeKind::IdPack:
                visit_id_pack(node, conv::id_pack(*ast, node));
                break;
            case NodeKind::FuncParam:
                visit_func_param(node, conv::func_param(*ast, node));
                break;
            case NodeKind::FuncRetPack:
                visit_func_ret_pack(node, conv::ret_pack(*ast, node));
                break;
            case NodeKind::Decorator:
                visit_decorator(node, conv::decorator(*ast, node));
                break;

            case NodeKind::ImportStmt:
                visit_import_stmt(node, conv::import_stmt(*ast, node));
                break;

            case NodeKind::ExprPack:
                visit_expr_pack(node, conv::expr_pack(*ast, node));
                break;

            case NodeKind::Add:
                visit_add(node, conv::binary(*ast, node));
                break;

            case NodeKind::Sub:
                visit_sub(node, conv::binary(*ast, node));
                break;

            case NodeKind::Mul:
                visit_mul(node, conv::binary(*ast, node));
                break;

            case NodeKind::Div:
                visit_div(node, conv::binary(*ast, node));
                break;

            case NodeKind::Mod:
                visit_mod(node, conv::binary(*ast, node));
                break;

            case NodeKind::LeftShift:
                visit_left_shift(node, conv::binary(*ast, node));
                break;

            case NodeKind::RightShift:
                visit_right_shift(node, conv::binary(*ast, node));
                break;

            case NodeKind::Equal:
                visit_equal(node, conv::binary(*ast, node));
                break;

            case NodeKind::NotEqual:
                visit_not_equal(node, conv::binary(*ast, node));
                break;

            case NodeKind::Less:
                visit_less(node, conv::binary(*ast, node));
                break;

            case NodeKind::LessEqual:
                visit_less_equal(node, conv::binary(*ast, node));
                break;

            case NodeKind::Greater:
                visit_greater(node, conv::binary(*ast, node));
                break;

            case NodeKind::GreaterEqual:
                visit_greater_equal(node, conv::binary(*ast, node));
                break;

            case NodeKind::Band:
                visit_band(node, conv::binary(*ast, node));
                break;

            case NodeKind::Bor:
                visit_bor(node, conv::binary(*ast, node));
                break;

            case NodeKind::Bxor:
                visit_bxor(node, conv::binary(*ast, node));
                break;

            case NodeKind::Land:
                visit_land(node, conv::binary(*ast, node));
                break;

            case NodeKind::Lor:
                visit_lor(node, conv::binary(*ast, node));
                break;

            case NodeKind::Cast:
                visit_cast(node, conv::binary(*ast, node));
                break;

            case NodeKind::AddrOf:
                visit_addrof(node, conv::unary(*ast, node));
                break;
            case NodeKind::Lnot:
                visit_lnot(node, conv::unary(*ast, node));
                break;
            case NodeKind::Bnot:
                visit_bnot(node, conv::unary(*ast, node));
                break;
            case NodeKind::Neg: visit_neg(node, conv::unary(*ast, node)); break;

            case NodeKind::StructType:
                visit_struct_type(node, conv::struct_type(*ast, node));
                break;
            case NodeKind::StructField:
                visit_struct_field(node, conv::struct_field(*ast, node));
                break;

            case NodeKind::PtrConst:
            case NodeKind::Ptr: visit_ptr(node, conv::ptr(*ast, node)); break;

            case NodeKind::MultiPtrConst:
            case NodeKind::MultiPtr:
                visit_mptr(node, conv::mptr(*ast, node));
                break;

            case NodeKind::SliceConst:
            case NodeKind::Slice:
                visit_slice(node, conv::slice(*ast, node));
                break;

            case NodeKind::ArrayTypeConst:
            case NodeKind::ArrayType:
                visit_array_type(node, conv::array_type(*ast, node));
                break;

            case NodeKind::Array:
                visit_array(node, conv::array(*ast, node));
                break;

            case NodeKind::Lit: visit_lit(node, conv::lit(*ast, node)); break;

            case NodeKind::Call:
                visit_call(node, conv::call(*ast, node));
                break;

            case NodeKind::Field:
                visit_field(node, conv::field(*ast, node));
                break;

            case NodeKind::Block:
                visit_block(node, conv::block(*ast, node));
                break;

            case NodeKind::ExprStmt:
                visit_expr_stmt(node, conv::unary(*ast, node));
                break;

            case NodeKind::ReturnStmt:
                visit_return_stmt(node, conv::unary(*ast, node));
                break;

            case NodeKind::IfStmt:
            case NodeKind::IfStmtWithElse:
                visit_if_stmt(node, conv::if_stmt(*ast, node));
                break;

            case NodeKind::WhileStmt:
                visit_while_stmt(node, conv::while_stmt(*ast, node));
                break;

            case NodeKind::Break: visit_break(node); break;
            case NodeKind::Continue: visit_continue(node); break;

            case NodeKind::DeferStmt:
                visit_defer_stmt(node, conv::defer_stmt(*ast, node));
                break;

            case NodeKind::VarDecl:
                visit_var_decl(node, conv::var_decl(*ast, node));
                break;

            case NodeKind::DefDecl:
                visit_def_decl(node, conv::def_decl(*ast, node));
                break;

            case NodeKind::Assign:
                visit_assign(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignAdd:
                visit_assign_add(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignSub:
                visit_assign_sub(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignMul:
                visit_assign_mul(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignDiv:
                visit_assign_div(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignMod:
                visit_assign_mod(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignShiftLeft:
                visit_assign_left_shift(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignShiftRight:
                visit_assign_right_shift(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignBand:
                visit_assign_band(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignBxor:
                visit_assign_bxor(node, conv::assign(*ast, node));
                break;
            case NodeKind::AssignBor:
                visit_assign_bor(node, conv::assign(*ast, node));
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

    virtual void visit_module(Node const& node, conv::Module const& data) {
        visit_before_module(node, data);
        for (auto const& child : data.children) visit(child);
        visit_after_module(node, data);
    }

    virtual void visit_before_module(Node const&         node,
                                     conv::Module const& data) {}
    virtual void visit_after_module(Node const&         node,
                                    conv::Module const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_source_file(Node const&             node,
                                   conv::SourceFile const& data) {
        visit_before_source_file(node, data);
        visit(data.mod);
        for (auto const& child : data.children) visit(child);
        visit_after_source_file(node, data);
    }

    virtual void visit_before_source_file(Node const&             node,
                                          conv::SourceFile const& data) {}
    virtual void visit_after_source_file(Node const&             node,
                                         conv::SourceFile const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_module_decl(Node const& node, std::string_view name) {}

    // -----------------------------------------------------------------------

    virtual void visit_func_decl(Node const& node, conv::FuncDecl const& data) {
        visit_before_func_decl(node, data);

        for (auto const& dec : data.decorators) visit(dec);
        visit(data.name);
        for (auto const& garg : data.gargs) visit(garg);
        for (auto const& arg : data.args) visit(arg);
        if (data.ret.is_valid()) visit(data.ret);
        if (data.body.is_valid()) visit(data.body);

        visit_after_func_decl(node, data);
    }

    virtual void visit_before_func_decl(Node const&           node,
                                        conv::FuncDecl const& data) {}
    virtual void visit_after_func_decl(Node const&           node,
                                       conv::FuncDecl const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_top_var_decl(Node const&             node,
                                    conv::TopVarDecl const& data) {
        visit_before_top_var_decl(node, data);
        for (auto const& dec : data.decorators) visit(dec);
        visit(data.decl);
        visit_after_top_var_decl(node, data);
    }

    virtual void visit_before_top_var_decl(Node const&             node,
                                           conv::TopVarDecl const& data) {}
    virtual void visit_after_top_var_decl(Node const&             node,
                                          conv::TopVarDecl const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_top_def_decl(Node const&             node,
                                    conv::TopDefDecl const& data) {
        visit_before_top_def_decl(node, data);
        for (auto const& dec : data.decorators) visit(dec);
        visit(data.decl);
        visit_after_top_def_decl(node, data);
    }

    virtual void visit_before_top_def_decl(Node const&             node,
                                           conv::TopDefDecl const& data) {}
    virtual void visit_after_top_def_decl(Node const&             node,
                                          conv::TopDefDecl const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_id_pack(Node const& node, conv::IdPack const& data) {}

    virtual void visit_func_param(Node const&            node,
                                  conv::FuncParam const& data) {
        if (data.type.is_valid()) visit(data.type);
    }

    virtual void visit_func_ret_pack(Node const&          node,
                                     conv::RetPack const& data) {
        auto ret = data.ret;
        ASSERT((ret.size() & 1) == 0);
        for (size_t i = 0; i < ret.size(); i += 2) {
            visit(ret[i + 1]);
        }
    }

    virtual void visit_decorator(Node const&            node,
                                 conv::Decorator const& data) {
        auto params = data.params;
        ASSERT((params.size() & 1) == 0);
        for (size_t i = 0; i < params.size(); i += 2) {
            if (params[i + 1].is_valid()) visit(params[i + 1]);
        }
    }

    virtual void visit_import_stmt(Node const&             node,
                                   conv::ImportStmt const& data) {}

    // =======================================================================

    virtual void visit_expr_pack(Node const& node, conv::ExprPack const& data) {
        for (auto const& child : data.items) visit(child);
    }

#define VISIT_BIN(name)                                                     \
    virtual void visit_##name(Node const& node, conv::Binary const& data) { \
        visit(data.lhs);                                                    \
        visit(data.rhs);                                                    \
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

#define VISIT_UNARY(name)                                                  \
    virtual void visit_##name(Node const& node, conv::Unary const& data) { \
        visit(data.child);                                                 \
    }

    VISIT_UNARY(addrof);
    VISIT_UNARY(lnot);
    VISIT_UNARY(bnot);
    VISIT_UNARY(neg);

#undef VISIT_UNARY

    virtual void visit_struct_type(Node const&             node,
                                   conv::StructType const& data) {
        for (auto const& field : data.fields) visit(field);
    }

    virtual void visit_struct_field(Node const&              node,
                                    conv::StructField const& data) {
        visit(data.type);
        if (data.init.is_valid()) visit(data.init);
    }

    virtual void visit_ptr(Node const& node, conv::Ptr const& data) {
        visit(data.inner);
    }

    virtual void visit_mptr(Node const& node, conv::MultiPtr const& data) {
        visit(data.inner);
    }

    virtual void visit_slice(Node const& node, conv::Slice const& data) {
        visit(data.inner);
    }

    virtual void visit_array_type(Node const&            node,
                                  conv::ArrayType const& data) {
        visit(data.size);
        visit(data.inner);
    }

    virtual void visit_array(Node const& node, conv::Array const& data) {
        visit(data.size);
        visit(data.inner);
        for (auto const& item : data.items) visit(item);
    }

    virtual void visit_lit(Node const& node, conv::Lit const& data) {
        for (auto const& item : data.items) visit(item);
    }

    virtual void visit_call(Node const& node, conv::Call const& data) {
        visit(data.callee);
        for (auto const& arg : data.args) visit(arg);
    }

    virtual void visit_field(Node const& node, conv::Field const& data) {
        visit(data.receiver);
    }

    // =======================================================================

    virtual void visit_block(Node const& node, conv::Block const& data) {
        visit_before_block(node, data);
        for (auto const& child : data.items) visit(child);
        visit_after_block(node, data);
    }

    virtual void visit_before_block(Node const& node, conv::Block const& data) {
    }
    virtual void visit_after_block(Node const& node, conv::Block const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_expr_stmt(Node const& node, conv::Unary const& data) {
        visit(data.child);
    }

    virtual void visit_return_stmt(Node const& node, conv::Unary const& data) {
        if (data.child.is_valid()) visit(data.child);
    }

    virtual void visit_if_stmt(Node const& node, conv::IfStmt const& data) {
        visit(data.cond);
        visit(data.wt);
        if (data.wf.is_valid()) visit(data.wf);
    }

    virtual void visit_while_stmt(Node const&            node,
                                  conv::WhileStmt const& data) {
        visit(data.cond);
        visit(data.body);
    }

    virtual void visit_break(Node const& node) {}
    virtual void visit_continue(Node const& node) {}

    virtual void visit_defer_stmt(Node const&            node,
                                  conv::DeferStmt const& data) {
        visit(data.stmt);
    }

    virtual void visit_var_decl(Node const& node, conv::VarDecl const& data) {
        visit(data.ids);
        if (data.types.is_valid()) visit(data.types);
        if (data.inits.is_valid()) visit(data.inits);
    }

    virtual void visit_def_decl(Node const& node, conv::DefDecl const& data) {
        visit(data.ids);
        if (data.types.is_valid()) visit(data.types);
        visit(data.inits);
    }

#define VISIT_ASSIGN(name)                                                  \
    virtual void visit_##name(Node const& node, conv::Assign const& data) { \
        visit(data.lhs);                                                    \
        visit(data.rhs);                                                    \
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
