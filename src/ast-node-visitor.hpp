#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "ast-node-conv.hpp"
#include "ast.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"

namespace yal::ast {

template <typename AstT = Ast const>
struct Visitor {
    explicit Visitor(AstT& ast) : ast{&ast} {}

    AstT* ast;

    void visit(Node* node) {
        if (node) visit(*node);
    }

    void visit(Node& node) {
        visit_before(node);

        switch (node.get_kind()) {
            case NodeKind::Err: visit_err(node); break;

            case NodeKind::Id: visit_id(node, conv::id(node)); break;
            case NodeKind::KwLit: visit_kw_lit(node, conv::kwlit(node)); break;
            case NodeKind::Int: visit_int(node, conv::integers(node)); break;
            case NodeKind::Double:
                visit_double(node, conv::float64(node));
                break;
            case NodeKind::Float: visit_float(node, conv::float32(node)); break;
            case NodeKind::Str: visit_str(node, conv::str(node)); break;
            case NodeKind::Char: visit_char(node, conv::character(node)); break;

            case NodeKind::Module:
                visit_module(node, conv::module(node));
                break;
            case NodeKind::SourceFile:
                visit_source_file(node, conv::source_file(node));
                break;

            case NodeKind::ModuleDecl:
                visit_module_decl(node, conv::module_decl(node).name);
                break;

            case NodeKind::FuncDecl:
            case NodeKind::FuncDeclWithCVarArgs:
                visit_func_decl(node, conv::func_decl(node));
                break;

            case NodeKind::FuncParams:
                visit_func_params(node, conv::func_params(node));
                break;

            case NodeKind::TopVarDecl:
                visit_top_var_decl(node, conv::top_var_decl(node));
                break;

            case NodeKind::TopDefDecl:
                visit_top_def_decl(node, conv::top_def_decl(node));
                break;

            case NodeKind::IdPack:
                visit_id_pack(node, conv::id_pack(node));
                break;
            case NodeKind::FuncParam:
                visit_func_param(node, conv::func_param(node));
                break;
            case NodeKind::FuncRetPack:
                visit_func_ret_pack(node, conv::func_ret_pack(node));
                break;
            case NodeKind::NamedRet:
                visit_named_ret(node, conv::named_ret(node));
                break;
            case NodeKind::Decorator:
                visit_decorator(node, conv::decorator(node));
                break;
            case NodeKind::DecoratorParam:
                visit_decorator_param(node, conv::decorator_param(node));
                break;
            case NodeKind::Decorators:
                visit_decorators(node, conv::decorators(node));
                break;

            case NodeKind::ImportStmt:
                visit_import_stmt(node, conv::import_stmt(node));
                break;

            case NodeKind::ExprPack:
                visit_expr_pack(node, conv::expr_pack(node));
                break;

            case NodeKind::Add: visit_add(node, conv::binary(node)); break;
            case NodeKind::Sub: visit_sub(node, conv::binary(node)); break;
            case NodeKind::Mul: visit_mul(node, conv::binary(node)); break;
            case NodeKind::Div: visit_div(node, conv::binary(node)); break;
            case NodeKind::Mod: visit_mod(node, conv::binary(node)); break;
            case NodeKind::LeftShift:
                visit_left_shift(node, conv::binary(node));
                break;
            case NodeKind::RightShift:
                visit_right_shift(node, conv::binary(node));
                break;
            case NodeKind::Equal: visit_equal(node, conv::binary(node)); break;
            case NodeKind::NotEqual:
                visit_not_equal(node, conv::binary(node));
                break;
            case NodeKind::Less: visit_less(node, conv::binary(node)); break;
            case NodeKind::LessEqual:
                visit_less_equal(node, conv::binary(node));
                break;
            case NodeKind::Greater:
                visit_greater(node, conv::binary(node));
                break;
            case NodeKind::GreaterEqual:
                visit_greater_equal(node, conv::binary(node));
                break;
            case NodeKind::Band: visit_band(node, conv::binary(node)); break;
            case NodeKind::Bor: visit_bor(node, conv::binary(node)); break;
            case NodeKind::Bxor: visit_bxor(node, conv::binary(node)); break;
            case NodeKind::Land: visit_land(node, conv::binary(node)); break;
            case NodeKind::Lor: visit_lor(node, conv::binary(node)); break;
            case NodeKind::Cast: visit_cast(node, conv::binary(node)); break;

            case NodeKind::AddrOf: visit_addrof(node, conv::unary(node)); break;
            case NodeKind::Lnot: visit_lnot(node, conv::unary(node)); break;
            case NodeKind::Bnot: visit_bnot(node, conv::unary(node)); break;
            case NodeKind::Neg: visit_neg(node, conv::unary(node)); break;

            case NodeKind::StructType:
                visit_struct_type(node, conv::struct_type(node));
                break;
            case NodeKind::StructField:
                visit_struct_field(node, conv::struct_field(node));
                break;

            case NodeKind::PtrConst:
            case NodeKind::Ptr: visit_ptr(node, conv::ptr(node)); break;

            case NodeKind::MultiPtrConst:
            case NodeKind::MultiPtr: visit_mptr(node, conv::mptr(node)); break;

            case NodeKind::SliceConst:
            case NodeKind::Slice: visit_slice(node, conv::slice(node)); break;

            case NodeKind::ArrayTypeConst:
            case NodeKind::ArrayType:
                visit_array_type(node, conv::array_type(node));
                break;

            case NodeKind::Array: visit_array(node, conv::array(node)); break;

            case NodeKind::Lit: visit_lit(node, conv::lit(node)); break;
            case NodeKind::LitParam:
                visit_lit_param(node, conv::lit_param(node));
                break;

            case NodeKind::Call: visit_call(node, conv::call(node)); break;

            case NodeKind::Field: visit_field(node, conv::field(node)); break;

            case NodeKind::Block: visit_block(node, conv::block(node)); break;

            case NodeKind::ExprStmt:
                visit_expr_stmt(node, conv::unary(node));
                break;

            case NodeKind::ReturnStmt:
                visit_return_stmt(node, conv::unary(node));
                break;

            case NodeKind::IfStmt:
                visit_if_stmt(node, conv::if_stmt(node));
                break;

            case NodeKind::WhileStmt:
                visit_while_stmt(node, conv::while_stmt(node));
                break;

            case NodeKind::Break: visit_break(node); break;
            case NodeKind::Continue: visit_continue(node); break;

            case NodeKind::DeferStmt:
                visit_defer_stmt(node, conv::defer_stmt(node));
                break;

            case NodeKind::VarDecl:
                visit_var_decl(node, conv::var_decl(node));
                break;

            case NodeKind::DefDecl:
                visit_def_decl(node, conv::def_decl(node));
                break;

            case NodeKind::Assign:
                visit_assign(node, conv::assign(node));
                break;
            case NodeKind::AssignAdd:
                visit_assign_add(node, conv::assign(node));
                break;
            case NodeKind::AssignSub:
                visit_assign_sub(node, conv::assign(node));
                break;
            case NodeKind::AssignMul:
                visit_assign_mul(node, conv::assign(node));
                break;
            case NodeKind::AssignDiv:
                visit_assign_div(node, conv::assign(node));
                break;
            case NodeKind::AssignMod:
                visit_assign_mod(node, conv::assign(node));
                break;
            case NodeKind::AssignShiftLeft:
                visit_assign_left_shift(node, conv::assign(node));
                break;
            case NodeKind::AssignShiftRight:
                visit_assign_right_shift(node, conv::assign(node));
                break;
            case NodeKind::AssignBand:
                visit_assign_band(node, conv::assign(node));
                break;
            case NodeKind::AssignBxor:
                visit_assign_bxor(node, conv::assign(node));
                break;
            case NodeKind::AssignBor:
                visit_assign_bor(node, conv::assign(node));
                break;
        }
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

    virtual void visit_before(Node& node) {}

    virtual void visit_err(Node& node) {}
    virtual void visit_id(Node& node, conv::Id const& id) {}
    virtual void visit_kw_lit(Node& node, conv::KwLit id) {}
    virtual void visit_int(Node& node, conv::Int value) {}
    virtual void visit_double(Node& node, conv::Double value) {}
    virtual void visit_float(Node& node, conv::Float value) {}
    virtual void visit_str(Node& node, conv::Str s) {}
    virtual void visit_char(Node& node, conv::Char character) {}

    // -----------------------------------------------------------------------

    virtual void visit_module(Node& node, conv::Module const& data) {
        visit_before_module(node, data);
        for (auto const& child : data.children) visit(child);
        visit_after_module(node, data);
    }

    virtual void visit_before_module(Node& node, conv::Module const& data) {}
    virtual void visit_after_module(Node& node, conv::Module const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_source_file(Node& node, conv::SourceFile const& data) {
        visit_before_source_file(node, data);
        visit(data.mod);
        for (auto const& child : data.children) visit(child);
        visit_after_source_file(node, data);
    }

    virtual void visit_before_source_file(Node&                   node,
                                          conv::SourceFile const& data) {}
    virtual void visit_after_source_file(Node&                   node,
                                         conv::SourceFile const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_module_decl(Node& node, std::string_view name) {}

    // -----------------------------------------------------------------------

    virtual void visit_func_decl(Node& node, conv::FuncDecl const& data) {
        visit_before_func_decl(node, data);

        visit_decorators(*data.decorators, data.get_decorators());
        visit_id_pack(*data.name, data.get_name());
        visit_func_params(*data.gargs, data.get_gargs());
        visit_func_params(*data.args, data.get_args());
        visit_func_ret_pack(*data.ret, data.get_ret());
        visit(data.body);

        visit_after_func_decl(node, data);
    }

    virtual void visit_before_func_decl(Node&                 node,
                                        conv::FuncDecl const& data) {}
    virtual void visit_after_func_decl(Node& node, conv::FuncDecl const& data) {
    }

    // -----------------------------------------------------------------------

    virtual void visit_func_params(Node& node, conv::FuncParams const& data) {
        for (auto const& param : data.params) visit(param);
    }

    // -----------------------------------------------------------------------

    virtual void visit_top_var_decl(Node& node, conv::TopVarDecl const& data) {
        visit_before_top_var_decl(node, data);

        visit_decorators(*data.decorators, data.get_decorators());
        visit_var_decl(*data.decl, data.get_decl());

        visit_after_top_var_decl(node, data);
    }

    virtual void visit_before_top_var_decl(Node&                   node,
                                           conv::TopVarDecl const& data) {}
    virtual void visit_after_top_var_decl(Node&                   node,
                                          conv::TopVarDecl const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_top_def_decl(Node& node, conv::TopDefDecl const& data) {
        visit_before_top_def_decl(node, data);
        visit_decorators(*data.decorators, data.get_decorators());
        visit_def_decl(*data.decl, data.get_decl());
        visit_after_top_def_decl(node, data);
    }

    virtual void visit_before_top_def_decl(Node&                   node,
                                           conv::TopDefDecl const& data) {}
    virtual void visit_after_top_def_decl(Node&                   node,
                                          conv::TopDefDecl const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_id_pack(Node& node, conv::IdPack const& data) {
        for (auto const& id : data.ids) visit(id);
    }

    virtual void visit_func_param(Node& node, conv::FuncParam const& data) {
        visit_before_func_param(node, data);
        visit(data.type);
        visit_after_func_param(node, data);
    }

    virtual void visit_before_func_param(Node&                  node,
                                         conv::FuncParam const& data) {}
    virtual void visit_after_func_param(Node&                  node,
                                        conv::FuncParam const& data) {}

    virtual void visit_func_ret_pack(Node&                    node,
                                     conv::FuncRetPack const& data) {
        visit_before_ret_pack(node, data);
        for (auto const& child : data.ret) visit(child);
        visit_after_ret_pack(node, data);
    }

    virtual void visit_before_ret_pack(Node&                    node,
                                       conv::FuncRetPack const& data) {}
    virtual void visit_after_ret_pack(Node&                    node,
                                      conv::FuncRetPack const& data) {}

    virtual void visit_named_ret(Node& node, conv::NamedRet const& data) {
        visit(data.type);
    }

    virtual void visit_decorator(Node& node, conv::Decorator const& data) {
        for (auto const& param : data.params) visit(param);
    }

    virtual void visit_decorator_param(Node&                       node,
                                       conv::DecoratorParam const& data) {
        visit(data.value);
    }

    virtual void visit_decorators(Node& node, conv::Decorators const& data) {
        for (auto const& item : data.items) visit(item);
    }

    virtual void visit_import_stmt(Node& node, conv::ImportStmt const& data) {}

    // =======================================================================

    virtual void visit_expr_pack(Node& node, conv::ExprPack const& data) {
        for (auto const& child : data.items) visit(child);
    }

#define VISIT_BIN(name)                                               \
    virtual void visit_##name(Node& node, conv::Binary const& data) { \
        visit(data.lhs);                                              \
        visit(data.rhs);                                              \
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

#define VISIT_UNARY(name)                                            \
    virtual void visit_##name(Node& node, conv::Unary const& data) { \
        visit(data.child);                                           \
    }

    VISIT_UNARY(addrof);
    VISIT_UNARY(lnot);
    VISIT_UNARY(bnot);
    VISIT_UNARY(neg);

#undef VISIT_UNARY

    virtual void visit_struct_type(Node& node, conv::StructType const& data) {
        for (auto const& field : data.fields) visit(field);
    }

    virtual void visit_struct_field(Node& node, conv::StructField const& data) {
        visit(data.type);
        visit(data.init);
    }

    virtual void visit_ptr(Node& node, conv::Ptr const& data) {
        visit(data.inner);
    }

    virtual void visit_mptr(Node& node, conv::MultiPtr const& data) {
        visit(data.inner);
    }

    virtual void visit_slice(Node& node, conv::Slice const& data) {
        visit(data.inner);
    }

    virtual void visit_array_type(Node& node, conv::ArrayType const& data) {
        visit(data.size);
        visit(data.inner);
    }

    virtual void visit_array(Node& node, conv::Array const& data) {
        visit(data.size);
        visit(data.inner);
        for (auto const& item : data.items) visit(item);
    }

    virtual void visit_lit(Node& node, conv::Lit const& data) {
        for (auto const& item : data.items) visit(item);
    }

    virtual void visit_lit_param(Node& node, conv::LitParam const& data) {
        visit(data.init);
    }

    virtual void visit_call(Node& node, conv::Call const& data) {
        visit(data.callee);
        for (auto const& arg : data.args) visit(arg);
    }

    virtual void visit_field(Node& node, conv::Field const& data) {
        visit(data.receiver);
    }

    // =======================================================================

    virtual void visit_block(Node& node, conv::Block const& data) {
        visit_before_block(node, data);
        for (auto const& child : data.items) visit(child);
        visit_after_block(node, data);
    }

    virtual void visit_before_block(Node& node, conv::Block const& data) {}
    virtual void visit_after_block(Node& node, conv::Block const& data) {}

    // -----------------------------------------------------------------------

    virtual void visit_expr_stmt(Node& node, conv::Unary const& data) {
        visit(data.child);
    }

    virtual void visit_return_stmt(Node& node, conv::Unary const& data) {
        visit(data.child);
    }

    virtual void visit_if_stmt(Node& node, conv::IfStmt const& data) {
        visit(data.cond);
        visit(data.wt);
        visit(data.wf);
    }

    virtual void visit_while_stmt(Node& node, conv::WhileStmt const& data) {
        visit(data.cond);
        visit(data.body);
    }

    virtual void visit_break(Node& node) {}
    virtual void visit_continue(Node& node) {}

    virtual void visit_defer_stmt(Node& node, conv::DeferStmt const& data) {
        visit(data.stmt);
    }

    virtual void visit_var_decl(Node& node, conv::VarDecl const& data) {
        visit_before_var_decl(node, data);
        visit(data.ids);
        visit(data.types);
        visit(data.inits);
        visit_after_var_decl(node, data);
    }

    virtual void visit_before_var_decl(Node& node, conv::VarDecl const& data) {}
    virtual void visit_after_var_decl(Node& node, conv::VarDecl const& data) {}

    virtual void visit_def_decl(Node& node, conv::DefDecl const& data) {
        visit_before_def_decl(node, data);
        visit(data.ids);
        visit(data.types);
        visit(data.inits);
        visit_after_def_decl(node, data);
    }

    virtual void visit_before_def_decl(Node& node, conv::DefDecl const& data) {}
    virtual void visit_after_def_decl(Node& node, conv::DefDecl const& data) {}

#define VISIT_ASSIGN(name)                                            \
    virtual void visit_##name(Node& node, conv::Assign const& data) { \
        visit_expr_pack(*data.lhs, data.get_lhs());                   \
        visit_expr_pack(*data.rhs, data.get_rhs());                   \
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
