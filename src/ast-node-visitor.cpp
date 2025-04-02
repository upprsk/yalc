#include "ast-node-visitor.hpp"

#include "ast-node.hpp"

namespace yal::ast {

void Visitor::visit(Ast& ast, NodeId node_id) {
    ASSERT(node_id.is_valid());

    auto node = ast.get_node(node_id.as_ref());
    visit_before(ast, node);

    switch (node.get_kind()) {
        case NodeKind::Err: visit_err(ast, node); break;

        case NodeKind::Id:
            visit_id(ast, node, ast.get_identifier(node.get_first().as_id()));
            break;
        case NodeKind::Int: visit_int(ast, node, node.cast_u64()); break;
        case NodeKind::Double:
            visit_double(ast, node, node.cast_double());
            break;
        case NodeKind::Float: visit_float(ast, node, node.cast_float()); break;
        case NodeKind::Str:
            visit_str(
                ast, node,
                ast.get_bytes_as_string_view(node.get_first().as_bytes()));
            break;
        case NodeKind::Char: visit_char(ast, node, node.cast_u32()); break;

        case NodeKind::Module:
            visit_module(
                ast, node,
                ast.get_bytes_as_string_view(node.get_first().as_bytes()),
                ast.get_array(node.get_second().as_array()));
            break;
        case NodeKind::SourceFile:
            visit_source_file(ast, node, node.get_first(),
                              ast.get_array(node.get_second().as_array()));
            break;

        case NodeKind::ModuleDecl:
            visit_module_decl(ast, node,
                              ast.get_identifier(node.get_first().as_id()));
            break;

        case NodeKind::FuncDecl: {
            auto second = ast.get_array_unbounded(node.get_second().as_array());
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

            visit_func_decl(ast, node, decorators, node.get_first(), gargs,
                            args, ret, body, false);
        } break;

        case NodeKind::FuncDeclWithCVarArgs: {
            auto second = ast.get_array_unbounded(node.get_second().as_array());
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

            visit_func_decl(ast, node, decorators, node.get_first(), gargs,
                            args, ret, body, true);
        } break;

        case NodeKind::TopVarDecl:
            visit_top_var_decl(ast, node,
                               ast.get_array(node.get_second().as_array()),
                               node.get_first());
            break;

        case NodeKind::TopDefDecl:
            visit_top_def_decl(ast, node,
                               ast.get_array(node.get_second().as_array()),
                               node.get_first());
            break;

        case NodeKind::IdPack:
            visit_id_pack(ast, node,
                          ast.get_array(node.get_first().as_count(),
                                        node.get_second().as_array()));
            break;
        case NodeKind::FuncParam:
            visit_func_param(ast, node,
                             ast.get_identifier(node.get_first().as_id()),
                             node.get_second());
            break;
        case NodeKind::FuncRetPack:
            visit_func_ret_pack(
                ast, node,
                ast.get_array(node.get_first().as_count().of_kv(),
                              node.get_second().as_array()));
            break;
        case NodeKind::Decorator:
            visit_decorator(ast, node,
                            ast.get_identifier(node.get_first().as_id()),
                            ast.get_array_of_kv(node.get_second().as_array()));
            break;

        case NodeKind::ExprPack:
            visit_expr_pack(ast, node,
                            ast.get_array(node.get_first().as_count(),
                                          node.get_second().as_array()));
            break;

        case NodeKind::Add:
            visit_add(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Sub:
            visit_sub(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Mul:
            visit_mul(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Div:
            visit_div(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Mod:
            visit_mod(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::LeftShift:
            visit_left_shift(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::RightShift:
            visit_right_shift(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Equal:
            visit_equal(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::NotEqual:
            visit_not_equal(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Less:
            visit_less(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::LessEqual:
            visit_less_equal(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Greater:
            visit_greater(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::GreaterEqual:
            visit_greater_equal(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Band:
            visit_band(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Bor:
            visit_bor(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Bxor:
            visit_bxor(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Land:
            visit_land(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Lor:
            visit_lor(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::Cast:
            visit_cast(ast, node, node.get_first(), node.get_second());
            break;

        case NodeKind::AddrOf: visit_addrof(ast, node, node.get_first()); break;
        case NodeKind::Lnot: visit_lnot(ast, node, node.get_first()); break;
        case NodeKind::Bnot: visit_bnot(ast, node, node.get_first()); break;
        case NodeKind::Neg: visit_neg(ast, node, node.get_first()); break;

        case NodeKind::StructType:
            visit_struct_type(ast, node,
                              ast.get_array(node.get_first().as_count(),
                                            node.get_second().as_array()));
            break;
        case NodeKind::StructField: {
            auto parts = ast.get_array({2}, node.get_second().as_array());
            visit_struct_field(ast, node,
                               ast.get_identifier(node.get_first().as_id()),
                               parts[0], parts[1]);
        } break;

        case NodeKind::Block:
            visit_block(ast, node,
                        ast.get_array(node.get_first().as_count(),
                                      node.get_second().as_array()));
            break;

        case NodeKind::ExprStmt:
            visit_expr_stmt(ast, node, node.get_first());
            break;

        case NodeKind::ReturnStmt:
            visit_return_stmt(ast, node, node.get_first());
            break;

        case NodeKind::VarDecl: {
            auto parts = ast.get_array({2}, node.get_second().as_array());
            visit_var_decl(ast, node, node.get_first(), parts[0], parts[1]);
        } break;

        case NodeKind::DefDecl: {
            auto parts = ast.get_array({2}, node.get_second().as_array());
            visit_def_decl(ast, node, node.get_first(), parts[0], parts[1]);
        } break;
    }
}

}  // namespace yal::ast
