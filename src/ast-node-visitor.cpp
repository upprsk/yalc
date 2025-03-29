#include "ast-node-visitor.hpp"

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
                            args, ret, body);
        } break;

        case NodeKind::FuncId:
            visit_func_id(ast, node,
                          ast.get_array(node.get_first().as_count(),
                                        node.get_second().as_array()));
            break;
        case NodeKind::FuncParam:
            visit_func_param(ast, node,
                             ast.get_identifier(node.get_first().as_id()),
                             node.get_second());
            break;

        case NodeKind::Block:
            visit_block(ast, node,
                        ast.get_array(node.get_first().as_count(),
                                      node.get_second().as_array()));
            break;
    }
}

}  // namespace yal::ast
