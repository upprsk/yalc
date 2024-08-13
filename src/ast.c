#include "ast.h"

#include <stdio.h>

#include "da.h"

static inline void indent_by_2(FILE* f, int indent) {
    for (int i = 0; i < indent; ++i) fprintf(f, "  ");
}

void dump_node(FILE* f, node_t* node, int indent) {
    indent_by_2(f, indent);

    if (!node) {
        fprintf(f, "<null>\n");
        return;
    }

    switch (node->type) {
        case NODE_ERR: fprintf(f, "ERR\n"); break;
        case NODE_INT: fprintf(f, "INT (%lu)\n", node->as.int_.value); break;
        case NODE_FLOAT:
            fprintf(f, "FLOAT (%f)\n", node->as.float_.value);
            break;
        case NODE_IDENT:
            fprintf(f, "IDENT '%s'\n", node->as.ident.ident);
            break;
        case NODE_BINOP:
            fprintf(f, "BINOP %s\n", binop_to_str(node->as.binop.type));
            dump_node(f, node->as.binop.left, indent + 1);
            dump_node(f, node->as.binop.right, indent + 1);
            break;
        case NODE_UNOP:
            fprintf(f, "UNOP %s\n", unop_to_str(node->as.unop.type));
            dump_node(f, node->as.unop.child, indent + 1);
            break;
        case NODE_STMT_EXPR:
            fprintf(f, "STMT EXPR ");
            dump_node(f, node->as.stmt_expr.expr, indent + 1);
            break;
        case NODE_STMT_RET:
            fprintf(f, "STMT RET ");
            dump_node(f, node->as.stmt_ret.child, indent + 1);
            break;
        case NODE_STMT_BLK: {
            fprintf(f, "STMT BLK\n");

            uint32_t size = da_get_size(node->as.stmt_blk.stmts);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.stmt_blk.stmts[i], indent + 1);
            }
        } break;
        case NODE_DECL:
            fprintf(f, "DECL \"%s\"\n", node->as.decl.name);
            dump_node(f, node->as.decl.type, indent + 1);
            dump_node(f, node->as.decl.init, indent + 1);
            break;
        case NODE_ASSIGN:
            fprintf(f, "ASSIGN\n");
            dump_node(f, node->as.assign.lhs, indent + 1);
            dump_node(f, node->as.assign.rhs, indent + 1);
            break;
        case NODE_ARG:
            fprintf(f, "ARG \"%s\"\n", node->as.arg.name);
            dump_node(f, node->as.arg.type, indent + 1);
            break;
        case NODE_PROC: {
            fprintf(f, "PROC\n");

            uint32_t size = da_get_size(node->as.proc.args);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.proc.args[i], indent + 1);
            }

            dump_node(f, node->as.proc.return_type, indent + 1);
            dump_node(f, node->as.proc.body, indent + 1);
        } break;
    }
}
