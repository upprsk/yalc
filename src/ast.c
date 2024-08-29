#include "ast.h"

#include <stdio.h>

#include "da.h"
#include "typestore.h"

static inline void indent_by_2(FILE* f, int indent) {
    for (int i = 0; i < indent; ++i) fprintf(f, "  ");
}

void dump_node(FILE* f, node_t* node, int indent) {
    indent_by_2(f, indent);

    if (!node) {
        fprintf(f, "<null>\n");
        return;
    }

    if (type_id_is_valid(node->type_id)) {
        fprintf(f, "[%d] ", node->type_id.id);
    }

    switch (node->type) {
        case NODE_ERR: fprintf(f, "ERR\n"); break;
        case NODE_INT: fprintf(f, "INT (%lu)\n", node->as.int_.value); break;
        case NODE_FLOAT:
            fprintf(f, "FLOAT (%f)\n", node->as.float_.value);
            break;
        case NODE_STR:
            fprintf(f, "STR \"%.*s\"\n", node->as.str.len, node->as.str.str);
            break;
        case NODE_IDENT:
            fprintf(f, "IDENT '%s'\n", node->as.ident.ident);
            break;
        case NODE_KW: fprintf(f, "KW %s\n", node->as.kw.ident); break;
        case NODE_BINOP:
            fprintf(f, "BINOP %s\n", binop_to_str(node->as.binop.type));
            dump_node(f, node->as.binop.left, indent + 1);
            dump_node(f, node->as.binop.right, indent + 1);
            break;
        case NODE_UNOP:
            fprintf(f, "UNOP %s\n", unop_to_str(node->as.unop.type));
            dump_node(f, node->as.unop.child, indent + 1);
            break;
        case NODE_LOGIC:
            fprintf(f, "LOGIC %s\n", logic_to_str(node->as.logic.type));
            dump_node(f, node->as.logic.left, indent + 1);
            dump_node(f, node->as.logic.right, indent + 1);
            break;
        case NODE_COMP:
            fprintf(f, "COMP %s\n", comp_to_str(node->as.comp.type));
            dump_node(f, node->as.comp.left, indent + 1);
            dump_node(f, node->as.comp.right, indent + 1);
            break;
        case NODE_INDEX:
            fprintf(f, "index\n");
            dump_node(f, node->as.index.receiver, indent + 1);
            dump_node(f, node->as.index.index, indent + 1);
            break;
        case NODE_REF:
            fprintf(f, "REF\n");
            dump_node(f, node->as.ref.child, indent + 1);
            break;
        case NODE_CALL: {
            fprintf(f, "CALL\n");
            dump_node(f, node->as.call.callee, indent + 1);

            uint32_t size = da_get_size(node->as.call.args);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.call.args[i], indent + 1);
            }
        } break;
        case NODE_DEREF:
            fprintf(f, "DEREF\n");
            dump_node(f, node->as.deref.child, indent + 1);
            break;
        case NODE_CAST:
            fprintf(f, "CAST\n");
            dump_node(f, node->as.cast.child, indent + 1);
            dump_node(f, node->as.cast.type, indent + 1);
            break;
        case NODE_STMT_EXPR:
            fprintf(f, "STMT EXPR\n");
            dump_node(f, node->as.stmt_expr.expr, indent + 1);
            break;
        case NODE_STMT_RET:
            fprintf(f, "STMT RET\n");
            dump_node(f, node->as.stmt_ret.child, indent + 1);
            break;
        case NODE_STMT_BREAK:
            fprintf(f, "STMT BREAK\n");
            dump_node(f, node->as.stmt_break.child, indent + 1);
            break;
        case NODE_STMT_IF:
            fprintf(f, "STMT IF\n");
            dump_node(f, node->as.stmt_if.condition, indent + 1);
            dump_node(f, node->as.stmt_if.when_true, indent + 1);
            dump_node(f, node->as.stmt_if.when_false, indent + 1);
            break;
        case NODE_STMT_WHILE:
            fprintf(f, "STMT WHILE\n");
            dump_node(f, node->as.stmt_while.condition, indent + 1);
            dump_node(f, node->as.stmt_while.body, indent + 1);
            break;
        case NODE_STMT_BLK: {
            fprintf(f, "STMT BLK\n");

            uint32_t size = da_get_size(node->as.stmt_blk.stmts);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.stmt_blk.stmts[i], indent + 1);
            }
        } break;
        case NODE_MOD: {
            fprintf(f, "MOD\n");

            uint32_t size = da_get_size(node->as.mod.decls);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.mod.decls[i], indent + 1);
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
        case NODE_RECORD:
            fprintf(f, "RECORD\n");
            dump_node(f, node->as.record.blk, indent + 1);
            break;
        case NODE_CINITF:
            fprintf(f, "CINITF\n");
            dump_node(f, node->as.cinitf.name, indent + 1);
            dump_node(f, node->as.cinitf.init, indent + 1);
            break;
        case NODE_CINIT: {
            fprintf(f, "CINIT\n");

            uint32_t size = da_get_size(node->as.cinit.kids);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.cinit.kids[i], indent + 1);
            }
        } break;
        case NODE_FIELD:
            fprintf(f, "FIELD .%s\n", node->as.field.field);
            dump_node(f, node->as.field.receiver, indent + 1);
            break;
        case NODE_ARRAY: {
            fprintf(f, "ARRAY\n");
            dump_node(f, node->as.array.len, indent + 1);
            dump_node(f, node->as.array.type, indent + 1);

            uint32_t size = da_get_size(node->as.array.initializer_list);
            for (uint32_t i = 0; i < size; ++i) {
                dump_node(f, node->as.array.initializer_list[i], indent + 1);
            }
        } break;
        case NODE_PTR:
            fprintf(f, "PTR\n");
            dump_node(f, node->as.ptr.child, indent + 1);
            break;
        case NODE_MPTR:
            fprintf(f, "MPTR\n");
            dump_node(f, node->as.mptr.term, indent + 1);
            dump_node(f, node->as.mptr.child, indent + 1);
            break;
    }
}
