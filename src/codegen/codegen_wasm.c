#include "codegen_wasm.h"

#include <stdio.h>

#include "ast.h"
#include "da.h"
#include "span.h"
#include "typestore.h"

typedef struct codegen_state {
    FILE* out;
} codegen_state_t;

static void codegen_node(codegen_state_t* cgs, node_t* node) {
    munit_assert_not_null(node);

    FILE* out = cgs->out;

    switch (node->type) {
        case NODE_ERR: break;
        case NODE_INT:
            fprintf(out, "(i32.const %lu)\n", node->as.int_.value);
            return;
        case NODE_FLOAT: break;
        case NODE_IDENT: break;
        case NODE_BINOP:
            codegen_node(cgs, node->as.binop.left);
            codegen_node(cgs, node->as.binop.right);

            // FIXME: using i32 for all types!
            switch (node->as.binop.type) {
                case BINOP_ADD: fprintf(out, "(i32.add)\n"); break;
                case BINOP_SUB: fprintf(out, "(i32.sub)\n"); break;
                case BINOP_MUL: fprintf(out, "(i32.mul)\n"); break;
                // FIXME: Using div_u for everything!
                case BINOP_DIV: fprintf(out, "(i32.div_u)\n"); break;
            }
            return;
        case NODE_UNOP: break;
        case NODE_STMT_EXPR: break;
        case NODE_STMT_RET: break;
        case NODE_STMT_BLK: break;
        case NODE_MOD: break;
        case NODE_DECL: break;
        case NODE_ASSIGN: break;
        case NODE_ARG: break;
        case NODE_PROC: break;
        case NODE_PTR: break;
        case NODE_MPTR: break;
    }

    munit_assert(false);
}

void codegen_wasm(codegen_params_t* params) {
    FILE*   out = params->out;
    node_t* ast = params->ast;

    codegen_state_t cgs = {.out = out};

    munit_assert_uint8(ast->type, ==, NODE_MOD);
    node_mod_t* mod = &ast->as.mod;

    fprintf(out, "(module\n");

    size_t size = da_get_size(mod->decls);
    for (size_t i = 0; i < size; ++i) {
        munit_assert_uint8(mod->decls[i]->type, ==, NODE_DECL);

        node_decl_t* decl = &mod->decls[i]->as.decl;

        munit_assert_uint8(decl->init->type, ==, NODE_PROC);
        node_proc_t* proc = &decl->init->as.proc;

        type_t const* ty = typestore_find_type(params->ts, decl->init->type_id);
        munit_assert_not_null(ty);
        munit_assert_uint8(ty->tag, ==, TYPE_PROC);

        type_t const* retty =
            typestore_find_type(params->ts, ty->as.proc.return_type);
        munit_assert_not_null(ty);

        munit_assert_uint32(da_get_size(proc->args), ==, 0);

        char const* proc_name = decl->name;
        fprintf(out, "(func (export \"%s\") ", proc_name);

        if (retty->tag != TYPE_VOID) {
            char const* wasm_type_name = NULL;
            switch (retty->tag) {
                case TYPE_INT:
                    munit_assert_uint16(retty->as.int_.bits, ==, 32);
                    wasm_type_name = "i32";
                    break;
                case TYPE_FLOAT:
                case TYPE_PTR:
                case TYPE_MPTR:
                case TYPE_PROC:
                case TYPE_ERR:
                case TYPE_VOID:
                case TYPE_TYPE: munit_assert(false);
            }

            munit_assert_not_null(wasm_type_name);
            fprintf(out, "(result %s)\n", wasm_type_name);
        }

        munit_assert_uint8(proc->body->type, ==, NODE_STMT_BLK);

        node_stmt_block_t* body_blk = &proc->body->as.stmt_blk;
        size_t             size = da_get_size(body_blk->stmts);
        for (size_t i = 0; i < size; ++i) {
            munit_assert_uint8(body_blk->stmts[i]->type, ==, NODE_STMT_RET);

            codegen_node(&cgs, body_blk->stmts[i]->as.stmt_ret.child);
            fprintf(out, "(return)\n");
        }

        fprintf(out, ")\n");
    }

    fprintf(out, ")\n");
}
