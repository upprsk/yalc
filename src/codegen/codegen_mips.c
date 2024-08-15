#include "codegen_mips.h"

#include <stdint.h>
#include <stdio.h>

#include "allocator.h"
#include "ast.h"
#include "da.h"
#include "span.h"
#include "typestore.h"

typedef struct register_alloc {
    uint8_t head;
} register_alloc_t;

static uint8_t register_alloc(register_alloc_t* alloc) {
    munit_assert_uint8(alloc->head, <, 8);

    return 8 + alloc->head++;
}

static void register_free_all(register_alloc_t* alloc) { alloc->head = 0; }

typedef struct value {
    uint8_t reg;
} value_t;

da_declare(value_t, value);

typedef struct codegen_state {
    allocator_t  temp_alloc;
    typestore_t* ts;
    FILE*        out;

    register_alloc_t regalloc;
    value_t*         values;
} codegen_state_t;

static void push_value(codegen_state_t* cs, value_t const* v) {
    cs->values = da_append_value(cs->values, cs->temp_alloc, v);
}

static value_t pop_value(codegen_state_t* cs) {
    value_t value;
    munit_assert(da_pop_value(cs->values, &value));

    return value;
}

#define REGISTER_RA 31

static void register_push_to_stack(codegen_state_t* cs, uint8_t reg) {
    // allocate space for register
    fprintf(cs->out,
            "    addiu $sp, $sp, -4\n"
            "    sw $%d, 0($sp)\n"
            "\n",
            reg);
}

static void register_pop_from_stack(codegen_state_t* cs, uint8_t reg) {
    // allocate space for register
    fprintf(cs->out,
            "    lw $%d, 0($sp)\n"
            "    addiu $sp, $sp, 4\n"
            "\n",
            reg);
}

static void codegen_expr(codegen_state_t* cs, node_t* node) {
    switch (node->type) {
        case NODE_INT: {
            uint8_t reg = register_alloc(&cs->regalloc);
            push_value(cs, &(value_t){.reg = reg});
            fprintf(cs->out, "    li $%d, %lu\n", reg, node->as.int_.value);
        } break;
        case NODE_BINOP: {
            codegen_expr(cs, node->as.binop.left);
            codegen_expr(cs, node->as.binop.right);

            type_t const* ty = typestore_find_type(cs->ts, node->type_id);
            munit_assert_not_null(ty);
            munit_assert_uint8(ty->tag, ==, TYPE_INT);

            bool is_signed = ty->as.int_.signed_;
            munit_assert(is_signed);

            value_t rhs = pop_value(cs);
            value_t lhs = pop_value(cs);

            uint8_t reg = register_alloc(&cs->regalloc);

            switch (node->as.binop.type) {
                case BINOP_ADD: {
                    fprintf(cs->out, "    add $%d, $%d, $%d\n", reg, lhs.reg,
                            rhs.reg);
                    push_value(cs, &(value_t){.reg = reg});
                } break;
                case BINOP_SUB:
                case BINOP_MUL:
                case BINOP_DIV: munit_assert(false); break;
            }
        } break;
        case NODE_CALL: {
            size_t count = da_get_size(node->as.call.args);
            munit_assert_size(count, <, 4);

            for (size_t i = 0; i < count; ++i) {
                codegen_expr(cs, node->as.call.args[i]);
            }

            for (int i = count - 1; i >= 0; --i) {
                value_t v = pop_value(cs);
                fprintf(cs->out, "    move $%d, $%d\n", i + 4, v.reg);
            }

            munit_assert_not_null(node->as.call.call_extern_name);

            fprintf(cs->out, "    jal %s\n", node->as.call.call_extern_name);

            uint8_t reg = register_alloc(&cs->regalloc);
            push_value(cs, &(value_t){.reg = reg});
        } break;
        default: munit_assert(false);
    }
}

static void codegen_stmt_expr(codegen_state_t* cs, node_t* node) {
    codegen_expr(cs, node->as.stmt_expr.expr);
    register_free_all(&cs->regalloc);
}

static void codegen_stmt(codegen_state_t* cs, node_t* node) {
    switch (node->type) {
        case NODE_STMT_EXPR:
            codegen_stmt_expr(cs, node);
            pop_value(cs);
            break;
        default: munit_assert(false);
    }

    munit_assert_size(da_get_size(cs->values), ==, 0);
}

static void codegen_blk(codegen_state_t* cs, node_t* node) {
    munit_assert_not_null(node);
    munit_assert_uint8(node->type, ==, NODE_STMT_BLK);
    node_stmt_block_t* blk = &node->as.stmt_blk;

    size_t count = da_get_size(blk->stmts);
    for (size_t i = 0; i < count; ++i) {
        codegen_stmt(cs, blk->stmts[i]);
    }
}

static void codegen_proc(codegen_state_t* cs, char const* decl_name,
                         node_t* node) {
    munit_assert_uint8(node->type, ==, NODE_PROC);

    node_proc_t*  proc = &node->as.proc;
    type_t const* proc_type = typestore_find_type(cs->ts, node->type_id);
    munit_assert_uint8(proc_type->tag, ==, TYPE_PROC);

    // handle arguments
    fprintf(cs->out, "# args:\n");

    size_t argc = da_get_size(proc->args);
    munit_assert_size(argc, ==, da_get_size(proc_type->as.proc.args));
    munit_assert_size(argc, <=, 4);

    for (size_t i = 0; i < argc; ++i) {
        munit_assert_uint8(proc->args[i]->type, ==, NODE_ARG);
        node_arg_t* arg = &proc->args[i]->as.arg;

        char const* argtypestr = typestore_type_id_to_str(
            cs->ts, cs->temp_alloc, proc_type->as.proc.args[i]);

        fprintf(cs->out, "# %s: %s\n", arg->name, argtypestr);
    }

    fprintf(cs->out, "%s:\n", decl_name);

    register_push_to_stack(cs, REGISTER_RA);

    codegen_blk(cs, proc->body);

    register_pop_from_stack(cs, REGISTER_RA);

    if (proc->uses_implicit_return) {
        fprintf(cs->out, "    jr $ra\n");
    }
}

static void codegen_mod(codegen_state_t* cs, node_t* node) {
    munit_assert_uint8(node->type, ==, NODE_MOD);

    node_mod_t* mod = &node->as.mod;

    size_t size = da_get_size(mod->decls);

    for (size_t i = 0; i < size; ++i) {
        node_t* n = mod->decls[i];
        if (!n->as.decl.is_extern) continue;
    }

    for (size_t i = 0; i < size; ++i) {
        node_t* n = mod->decls[i];
        if (n->as.decl.is_extern) continue;

        munit_assert_uint8(n->type, ==, NODE_DECL);
        munit_assert_not_null(n->as.decl.init);

        char const* typestr = typestore_type_id_to_str(
            cs->ts, cs->temp_alloc, n->as.decl.init->type_id);
        fprintf(cs->out,
                "# name: %s\n"
                "# type: %s\n",
                n->as.decl.name, typestr);

        codegen_proc(cs, n->as.decl.name, n->as.decl.init);

        fprintf(cs->out, "# end: %s\n\n", n->as.decl.name);
    }
}

void codegen_mips(codegen_mips_params_t const* params) {
    Arena       arena = {};
    allocator_t temp_alloc = {};
    allocator_init_arena(&temp_alloc, &arena);

    codegen_state_t cs = {
        .temp_alloc = temp_alloc,
        .ts = params->ts,
        .out = params->out,
        .values = da_init_value(temp_alloc),
    };

    // Tell assembler to not insert instructions to fill branch delay slots.
    // This is necessary when branch delay slots are disabled.
    fprintf(cs.out, ".set noreorder\n\n");

    // procedure to use syscal to print a single integer
    fprintf(cs.out,
            "__builtin_print_i32:\n"
            "    li $v0, 1\n"
            "    syscall\n"
            "    jr $ra\n\n");

    // _start calls main and then exit() (syscall 10)
    fprintf(cs.out,
            ".global _start\n"
            "_start:\n"
            "    jal main\n"
            "    li $v0, 10\n"
            "    syscall\n\n");

    codegen_mod(&cs, params->ast);
}
