#include "codegen_mips.h"

#include <stdint.h>
#include <stdio.h>

#include "allocator.h"
#include "ast.h"
#include "common.h"
#include "da.h"
#include "span.h"
#include "typestore.h"

typedef struct register_alloc {
    uint8_t cap;
    uint8_t off;
    uint8_t head;
} register_alloc_t;

static uint8_t register_alloc(register_alloc_t* alloc) {
    munit_assert_uint8(alloc->head, <, alloc->cap);

    return alloc->off + alloc->head++;
}

static void register_free_all(register_alloc_t* alloc) { alloc->head = 0; }

typedef struct value {
    uint8_t     reg;
    char const* name;
} value_t;

da_declare(value_t, value);

typedef struct codegen_state {
    allocator_t  temp_alloc;
    typestore_t* ts;
    FILE*        out;

    size_t register_stack_pushes;
    size_t label_counter;

    register_alloc_t tmpregalloc;
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

static value_t find_value(codegen_state_t* cs, char const* name) {
    size_t size = da_get_size(cs->values);
    for (size_t i = 0; i < size; ++i) {
        if (streq(cs->values[i].name, name)) return cs->values[i];
    }

    munit_assert(false);
}

static size_t gen_label_id(codegen_state_t* cs) { return cs->label_counter++; }

#define REGISTER_RA 31

static void register_push_to_stack(codegen_state_t* cs, uint8_t reg) {
    cs->register_stack_pushes++;

    // allocate space for register
    fprintf(cs->out,
            "    addiu $sp, $sp, -4\n"
            "    sw $%d, 0($sp)\n",
            reg);
}

static void register_pop_from_stack(codegen_state_t* cs, uint8_t reg) {
    cs->register_stack_pushes--;

    // allocate space for register
    fprintf(cs->out,
            "    lw $%d, 0($sp)\n"
            "    addiu $sp, $sp, 4\n",
            reg);
}

typedef struct proc_state {
    register_alloc_t locregalloc;
} proc_state_t;

static void codegen_expr(codegen_state_t* cs, proc_state_t* ps, node_t* node) {
    switch (node->type) {
        case NODE_INT: {
            uint8_t reg = register_alloc(&cs->tmpregalloc);
            push_value(cs, &(value_t){.reg = reg});
            fprintf(cs->out, "    li $%d, %lu\n", reg, node->as.int_.value);
        } break;
        case NODE_IDENT: {
            value_t v = find_value(cs, node->as.ident.ident);
            // fprintf(cs->out, "    # got thing %d: %s\n", v.reg, v.name);
            push_value(cs, &v);
        } break;
        case NODE_BINOP: {
            codegen_expr(cs, ps, node->as.binop.left);
            codegen_expr(cs, ps, node->as.binop.right);

            type_t const* ty = typestore_find_type(cs->ts, node->type_id);
            munit_assert_not_null(ty);
            munit_assert_uint8(ty->tag, ==, TYPE_INT);

            bool is_signed = ty->as.int_.signed_;
            munit_assert(is_signed);

            value_t rhs = pop_value(cs);
            value_t lhs = pop_value(cs);

            uint8_t reg = register_alloc(&cs->tmpregalloc);

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
        case NODE_COMP: {
            codegen_expr(cs, ps, node->as.comp.left);
            codegen_expr(cs, ps, node->as.comp.right);

            type_t const* ty =
                typestore_find_type(cs->ts, node->as.comp.left->type_id);
            munit_assert_not_null(ty);
            munit_assert_uint8(ty->tag, ==, TYPE_INT);

            bool is_signed = ty->as.int_.signed_;
            munit_assert(is_signed);

            value_t rhs = pop_value(cs);
            value_t lhs = pop_value(cs);

            uint8_t reg = register_alloc(&cs->tmpregalloc);

            switch (node->as.comp.type) {
                case COMP_LT: {
                    fprintf(cs->out, "    slt $%d, $%d, $%d\n", reg, lhs.reg,
                            rhs.reg);
                    push_value(cs, &(value_t){.reg = reg});
                } break;
                case COMP_EQ:
                case COMP_NEQ:
                case COMP_LTE:
                case COMP_GT:
                case COMP_GTE: munit_assert(false); break;
            }
        } break;
        case NODE_CALL: {
            size_t count = da_get_size(node->as.call.args);
            munit_assert_size(count, <, 4);

            for (size_t i = 0; i < count; ++i) {
                codegen_expr(cs, ps, node->as.call.args[i]);
            }

            fprintf(cs->out, "    # call with %zu args\n", count);

            for (int i = count - 1; i >= 0; --i) {
                value_t v = pop_value(cs);
                if (i + 4 != v.reg)
                    fprintf(cs->out, "    move $%d, $%d\n", i + 4, v.reg);
            }

            fprintf(cs->out, "    jal %s\n", node->as.call.call_extern_name);

            munit_assert_not_null(node->as.call.call_extern_name);

            uint8_t reg = register_alloc(&cs->tmpregalloc);
            push_value(cs, &(value_t){.reg = reg});

            if (2 != reg) fprintf(cs->out, "    move $%d, $%d\n", reg, 2);
        } break;
        default: munit_assert(false);
    }
}

static void codegen_blk(codegen_state_t* cs, proc_state_t* ps, node_t* node);

static void codegen_stmt_expr(codegen_state_t* cs, proc_state_t* ps,
                              node_t* node) {
    codegen_expr(cs, ps, node->as.stmt_expr.expr);
    pop_value(cs);

    register_free_all(&cs->tmpregalloc);
}

static void codegen_stmt_ret(codegen_state_t* cs, proc_state_t* ps,
                             node_t* node) {
    codegen_expr(cs, ps, node->as.stmt_ret.child);
    value_t v = pop_value(cs);
    if (2 != v.reg) fprintf(cs->out, "    move $%d, $%d\n", 2, v.reg);

    register_free_all(&cs->tmpregalloc);

    register_pop_from_stack(cs, REGISTER_RA);
    fprintf(cs->out, "    jr $ra\n");
}

static void codegen_stmt_if(codegen_state_t* cs, proc_state_t* ps,
                            node_t* node) {
    codegen_expr(cs, ps, node->as.stmt_if.condition);
    value_t v = pop_value(cs);

    register_free_all(&cs->tmpregalloc);

    munit_assert_not_null(node->as.stmt_if.when_true);
    if (node->as.stmt_if.when_false) {
        size_t when_false = gen_label_id(cs);
        size_t after = gen_label_id(cs);

        fprintf(cs->out, "    beq $%d, $zero, label_%zu # when_false\n", v.reg,
                when_false);

        codegen_blk(cs, ps, node->as.stmt_if.when_true);
        fprintf(cs->out, "    j label_%zu # after\n", after);

        fprintf(cs->out, "label_%zu: # when_false\n", when_false);

        codegen_blk(cs, ps, node->as.stmt_if.when_false);
        fprintf(cs->out, "label_%zu: # after\n", after);
    } else {
        size_t after = gen_label_id(cs);

        fprintf(cs->out, "    beq $%d, $zero, label_%zu # after\n", v.reg,
                after);

        codegen_blk(cs, ps, node->as.stmt_if.when_true);
        fprintf(cs->out, "label_%zu: # after\n", after);
    }
}

static void codegen_assign(codegen_state_t* cs, proc_state_t* ps,
                           node_t* node) {
    node_assign_t* assign = &node->as.assign;

    munit_assert_uint8(assign->lhs->type, ==, NODE_IDENT);
    value_t lhs = find_value(cs, assign->lhs->as.ident.ident);

    codegen_expr(cs, ps, assign->rhs);
    value_t v = pop_value(cs);

    fprintf(cs->out,
            "    # assign %s\n"
            "    move $%d, $%d\n",
            lhs.name, lhs.reg, v.reg);
}

static void codegen_stmt_decl(codegen_state_t* cs, proc_state_t* ps,
                              node_t* node) {
    codegen_expr(cs, ps, node->as.decl.init);
    value_t v = pop_value(cs);

    uint8_t reg = register_alloc(&ps->locregalloc);
    register_push_to_stack(cs, reg);

    fprintf(cs->out,
            "    # local %s\n"
            "    move $%d, $%d\n",
            node->as.decl.name, reg, v.reg);

    push_value(cs, &(value_t){.reg = reg, .name = node->as.decl.name});
}

static void codegen_stmt(codegen_state_t* cs, proc_state_t* ps, node_t* node) {
    size_t size_start = da_get_size(cs->values);

    switch (node->type) {
        case NODE_STMT_EXPR: codegen_stmt_expr(cs, ps, node); break;
        case NODE_STMT_RET: codegen_stmt_ret(cs, ps, node); break;
        case NODE_STMT_IF: codegen_stmt_if(cs, ps, node); break;
        case NODE_ASSIGN: codegen_assign(cs, ps, node); break;
        case NODE_DECL:
            codegen_stmt_decl(cs, ps, node);
            // declared a local, so increment the number of saved values
            size_start++;
            break;
        default: munit_assert(false);
    }

    munit_assert_size(da_get_size(cs->values), ==, size_start);
}

static void codegen_blk(codegen_state_t* cs, proc_state_t* ps, node_t* node) {
    munit_assert_not_null(node);
    munit_assert_uint8(node->type, ==, NODE_STMT_BLK);
    node_stmt_block_t* blk = &node->as.stmt_blk;

    size_t count = da_get_size(blk->stmts);
    for (size_t i = 0; i < count; ++i) {
        codegen_stmt(cs, ps, blk->stmts[i]);
    }
}

static void codegen_proc(codegen_state_t* cs, char const* decl_name,
                         node_t* node) {
    munit_assert_uint8(node->type, ==, NODE_PROC);

    size_t pushes_start = cs->register_stack_pushes;

    proc_state_t ps = {
        .locregalloc = {.cap = 8, .off = 16}
    };

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

    for (size_t i = 0; i < argc; ++i) {
        munit_assert_uint8(proc->args[i]->type, ==, NODE_ARG);
        node_arg_t* arg = &proc->args[i]->as.arg;

        uint8_t reg = register_alloc(&ps.locregalloc);
        register_push_to_stack(cs, reg);

        fprintf(cs->out,
                "    # local (arg) %s\n"
                "    move $%d, $%zu\n",
                arg->name, reg, i + 4);

        push_value(cs, &(value_t){.reg = reg, .name = arg->name});
    }

    fprintf(cs->out, "    # body start\n");
    codegen_blk(cs, &ps, proc->body);
    fprintf(cs->out, "    # body end\n");

    for (size_t i = 0; i < ps.locregalloc.head - argc; ++i) {
        value_t v = pop_value(cs);
        register_pop_from_stack(cs, v.reg);
    }

    for (size_t i = 0; i < argc; ++i) {
        value_t v = pop_value(cs);
        register_pop_from_stack(cs, v.reg);
    }

    if (proc->uses_implicit_return) {
        register_pop_from_stack(cs, REGISTER_RA);
        fprintf(cs->out, "    jr $ra\n");
    }

    munit_assert_size(da_get_size(cs->values), ==, 0);
    munit_assert_size(cs->register_stack_pushes, ==, pushes_start);
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
        munit_assert_size(cs->register_stack_pushes, ==, 0);

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
        .tmpregalloc = {.cap = 8, .off = 8},
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

    // procedure to use syscal to print a single character
    fprintf(cs.out,
            "__builtin_print_char:\n"
            "    li $v0, 11\n"
            "    syscall\n"
            "    jr $ra\n\n");

    // procedure to use syscal to read a single integer
    fprintf(cs.out,
            "__builtin_read_i32:\n"
            "    li $v0, 5\n"
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

    arena_free(&arena);
}
