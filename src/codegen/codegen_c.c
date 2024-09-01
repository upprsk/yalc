#include "codegen_c.h"

#include <stdint.h>
#include <stdio.h>

#include "allocator.h"
#include "ast.h"
#include "common.h"
#include "da.h"
#include "errors.h"
#include "span.h"
#include "typestore.h"

typedef struct value {
    char const* name;
    char const* extern_name;
    uint32_t    idx;
    bool        is_implicit_ptr;
} value_t;
da_declare(value_t, value);

typedef struct vstack {
    value_t*    values;
    allocator_t alloc;
    uint32_t    next_idx;
} vstack_t;

static inline void vstack_clean(vstack_t* s) { da_clear(s->values); }

static inline void vstack_push(vstack_t* s, value_t v) {
    s->values = da_append_value(s->values, s->alloc, &v);
}

static inline void vstack_push_idx(vstack_t* s, uint32_t idx) {
    vstack_push(s, (value_t){.idx = idx});
}

static inline void vstack_push_idx_lvalue(vstack_t* s, uint32_t idx) {
    vstack_push(s, (value_t){.idx = idx, .is_implicit_ptr = true});
}

static inline uint32_t vstack_push_tmp(vstack_t* s) {
    uint32_t idx = s->next_idx++;
    vstack_push_idx(s, idx);
    return idx;
}

static inline uint32_t vstack_push_tmp_lvalue(vstack_t* s) {
    uint32_t idx = s->next_idx++;
    vstack_push_idx_lvalue(s, idx);
    return idx;
}

static inline uint32_t vstack_push_named_tmp(vstack_t* s, char const* name) {
    uint32_t idx = s->next_idx++;
    vstack_push(s, (value_t){.name = name, .idx = idx});
    return idx;
}

static inline uint32_t vstack_push_tmp_from_global(vstack_t* s) {
    uint32_t idx = s->next_idx++;
    vstack_push(s, (value_t){.idx = idx, .is_implicit_ptr = true});
    return idx;
}

static inline value_t vstack_pop(vstack_t* s) {
    value_t value;
    munit_assert(da_pop_value(s->values, &value));

    return value;
}

static value_t* vstack_find(vstack_t* s, char const* name) {
    size_t count = da_get_size(s->values);
    for (size_t i = 0; i < count; i++) {
        // fprintf(stderr, "vstack_find(%s): %s\n", name, s->values[i].name);
        if (streq(s->values[i].name, name)) return &s->values[i];
    }

    return NULL;
}

static value_t* vstack_dump(vstack_t* s) {
    fprintf(stderr, "vstack_dump()\n");

    size_t count = da_get_size(s->values);
    for (size_t i = 0; i < count; i++) {
        fprintf(stderr,
                "[%zu] idx=%d, name='%s', extern_name='%s', is_global_ptr=%d\n",
                i, s->values[i].idx, s->values[i].name,
                s->values[i].extern_name, s->values[i].is_implicit_ptr);
    }

    return NULL;
}

typedef struct codegen_state {
    FILE*             out;
    typestore_t*      ts;
    error_reporter_t* er;
    allocator_t       tempalloc;
    vstack_t          vstack;
    vstack_t          globals;
} codegen_state_t;

typedef struct blk_state {
    struct blk_state* outer;
    node_t**          defers;
    bool              has_returned;
} blk_state_t;

static inline void append_defer(blk_state_t* bs, allocator_t alloc,
                                node_t* defer_stmt) {
    bs->defers = da_append_node(bs->defers, alloc, &defer_stmt);
}

#define TYPE "T%d"
#define TMP  "l%d"

static void codegen_type(codegen_state_t* cs, type_id_t id, type_t const* t) {
    fprintf(cs->out, "// type [%d] %s\n", id.id,
            typestore_type_to_str(cs->ts, cs->tempalloc, t));

    if (t->tag == TYPE_TYPE || t->tag == TYPE_ERR ||
        t->tag == TYPE_PLACEHOLDER || t->tag == TYPE_KW)
        return;

    fprintf(cs->out, "typedef ");

    switch (t->tag) {
        case TYPE_VOID: fprintf(cs->out, "void"); break;
        case TYPE_INT:
            fprintf(cs->out, "%sint%d_t", t->as.int_.signed_ ? "" : "u",
                    t->as.int_.bits);
            break;
        case TYPE_FLOAT:
            if (t->as.float_.bits == 32)
                fprintf(cs->out, "float");
            else if (t->as.float_.bits == 64)
                fprintf(cs->out, "double");
            else
                munit_assert(false);
            break;
        case TYPE_BOOL: fprintf(cs->out, "bool"); break;
        case TYPE_PTR:
        case TYPE_MPTR: fprintf(cs->out, TYPE "*", t->as.ptr.inner.id); break;
        case TYPE_PROC: {
            fprintf(cs->out, TYPE "(" TYPE ")(", t->as.proc.return_type.id,
                    id.id);

            size_t count = da_get_size(t->as.proc.args);
            for (size_t i = 0; i < count; i++) {
                if (i != 0) fprintf(cs->out, ", ");
                fprintf(cs->out, TYPE, t->as.proc.args[i].id);
            }

            if (t->as.proc.is_variadic) {
                fprintf(cs->out, ", ...");
            }

            fprintf(cs->out, ")");
        } break;
        case TYPE_RECORD: {
            fprintf(cs->out, "struct {\n");

            size_t count = da_get_size(t->as.record.fields);
            for (size_t i = 0; i < count; i++) {
                fprintf(cs->out, TYPE " %s;\n", t->as.record.fields[i].type.id,
                        t->as.record.fields[i].name);
            }

            fprintf(cs->out, "}");
        } break;
        case TYPE_KW:
        case TYPE_ARRAY:
        case TYPE_ERR:
        case TYPE_TYPE:
        case TYPE_PLACEHOLDER: munit_assert(false); break;
    }

    if (t->tag != TYPE_PROC) {
        fprintf(cs->out, " " TYPE, id.id);
    }

    fprintf(cs->out, ";\n");
}

static void codegen_types(codegen_state_t* cs) {
    fprintf(cs->out,
            "#include <stdint.h>\n"
            "#include <stdbool.h>\n"
            "\n");

    size_t count = da_get_size(cs->ts->entries);
    for (size_t i = 2; i < count; i++) {
        typestore_entry_t const* type_entry = &cs->ts->entries[i];

        codegen_type(cs, type_entry->id, &type_entry->type);
    }
    fprintf(cs->out, "\n");
}

static void codegen_proc_proto(codegen_state_t* cs, node_decl_t* decl,
                               node_t* proc_node, type_t const* proc_type) {
    munit_assert_not_null(proc_node);
    munit_assert_not_null(proc_type);

    node_proc_t* proc = &proc_node->as.proc;

    fprintf(cs->out, TYPE " %s(", proc_type->as.proc.return_type.id,
            decl->extern_name);

    size_t count = da_get_size(proc_type->as.proc.args);
    munit_assert_size(count, ==, da_get_size(proc->args));

    for (size_t i = 0; i < count; i++) {
        munit_assert_uint8(proc->args[i]->type, ==, NODE_ARG);

        char const* name = proc->args[i]->as.arg.name;
        uint32_t    idx = vstack_push_named_tmp(&cs->vstack, name);

        if (i != 0) fprintf(cs->out, ", ");
        fprintf(cs->out, TYPE " /* %s */ " TMP, proc_type->as.proc.args[i].id,
                name, idx);
    }

    if (proc_type->as.proc.is_variadic) {
        fprintf(cs->out, ", ...");
    }

    fprintf(cs->out, ")");
}

static void codegen_stmt_blk(codegen_state_t* cs, blk_state_t* bs,
                             node_t* body);
static void codegen_defers(codegen_state_t* cs, blk_state_t* bs);
static void codegen_defers_rec(codegen_state_t* cs, blk_state_t* bs);

static void codegen_expr(codegen_state_t* cs, node_t* node) {
    munit_assert_not_null(node);

    // fprintf(stderr, "# codegen_expr(%s)\n", node_type_to_str(node->type));
    // vstack_dump(&cs->vstack);

    // fprintf(cs->out, "// %s\n", node_type_to_str(node->type));

    switch (node->type) {
        case NODE_INT: {
            uint32_t idx = vstack_push_tmp(&cs->vstack);

            fprintf(cs->out, TYPE " " TMP " = %lu;\n", node->type_id.id, idx,
                    node->as.int_.value);
        } break;
        case NODE_STR: {
            uint32_t idx = vstack_push_tmp(&cs->vstack);

            type_t const* ty = typestore_find_type(cs->ts, node->type_id);
            munit_assert_not_null(ty);
            munit_assert_uint8(ty->tag, ==, TYPE_MPTR);
            type_id_t inner = ty->as.mptr.inner;

            fprintf(cs->out, "static " TYPE " " TMP "[] = {", inner.id, idx);
            for (size_t i = 0; i < node->as.str.len; i++) {
                fprintf(cs->out, "(" TYPE ")%d,", inner.id,
                        node->as.str.str[i]);
            }
            fprintf(cs->out, " 0};\n");
        } break;
        case NODE_IDENT: {
            char const* ident = node->as.ident.ident;

            value_t* v = vstack_find(&cs->vstack, ident);
            if (v == NULL) {
                v = vstack_find(&cs->globals, ident);
                munit_assert_not_null(v);

                uint32_t idx = vstack_push_tmp_from_global(&cs->vstack);
                fprintf(cs->out, "// global %s\n", ident);
                fprintf(cs->out, TYPE "* " TMP " = &%s;\n", node->type_id.id,
                        idx, v->extern_name);
            } else {
                fprintf(cs->out, "// local %s at %d\n", ident, v->idx);
                vstack_push_idx_lvalue(&cs->vstack, v->idx);
            }
        } break;
        case NODE_COMP: {
            codegen_expr(cs, node->as.comp.left);
            codegen_expr(cs, node->as.comp.right);

            value_t rhs = vstack_pop(&cs->vstack);
            value_t lhs = vstack_pop(&cs->vstack);

            uint32_t idx = vstack_push_tmp(&cs->vstack);

            char const* op = "";
            switch (node->as.comp.type) {
                case COMP_EQ: op = "=="; break;
                case COMP_NEQ: op = "!="; break;
                case COMP_LT: op = "<"; break;
                case COMP_LTE: op = "<="; break;
                case COMP_GT: op = ">"; break;
                case COMP_GTE: op = ">="; break;
            }

            fprintf(cs->out, TYPE " " TMP " = " TMP " %s " TMP ";\n",
                    node->type_id.id, idx, lhs.idx, op, rhs.idx);
        } break;
        case NODE_BINOP: {
            codegen_expr(cs, node->as.binop.left);
            codegen_expr(cs, node->as.binop.right);

            value_t rhs = vstack_pop(&cs->vstack);
            value_t lhs = vstack_pop(&cs->vstack);

            uint32_t idx = vstack_push_tmp(&cs->vstack);

            char const* op = "";
            switch (node->as.binop.type) {
                case BINOP_ADD: op = "+"; break;
                case BINOP_SUB: op = "-"; break;
                case BINOP_MUL: op = "*"; break;
                case BINOP_DIV: op = "/"; break;
            }

            fprintf(cs->out, TYPE " " TMP " = " TMP " %s " TMP ";\n",
                    node->type_id.id, idx, lhs.idx, op, rhs.idx);
        } break;
        case NODE_REF: {
            codegen_expr(cs, node->as.ref.child);
            value_t  v = vstack_pop(&cs->vstack);
            uint32_t idx = vstack_push_tmp(&cs->vstack);

            fprintf(cs->out, TYPE " " TMP " = &" TMP ";\n", node->type_id.id,
                    idx, v.idx);
        } break;
        case NODE_CAST: {
            codegen_expr(cs, node->as.ref.child);
            value_t  v = vstack_pop(&cs->vstack);
            uint32_t idx = vstack_push_tmp(&cs->vstack);

            fprintf(cs->out, TYPE " " TMP " = " TMP ";\n", node->type_id.id,
                    idx, v.idx);
        } break;
        case NODE_CALL: {
            codegen_expr(cs, node->as.call.callee);
            value_t v = vstack_pop(&cs->vstack);

            value_t* args = da_init_value(cs->tempalloc);
            size_t   count = da_get_size(node->as.call.args);
            for (size_t i = 0; i < count; i++) {
                node_t* arg = node->as.call.args[i];

                codegen_expr(cs, arg);
                value_t value = vstack_pop(&cs->vstack);
                args = da_append_value(args, cs->tempalloc, &value);
            }

            type_t const* ty =
                typestore_find_type(cs->ts, node->as.call.callee->type_id);
            munit_assert_not_null(ty);
            munit_assert_uint8(ty->tag, ==, TYPE_PROC);

            uint32_t idx = vstack_push_tmp(&cs->vstack);
            if (!type_id_eq(ty->as.proc.return_type,
                            cs->ts->primitives.void_)) {
                fprintf(cs->out, TYPE " " TMP " = ", node->type_id.id, idx);
            }

            fprintf(cs->out, TMP "(", v.idx);

            count = da_get_size(args);
            for (size_t i = 0; i < count; i++) {
                if (i != 0) fprintf(cs->out, ", ");
                fprintf(cs->out, TMP, args[i].idx);
            }

            fprintf(cs->out, ");\n");
        } break;
        case NODE_CINIT: {
            value_t* fields = da_init_value(cs->tempalloc);
            size_t   count = da_get_size(node->as.cinit.kids);
            for (size_t i = 0; i < count; i++) {
                node_t* init = node->as.cinit.kids[i];
                munit_assert_uint8(init->type, ==, NODE_CINITF);

                node_t* kw = init->as.cinitf.name;
                munit_assert_uint8(kw->type, ==, NODE_KW);

                codegen_expr(cs, init->as.cinitf.init);
                value_t value = vstack_pop(&cs->vstack);
                fields = da_append_value(
                    fields, cs->tempalloc,
                    &(value_t){.idx = value.idx, .name = kw->as.kw.ident});
            }

            uint32_t idx = vstack_push_tmp(&cs->vstack);
            fprintf(cs->out, TYPE " " TMP "= {", node->type_id.id, idx);

            count = da_get_size(fields);
            for (size_t i = 0; i < count; i++) {
                fprintf(cs->out, ".%s=" TMP ", ", fields[i].name,
                        fields[i].idx);
            }

            fprintf(cs->out, "};\n");
        } break;
        case NODE_FIELD: {
            codegen_expr(cs, node->as.field.receiver);
            value_t  v = vstack_pop(&cs->vstack);
            uint32_t idx = vstack_push_tmp_lvalue(&cs->vstack);

            type_t const* rty =
                typestore_find_type(cs->ts, node->as.field.receiver->type_id);
            munit_assert_not_null(rty);

            char const* op = ".";
            if (rty->tag == TYPE_PTR) {
                op = "->";
            }

            fprintf(cs->out, TYPE " " TMP " = " TMP "%s%s;\n", node->type_id.id,
                    idx, v.idx, op, node->as.field.field);
        } break;
        default:
            fprintf(cs->out, "// UNIMPLEMENTED EXPR %s\n",
                    node_type_to_str(node->type));
            vstack_push_tmp(&cs->vstack);
    }
}

static void codegen_stmt(codegen_state_t* cs, blk_state_t* bs, node_t* node) {
    size_t stack_expected_size = da_get_size(cs->vstack.values);

    switch (node->type) {
        case NODE_DECL: {
            if (node->as.decl.init) {
                codegen_expr(cs, node->as.decl.init);
            } else {
                report_error(cs->er, node->span,
                             "Declarations without initializers are not "
                             "implemented. All variables must be initialized");
                vstack_push_tmp(&cs->vstack);
            }

            value_t v = vstack_pop(&cs->vstack);
            vstack_push(&cs->vstack,
                        (value_t){.idx = v.idx, .name = node->as.decl.name});

            ++stack_expected_size;

            fprintf(cs->out, "// local %s at %d\n", node->as.decl.name, v.idx);
        } break;
        case NODE_ASSIGN: {
            if (node->as.assign.lhs->type == NODE_IDENT &&
                streq(node->as.assign.lhs->as.ident.ident, "_")) {
                // this is a discard
                codegen_expr(cs, node->as.assign.rhs);
                value_t rhs = vstack_pop(&cs->vstack);

                fprintf(cs->out, "(void)" TMP ";\n", rhs.idx);
            } else {
                codegen_expr(cs, node->as.assign.rhs);
                value_t rhs = vstack_pop(&cs->vstack);

                node_t* lhs = node->as.assign.lhs;
                switch (lhs->type) {
                    case NODE_FIELD: {
                        codegen_expr(cs, lhs->as.field.receiver);
                        value_t v = vstack_pop(&cs->vstack);

                        type_t const* rty = typestore_find_type(
                            cs->ts, lhs->as.field.receiver->type_id);
                        munit_assert_not_null(rty);

                        char const* op = ".";
                        if (rty->tag == TYPE_PTR) {
                            op = "->";
                        }

                        fprintf(cs->out, TMP "%s%s", v.idx, op,
                                lhs->as.field.field);
                    } break;
                    case NODE_IDENT: {
                        char const* ident = lhs->as.ident.ident;
                        value_t*    v = vstack_find(&cs->vstack, ident);
                        if (v == NULL) {
                            report_error(
                                cs->er, lhs->span,
                                "assign to global has not been implemented");
                            return;
                        }

                        fprintf(cs->out, TMP, v->idx);
                    } break;
                    default:
                        report_error(cs->er, lhs->span,
                                     "not an lvalue, can't use");
                        return;
                }

                fprintf(cs->out, " = " TMP ";\n", rhs.idx);
            }
        } break;
        case NODE_STMT_EXPR: {
            codegen_expr(cs, node->as.stmt_expr.expr);
            vstack_pop(&cs->vstack);
        } break;
        case NODE_STMT_DEFER: {
            append_defer(bs, cs->tempalloc, node->as.defer.stmt);
        } break;
        case NODE_STMT_RET: {
            codegen_expr(cs, node->as.stmt_ret.child);
            value_t v = vstack_pop(&cs->vstack);

            codegen_defers_rec(cs, bs);
            bs->has_returned = true;

            fprintf(cs->out, "return " TMP ";\n", v.idx);
        } break;
        case NODE_STMT_WHILE: {
            fprintf(cs->out, "while (true) {\n");

            codegen_expr(cs, node->as.stmt_while.condition);
            value_t v = vstack_pop(&cs->vstack);
            fprintf(cs->out, "if (!(" TMP ")) {\n", v.idx);
            fprintf(cs->out, "break;\n");
            fprintf(cs->out, "}\n");

            codegen_stmt(cs, bs, node->as.stmt_while.body);

            fprintf(cs->out, "}\n");
        } break;
        case NODE_STMT_BLK: {
            codegen_stmt_blk(cs, bs, node);
        } break;
        default:
            fprintf(cs->out, "// UNIMPLEMENTED STMT %s\n",
                    node_type_to_str(node->type));
            // munit_assert(false);
    }

    // fprintf(stderr, "# end of statment\n");
    // vstack_dump(&cs->vstack);

    munit_assert_size(da_get_size(cs->vstack.values), ==, stack_expected_size);
}

static void codegen_stmt_blk(codegen_state_t* cs, blk_state_t* outer_bs,
                             node_t* body) {
    munit_assert_uint8(body->type, ==, NODE_STMT_BLK);

    blk_state_t bs = {.defers = da_init_node(cs->tempalloc), .outer = outer_bs};

    fprintf(cs->out, "{\n");

    size_t count = da_get_size(body->as.stmt_blk.stmts);
    for (size_t i = 0; i < count; i++) {
        node_t* stmt = body->as.stmt_blk.stmts[i];

        codegen_stmt(cs, &bs, stmt);
    }

    if (!bs.has_returned) codegen_defers(cs, &bs);

    fprintf(cs->out, "}\n");
}

static void codegen_defers(codegen_state_t* cs, blk_state_t* bs) {
    // defers
    size_t count = da_get_size(bs->defers);
    for (size_t i = count; i > 0; i--) {
        fprintf(cs->out, "// defer %zu\n{\n", i);

        codegen_stmt(cs, bs, bs->defers[i - 1]);
        fprintf(cs->out, "}\n");
    }
}

static void codegen_defers_rec(codegen_state_t* cs, blk_state_t* bs) {
    codegen_defers(cs, bs);

    if (bs->outer) codegen_defers_rec(cs, bs->outer);
}

static void codegen_proc(codegen_state_t* cs, node_decl_t* decl, node_t* node) {
    munit_assert_uint8(node->type, ==, NODE_PROC);

    type_t const* proc_type = typestore_find_type(cs->ts, node->type_id);
    munit_assert_not_null(proc_type);
    munit_assert_uint8(proc_type->tag, ==, TYPE_PROC);

    codegen_proc_proto(cs, decl, node, proc_type);

    blk_state_t bs = {.defers = da_init_node(cs->tempalloc)};

    node_proc_t* proc = &node->as.proc;
    codegen_stmt_blk(cs, &bs, proc->body);
}

static void codegen_module_decl(codegen_state_t* cs, node_t* node) {
    munit_assert_uint8(node->type, ==, NODE_DECL);

    node_decl_t* decl = &node->as.decl;

    vstack_push(&cs->globals, (value_t){.name = decl->name,
                                        .extern_name = decl->extern_name});

    if (decl->is_extern) {
        munit_assert_null(decl->init);
        fprintf(cs->out, "extern ");

        type_t const* ty = typestore_find_type(cs->ts, decl->declared_type);
        if (ty->tag == TYPE_PROC) {
            codegen_proc_proto(cs, decl, decl->type, ty);
            fprintf(cs->out, ";\n\n");
        } else {
            munit_assert(false);
        }

        vstack_clean(&cs->vstack);
        return;
    }

    type_t const* ty = typestore_find_type(cs->ts, decl->declared_type);
    munit_assert_not_null(ty);

    if (ty->tag == TYPE_PROC) {
        codegen_proc(cs, decl, decl->init);
    } else if (ty->tag == TYPE_TYPE) {
        // ignore types
    } else {
        munit_assert(false);
    }

    fprintf(cs->out, "\n");

    vstack_clean(&cs->vstack);
}

static void codegen_module(codegen_state_t* cs, node_t* node) {
    munit_assert_uint8(node->type, ==, NODE_MOD);

    size_t count = da_get_size(node->as.mod.decls);
    for (size_t i = 0; i < count; i++) {
        node_t* decl = node->as.mod.decls[i];

        codegen_module_decl(cs, decl);
    }
}

void codegen_c(codegen_c_params_t const* params) {
    Arena       arena = {};
    allocator_t tempalloc = {};
    allocator_init_arena(&tempalloc, &arena);

    codegen_state_t cs = {
        .out = params->out,
        .ts = params->ts,
        .er = params->er,
        .tempalloc = tempalloc,
        .vstack = {.values = da_init_value(tempalloc), .alloc = tempalloc},
        .globals = {.values = da_init_value(tempalloc), .alloc = tempalloc},
    };

    codegen_types(&cs);

    codegen_module(&cs, params->ast);

    arena_free(&arena);
}
