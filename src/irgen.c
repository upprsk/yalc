#include "irgen.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <threads.h>

#include "alloc/allocator.h"
#include "ast.h"
#include "common.h"
#include "da/da.h"
#include "errors.h"
#include "ir.h"
#include "map/map.h"
#include "slice/slice.h"
#include "tstore.h"
#include "utils/string.h"

typedef struct reg {
    uint32_t num;
} reg_t;

typedef slice_t(reg_t) slice_reg_t;

typedef struct local {
    reg_t    reg;
    irtype_t type;
} local_t;

typedef map_str_t(local_t) map_str_local_t;
typedef map_str_t(proc_t) map_str_proc_t;

typedef struct irgen {
    allocator_t alloc;
    allocator_t temp_alloc;

    ast_t*    ast;
    tstore_t* ts;

    error_reporter_t* er;

    uint32_t ptr_size_bytes;
} irgen_t;

typedef struct curr_proc {
    slice_irtype_t args;
    irtype_t       ret;

    da_proc_ref_t cprocs;
    da_label_t    labels;

    da_inst_t insts;

    map_str_local_t* locals;

    reg_t       next_reg;
    label_ref_t next_label;
} curr_proc_t;

typedef struct curr_obj {
    map_str_proc_t* procs;
} curr_obj_t;

typedef struct ctx {
    curr_obj_t*  obj;
    curr_proc_t* proc;
} ctx_t;

static inline bool regs_are_seq(slice_reg_t regs) {
    for (size_t i = 1; i < regs.len; i++) {
        if (regs.ptr[i].num != regs.ptr[i - 1].num + 1) return false;
    }

    return true;
}

static inline irtype_t ptr_type(irgen_t* g) {
    switch (g->ptr_size_bytes) {
        case 4: return IRT_U32;
        case 8: return IRT_U64;
        default:
            fprintf(stderr, "FATAL: invalid pointer size: %d\n",
                    g->ptr_size_bytes);
            assert(false);
    }
}

static irtype_t type_to_irtype(irgen_t* g, span_t span, type_ref_t ref) {
    type_t const* type = tstore_get(g->ts, ref);

    switch (type->kind) {
        case TYPE_TYPE:
            report_error(
                g->er, span,
                "the type of types has not been implemented in the IR");
            assert(false);
        case TYPE_VOID: return IRT_VOID;
        case TYPE_INT:
            switch (type->as.int_.bits) {
                case 8: return type->as.int_.is_signed ? IRT_I8 : IRT_U8;
                case 16: return type->as.int_.is_signed ? IRT_I16 : IRT_U16;
                case 32: return type->as.int_.is_signed ? IRT_I32 : IRT_U32;
                case 64: return type->as.int_.is_signed ? IRT_I64 : IRT_U64;
                default:
                    fprintf(stderr, "FATAL: invalid integer width: %d\n",
                            type->as.int_.bits);
                    assert(false);
            }
        case TYPE_PTR:
            if (ptr_is_slice(type->as.ptr.flags)) {
                report_error(g->er, span, "slices have not been implemented");
                assert(false);
            }

            return ptr_type(g);
        case TYPE_PROC:
            report_error(g->er, span,
                         "can't convert proc to ir type, use a pointer");
            return IRT_INVAL;
        case TYPE_INVAL:
        default:
            report_error(g->er, span, "invalid type found during irgen");
            assert(false);
    }
}

// ----------------------------------------------------------------------------

static void add_proc(irgen_t* g, ctx_t* ctx, str_t name, proc_t proc) {
    assert_not_null(ctx);
    assert_not_null(ctx->obj);

    ctx->obj->procs = map_put(g->temp_alloc, ctx->obj->procs,
                              strkey(name.ptr, name.len), proc);
}

static proc_t* find_proc(ctx_t* ctx, str_t name) {
    assert_not_null(ctx);
    assert_not_null(ctx->obj);

    map_str_proc_t* v =
        map_get_kstr(ctx->obj->procs, strkey(name.ptr, name.len));
    if (!v) return NULL;

    return &v->value;
}

static lproc_ref_t add_proc_to_call_table(ctx_t* ctx, proc_t* p) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);
    assert_not_null(p);

    da_foreach(&ctx->proc->cprocs, i) {
        if (p->ref.id == da_at(ctx->proc->cprocs, i).id)
            return (lproc_ref_t){i};
    }

    // not in the call list, add it
    lproc_ref_t ref = {ctx->proc->cprocs.size};
    da_push_back(&ctx->proc->cprocs, p->ref);

    return ref;
}

static void add_local(irgen_t* g, ctx_t* ctx, str_t name, irtype_t ty,
                      reg_t reg) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);

    ctx->proc->locals =
        map_put(g->temp_alloc, ctx->proc->locals, strkey(name.ptr, name.len),
                (local_t){.type = ty, .reg = reg});
}

static local_t* find_local(ctx_t* ctx, str_t name) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);

    map_str_local_t* v =
        map_get_kstr(ctx->proc->locals, strkey(name.ptr, name.len));
    if (!v) return NULL;

    return &v->value;
}

static inline reg_t alloc_reg(ctx_t* ctx) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);

    reg_t r = ctx->proc->next_reg;
    ctx->proc->next_reg.num++;

    return r;
}

static label_ref_t gen_label(ctx_t* ctx) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);

    label_ref_t l = ctx->proc->next_label;
    ctx->proc->next_label.id++;

    da_push_back(&ctx->proc->labels, (label_t){});

    return l;
}

static void patch_label(ctx_t* ctx, label_ref_t l) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);
    assert_size(l.id, <, ctx->proc->labels.size);

    ctx->proc->labels.items[l.id] = (label_t){ctx->proc->insts.size};
}

static uint32_t add_inst(ctx_t* ctx, inst_t inst) {
    assert_not_null(ctx);
    assert_not_null(ctx->proc);

    uint32_t off = ctx->proc->insts.size;
    da_push_back(&ctx->proc->insts, inst);

#if 0   // debug thing start
    allocator_t alloc = c_allocator();
    string_t    s = inst_str(inst, alloc);
    printf("add_inst(%s)\n", s.items);
    da_free(&s);
#endif  // debug thing end

    return off;
}

// ----------------------------------------------------------------------------

static reg_t gen_expr(irgen_t* g, ctx_t* ctx, node_ref_t ref);

static reg_t gen_int(irgen_t* g, ctx_t* ctx, node_t const* node,
                     node_ref_t ref) {
    node_int_t const* int_ = node_as_int(node);
    type_ref_t        type = ast_get_type(g->ast, ref);

    irtype_t ity = type_to_irtype(g, node->span, type);

    reg_t  r = alloc_reg(ctx);
    inst_t inst = inst_copy(ity, r.num, int_->value);
    add_inst(ctx, inst);

    return r;
}

static reg_t gen_ident(irgen_t* g, ctx_t* ctx, node_t const* node,
                       node_ref_t ref) {
    node_ident_t const* ident = node_as_ident(node);
    type_ref_t          type = ast_get_type(g->ast, ref);

    local_t* l = find_local(ctx, ident->ident);
    if (!l) {
        // the name does not refer to a local, search for a global or builtin

        if (tstore_type_is_bool(g->ts, type)) {
            reg_t rd = alloc_reg(ctx);

            uint64_t v = 0;
            if (slice_eq(ident->ident, str_from_lit("true"))) {
                v = 1;
            } else if (slice_eq(ident->ident, str_from_lit("false"))) {
                v = 0;
            } else {
                report_error(g->er, node->span,
                             "INTERNAL: got invalid bool identifier, expected "
                             "'true' or 'false', got '%s'",
                             ident->ident.ptr);
            }

            add_inst(ctx, inst_copy(IRT_U8, rd.num, v));

            return rd;
        }

        if (tstore_type_is_proc(g->ts, type)) {
            // the thing is a procedure, let's try to find it in the list of
            // procedures.
            proc_t* p = find_proc(ctx, ident->ident);
            assert_not_null(p);

            lproc_ref_t proc = add_proc_to_call_table(ctx, p);
            // FIXME: we return the ref as a register?
            return (reg_t){proc.id};
        }
    }

    assert_not_null(l);

    irtype_t ity = type_to_irtype(g, node->span, type);
    if (l->type != ity) {
        report_error(
            g->er, node->span,
            "INTERNAL: expected identifier to have type %s, but got %s in AST",
            irtype_str(l->type), irtype_str(ity));
    }

    return l->reg;
}

static reg_t gen_arith(irgen_t* g, ctx_t* ctx, node_t const* node,
                       node_ref_t ref) {
    node_binary_t const* binary = node_as_binary(node);
    type_ref_t           type = ast_get_type(g->ast, ref);
    irtype_t             ity = type_to_irtype(g, node->span, type);

    reg_t lhs = gen_expr(g, ctx, binary->left);
    reg_t rhs = gen_expr(g, ctx, binary->right);
    reg_t r = alloc_reg(ctx);

    inst_t (*fn)(irtype_t t, uint32_t rd, uint32_t rs, uint32_t rt) = NULL;
    switch (node->kind) {
        case NODE_ADD: fn = inst_add; break;
        case NODE_SUB: fn = inst_sub; break;
        case NODE_MUL: fn = inst_mul; break;
        case NODE_DIV: fn = inst_div; break;
        default:
            report_error(g->er, node->span, "invalid node received in arith");
            assert(false);
    }

    assert_not_null(fn);
    inst_t inst = fn(ity, r.num, lhs.num, rhs.num);
    add_inst(ctx, inst);

    return r;
}

static reg_t gen_call(irgen_t* g, ctx_t* ctx, node_t const* node,
                      node_ref_t ref) {
    node_call_t const* call = node_as_call(node);
    type_ref_t         type = ast_get_type(g->ast, ref);
    slice_node_ref_t   args = ast_get_arr(g->ast, call->args);

    irtype_t ty = type_to_irtype(g, node->span, type);
    reg_t    cr = gen_expr(g, ctx, call->callee);

    // converting the returned ref to a proc, as we return the ref inside the
    // `reg_t` when parsing the expr. Because of sema validating everything,
    // this should not be a problem.
    lproc_ref_t pr = {cr.num};

    slice_reg_t arg_regs = slice_alloc(g->temp_alloc, slice_reg_t, args.len);

    slice_foreach(args, i) {
        node_ref_t arg = slice_at(args, i);
        reg_t      ar = gen_expr(g, ctx, arg);

        arg_regs.ptr[i] = ar;
    }

    // if the registers with the arguments are not sequential, we copy them so
    // that it becomes sequential.
    if (!regs_are_seq(arg_regs)) {
        slice_foreach(arg_regs, i) {
            type_ref_t type = ast_get_type(g->ast, slice_at(args, i));
            span_t     span = ast_get_span(g->ast, slice_at(args, i));

            reg_t    rd = alloc_reg(ctx);
            irtype_t irt = type_to_irtype(g, span, type);
            inst_t   inst = inst_move(irt, rd.num, slice_at(arg_regs, i).num);
            add_inst(ctx, inst);

            arg_regs.ptr[i] = rd;
        }
    }

    reg_t  rd = alloc_reg(ctx);
    reg_t  cs = arg_regs.len ? slice_at(arg_regs, 0) : (reg_t){0};
    inst_t inst = inst_call(ty, rd.num, pr.id, args.len, cs.num);
    add_inst(ctx, inst);

    return rd;
}

// ----------------------------------------------------------------------------

static void gen_stmt(irgen_t* g, ctx_t* ctx, node_ref_t ref);
static void gen_blk(irgen_t* g, ctx_t* ctx, node_ref_t ref);

static void gen_ret(irgen_t* g, ctx_t* ctx, node_t const* node,
                    node_ref_t ref) {
    (void)ref;

    node_w_child_t const* ret = node_as_ret(node);

    reg_t  r = gen_expr(g, ctx, ret->child);
    inst_t inst = inst_ret(ctx->proc->ret, r.num);
    add_inst(ctx, inst);
}

static void gen_decl(irgen_t* g, ctx_t* ctx, node_t const* node,
                     node_ref_t ref) {
    node_decl_t const* decl = node_as_decl(node);
    irtype_t ty = type_to_irtype(g, node->span, ast_get_type(g->ast, ref));

    reg_t init = gen_expr(g, ctx, decl->init);
    add_local(g, ctx, decl->name, ty, init);
}

static void gen_if(irgen_t* g, ctx_t* ctx, node_t const* node, node_ref_t ref) {
    (void)ref;

    node_ternary_t const* if_ = node_as_if(node);

    reg_t       cond = gen_expr(g, ctx, if_->cond);
    label_ref_t wfalse = gen_label(ctx);
    label_ref_t end = {};

    if (node_ref_valid(if_->wfalse)) {
        end = gen_label(ctx);
    }

    // jump to wfalse when the condition is false
    add_inst(ctx, inst_bz(IRT_U8, cond.num, wfalse.id));
    gen_blk(g, ctx, if_->wtrue);

    if (node_ref_valid(if_->wfalse)) {
        // when there is an else, we need to jump over it when the condition was
        // true
        add_inst(ctx, inst_b(IRT_VOID, end.id));
    }

    // this is where we want to jump to if the condition was false
    patch_label(ctx, wfalse);

    if (node_ref_valid(if_->wfalse)) {
        gen_blk(g, ctx, if_->wfalse);

        // this is where we want to jump to if the condition was true
        patch_label(ctx, end);
    }
}

static void gen_while(irgen_t* g, ctx_t* ctx, node_t const* node,
                      node_ref_t ref) {
    (void)ref;

    node_ternary_t const* while_ = node_as_while(node);

    reg_t       cond = gen_expr(g, ctx, while_->cond);
    label_ref_t end = gen_label(ctx);

    // jump to wfalse when the condition is false
    add_inst(ctx, inst_bz(IRT_U8, cond.num, end.id));
    gen_blk(g, ctx, while_->wtrue);

    // this is where we want to jump to if the condition was false
    patch_label(ctx, end);
}

// ----------------------------------------------------------------------------

static void gen_stmt(irgen_t* g, ctx_t* ctx, node_ref_t ref) {
    node_t const* node = ast_get(g->ast, ref);

    switch (node->kind) {
        case NODE_RET: gen_ret(g, ctx, node, ref); break;
        case NODE_DECL: gen_decl(g, ctx, node, ref); break;
        case NODE_IF: gen_if(g, ctx, node, ref); break;
        case NODE_WHILE: gen_while(g, ctx, node, ref); break;
        default:
            report_error(g->er, node->span, "stmt %s not implemented",
                         node_kind_str(node->kind));
            assert(false);
    }
}

static reg_t gen_expr(irgen_t* g, ctx_t* ctx, node_ref_t ref) {
    node_t const* node = ast_get(g->ast, ref);

    switch (node->kind) {
        case NODE_INT: return gen_int(g, ctx, node, ref);
        case NODE_IDENT: return gen_ident(g, ctx, node, ref);
        case NODE_ADD:
        case NODE_SUB:
        case NODE_MUL:
        case NODE_DIV: return gen_arith(g, ctx, node, ref);
        case NODE_CALL: return gen_call(g, ctx, node, ref);
        default:
            report_error(g->er, node->span, "expr %s not implemented",
                         node_kind_str(node->kind));
            assert(false);
    }
}

static void gen_blk(irgen_t* g, ctx_t* ctx, node_ref_t ref) {
    node_t const*            node = ast_get(g->ast, ref);
    node_w_children_t const* blk = node_as_blk(node);

    slice_node_ref_t children = ast_get_arr(g->ast, blk->children);
    slice_foreach(children, i) { gen_stmt(g, ctx, slice_at(children, i)); }
}

static void gen_proc(irgen_t* g, ctx_t* pctx, str_t name, node_ref_t ref) {
    node_t const*      node = ast_get(g->ast, ref);
    node_proc_t const* node_proc = node_as_proc(node);
    type_ref_t         type = ast_get_type(g->ast, ref);
    type_t const*      raw_type = tstore_get(g->ts, type);
    assert_uint(raw_type->kind, ==, TYPE_PROC);

    type_proc_t const* proc_type = &raw_type->as.proc;
    slice_args_t       proc_args = tstore_get_args(g->ts, proc_type->args);

    slice_irtype_t args = slice_alloc(g->alloc, slice_irtype_t, proc_args.len);
    slice_foreach(proc_args, i) {
        args.ptr[i] =
            type_to_irtype(g, node->span, slice_at(proc_args, i).type);
    }

    curr_proc_t proc = {
        .args = args,
        .ret = type_to_irtype(g, node->span, proc_type->ret),

        .cprocs = da_init(g->alloc),
        .labels = da_init(g->alloc),
        .insts = da_init(g->alloc),
    };

    ctx_t ctx = {.obj = pctx->obj, .proc = &proc};

    slice_node_ref_t node_args = ast_get_arr(g->ast, node_proc->args);
    assert_size(node_args.len, ==, args.len);

    slice_foreach(node_args, i) {
        node_ref_t        node_ref = slice_at(node_args, i);
        node_t const*     node = ast_get(g->ast, node_ref);
        node_arg_t const* arg = node_as_arg(node);

        add_local(g, &ctx, arg->name, slice_at(args, i), alloc_reg(&ctx));
    }

    gen_blk(g, &ctx, node_proc->body);

#if 0   // debug thing start
    fprintf(stderr, "\nproc: %s\n",
            tstore_type_str(g->ts, raw_type, g->temp_alloc).items);

    if (proc.cprocs.size) {
        fprintf(stderr, "[");

        da_foreach(&proc.cprocs, i) {
            fprintf(stderr, "%d: %d, ", i, da_at(proc.cprocs, i).id);
        }

        fprintf(stderr, "]\n");
    }

    da_foreach(&proc.insts, i) {
        string_t s = inst_str(da_at(proc.insts, i), g->temp_alloc);
        fprintf(stderr, "[%04x] %s\n", i, s.items);
    }
#endif  // debug thing end

    proc_t actual_proc = {
        .args = proc.args,
        .ret = proc.ret,
        .cprocs = da_to_slice(proc.cprocs),
        .labels = da_to_slice(proc.labels),
        .link_name = name,  // FIXME: extern
        .ref = {map_len(ctx.obj->procs)},
        .insts = da_to_slice(proc.insts),
    };

    add_proc(g, pctx, name, actual_proc);
}

static void gen_mod_decl(irgen_t* g, ctx_t* ctx, node_ref_t ref) {
    node_t const*      node = ast_get(g->ast, ref);
    node_decl_t const* decl = node_as_decl(node);

    // get the type of the init
    type_ref_t type = ast_get_type(g->ast, decl->init);
    assert(type_ref_valid(type));

    if (tstore_type_is_proc(g->ts, type)) {
        gen_proc(g, ctx, decl->name, decl->init);
        return;
    }

    report_error(g->er, node->span,
                 "global declarations that are not procedures have not been "
                 "implemented");
}

static void gen_mod(irgen_t* g, ctx_t* ctx, node_ref_t ref) {
    node_t const*            node = ast_get(g->ast, ref);
    node_w_children_t const* mod = node_as_mod(node);

    slice_node_ref_t children = ast_get_arr(g->ast, mod->children);
    slice_foreach(children, i) { gen_mod_decl(g, ctx, slice_at(children, i)); }

    // TODO: finalize the compilation unit
}

void irgen_pass(irgen_desc_t* desc, ast_t* ast, node_ref_t root) {
    assert_not_null(desc);
    assert_not_null(ast);

    irgen_t g = {
        .alloc = desc->alloc,
        .temp_alloc = desc->temp_alloc,
        .ast = ast,
        .ts = desc->ts,
        .er = desc->er,

        .ptr_size_bytes = sizeof(uintptr_t),
    };

    curr_obj_t obj = {};
    ctx_t      ctx = {.obj = &obj};

    gen_mod(&g, &ctx, root);

    size_t       len = map_len(obj.procs);
    slice_proc_t procs = slice_alloc(g.alloc, slice_proc_t, len);
    map_foreach(obj.procs, node) { procs.ptr[--len] = node->value; }

    obj_t u = {.procs = procs};

#if 1  // debug thing start
    slice_foreach(u.procs, i) {
        string_t s = proc_dump(&u.procs.ptr[i], g.temp_alloc);

        fprintf(stderr, "====\n%s\n", s.items);
    }
#endif  // debug thing end
}
