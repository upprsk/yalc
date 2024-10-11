#include "sema.h"

#include "ast.h"
#include "da/da.h"
#include "errors.h"
#include "map/map.h"
#include "slice/slice.h"
#include "span.h"
#include "tstore.h"

// #define debug_printf(...) fprintf(stderr, __VA_ARGS__)
#define debug_printf(...)

typedef struct sema {
    allocator_t alloc;
    allocator_t temp_alloc;

    ast_t*    ast;
    tstore_t* ts;

    error_reporter_t* er;
} sema_t;

typedef struct decl_val {
    span_t     where;
    type_ref_t type;
} decl_val_t;

typedef map_str_t(decl_val_t) map_str_decl_val_t;

typedef enum proc_ctx_flags {
    PROC_HAS_RETURNED = 1 << 0,
    PROC_UNREACHABLE_REPORTED = 1 << 1,
} proc_ctx_flags_t;

typedef struct proc_ctx {
    span_t           ret_span;
    type_ref_t       type;
    proc_ctx_flags_t flags;
} proc_ctx_t;

typedef struct ctx {
    map_str_decl_val_t* decls;
    proc_ctx_t*         proc;
    bool                in_type_eval;
} ctx_t;

/// Store needed data to perform type inference.
typedef struct inf {
    type_ref_t exp;
} inf_t;

static inline void proc_ctx_init(proc_ctx_t* c, type_ref_t curr_proc_type) {
    *c = (proc_ctx_t){.type = curr_proc_type};
}

static inline void ctx_init(ctx_t* c, ctx_t* parent, proc_ctx_t* pc) {
    *c = (ctx_t){.decls = parent->decls,
                 .proc = pc,
                 .in_type_eval = parent->in_type_eval};
}

static inline void ctx_init_scope(ctx_t* c, ctx_t* parent) {
    *c = (ctx_t){.decls = parent->decls,
                 .proc = parent->proc,
                 .in_type_eval = parent->in_type_eval};
}

static inline void ctx_init_type_scope(ctx_t* c, ctx_t* parent) {
    *c = (ctx_t){
        .decls = parent->decls, .proc = parent->proc, .in_type_eval = true};
}

static inline bool proc_has_returned(ctx_t const* c) {
    assert_not_null(c->proc);
    return c->proc->flags & PROC_HAS_RETURNED;
}

static inline bool proc_unreachable_reported(ctx_t const* c) {
    assert_not_null(c->proc);
    return c->proc->flags & PROC_UNREACHABLE_REPORTED;
}

static inline void clear_proc_returned(ctx_t* c) {
    assert_not_null(c->proc);
    c->proc->flags &= ~PROC_HAS_RETURNED;
}

static inline void mark_proc_returned(ctx_t* c) {
    assert_not_null(c->proc);
    c->proc->flags |= PROC_HAS_RETURNED;
}

static inline void mark_proc_unreachable_reported(ctx_t* c) {
    assert_not_null(c->proc);
    c->proc->flags |= PROC_UNREACHABLE_REPORTED;
}

static inline bool in_proc(ctx_t* c) { return c->proc != NULL; }

static void decl_val(sema_t* s, ctx_t* c, str_t name, decl_val_t val) {
    debug_printf("declaring '%s'\n", name.ptr);

    c->decls =
        map_put(s->temp_alloc, c->decls, strkey(name.ptr, name.len), val);
}

static decl_val_t* find_val(ctx_t* c, str_t name) {
    map_foreach(c->decls, n) {
        debug_printf("['%s'] (%x) == (%x) ['%s']\n", n->key.s, n->key.hash,
                     strkey(name.ptr, name.len).hash, name.ptr);
    }

    map_str_decl_val_t* elem =
        map_get_kstr(c->decls, strkey(name.ptr, name.len));
    if (!elem) return NULL;

    debug_printf("found '%s'\n", elem->key.s);

    return &elem->value;
}

static bool node_is_lvalue(sema_t* s, node_ref_t ref) {
    node_t const* node = ast_get(s->ast, ref);
    switch (node->kind) {
        case NODE_IDENT: return true;
        default: return false;
    }
}

static type_ref_t sema_node_ref(sema_t* s, ctx_t* ctx, inf_t inf,
                                node_ref_t ref);

// FIXME: this will be part of the compile-time eval system, so it should be
// moved out of here eventually.
static type_ref_t sema_node_to_type(sema_t* s, ctx_t* c, inf_t inf,
                                    node_ref_t ref) {
    // TODO: should sema the expression before trying to convert it to a type

    // type_ref_t ty = sema_node_ref(s, c, inf, ref);
    // if (!tstore_type_is_type(s->ts, ty)) {
    //     report_error(s->er, ast_get_span(s->ast, ref),
    //                  "can't evaluate expression to type, it has type %s",
    //                  tstore_type_ref_str(s->ts, ty, s->temp_alloc).items);
    //
    //     return (type_ref_t){};
    // }

    ctx_t ctx;
    ctx_init_type_scope(&ctx, c);

    node_t const* node = ast_get(s->ast, ref);
    if (node->kind == NODE_IDENT) {
        node_ident_t const* ident = node_as_ident(node);

        if (slice_eq(str_from_lit("i32"), ident->ident))
            return s->ts->builtins.i32;

        report_error(s->er, ast_get_span(s->ast, ref),
                     "can't evaluate expression to type");

        return (type_ref_t){};
    }

    if (node->kind == NODE_PROC) {
        // FIXME: this should be done for all nodes (see todo above)
        type_ref_t ty = sema_node_ref(s, &ctx, inf, ref);
        if (!tstore_type_is_proc(s->ts, ty)) {
            report_error(s->er, ast_get_span(s->ast, ref),
                         "can't evaluate expression to type, it has type %s",
                         tstore_type_ref_str(s->ts, ty, s->temp_alloc).items);

            return (type_ref_t){};
        }

        node_proc_t const* proc = node_as_proc(node);
        if (node_ref_valid(proc->body)) {
            report_error(s->er, ast_get_span(s->ast, ref),
                         "can't use procedure as type");
        }

        return ty;
    }

    report_error(s->er, ast_get_span(s->ast, ref),
                 "can't evaluate expression to type");

    return (type_ref_t){};
}

static type_ref_t sema_node_mod(sema_t* s, ctx_t* ctx, inf_t pinf,
                                node_t const*            node,
                                node_w_children_t const* mod) {
    (void)node;
    (void)pinf;

    inf_t            inf = {};
    slice_node_ref_t children = ast_get_arr(s->ast, mod->children);

    slice_foreach(children, i) {
        node_ref_t child = slice_at(children, i);
        type_ref_t ty = sema_node_ref(s, ctx, inf, child);
        if (!tstore_type_is_void(s->ts, ty) &&
            ast_get(s->ast, child)->kind != NODE_DECL) {
            report_warn(
                s->er, ast_get_span(s->ast, slice_at(children, i)),
                "INTERNAL: expected to get void in mod node, but got %s",
                tstore_type_ref_str(s->ts, ty, s->temp_alloc).items);
        }
    }

    return s->ts->builtins.void_;
}

static type_ref_t sema_node_decl(sema_t* s, ctx_t* ctx, inf_t pinf,
                                 node_t const* node, node_decl_t const* decl) {
    (void)pinf;

    // FIXME: when a node is const, we need to try to comptime it, and that's
    // where the procedures should come from

    type_ref_t type_expr = {};
    type_ref_t init_expr = {};

    inf_t inf = {};

    if (node_ref_valid(decl->type)) {
        type_expr = sema_node_to_type(s, ctx, inf, decl->type);
        inf.exp = type_expr;
    }

    if (node_ref_valid(decl->init)) {
        init_expr = sema_node_ref(s, ctx, inf, decl->init);
    }

    bool decl_is_proc = false;
    if (type_ref_valid(type_expr))
        decl_is_proc = tstore_type_is_proc(s->ts, type_expr);
    else if (type_ref_valid(init_expr))
        decl_is_proc = tstore_type_is_proc(s->ts, init_expr);

    // TODO: comptime
    if (!decl_is_var(decl->flags) && !decl_is_proc) {
        report_error(s->er, node->span,
                     "comptime for things other than procedures has not been "
                     "implemented");
        report_note(s->er, node->span, "type_expr has type: %s",
                    tstore_type_ref_str(s->ts, type_expr, s->temp_alloc).items);
        report_note(s->er, node->span, "init_expr has type: %s",
                    tstore_type_ref_str(s->ts, init_expr, s->temp_alloc).items);
    }

    if (decl_is_var(decl->flags) && !in_proc(ctx)) {
        report_error(s->er, node->span,
                     "global variables have not been implemented");
    }

    // FIXME: chech if the types match or can be converted/auto-casted
    if (type_ref_valid(type_expr) && type_ref_valid(init_expr) &&
        !type_ref_eq(type_expr, init_expr)) {
        report_error(s->er, node->span, "incompatible types in declaration");
        report_note(s->er, node->span, "expected type: %s",
                    tstore_type_ref_str(s->ts, type_expr, s->temp_alloc).items);
        report_note(s->er, node->span, "received type: %s",
                    tstore_type_ref_str(s->ts, init_expr, s->temp_alloc).items);
    }

    if (type_ref_valid(type_expr)) init_expr = type_expr;

    decl_val(s, ctx, decl->name,
             (decl_val_t){.where = node->span, .type = init_expr});

    return init_expr;
}

static type_ref_t sema_node_proc(sema_t* s, ctx_t* pctx, inf_t pinf,
                                 node_t const* node, node_proc_t const* proc) {
    (void)node;
    (void)pinf;

    da_args_t        type_args = da_init(s->temp_alloc);
    slice_node_ref_t args = ast_get_arr(s->ast, proc->args);
    type_ref_t       type_ret = {};
    span_t           span_ret = node->span;

    ctx_t head_ctx;
    ctx_init_scope(&head_ctx, pctx);

    inf_t        inf = {};
    slice_args_t inf_type_args = {};

    if (type_ref_valid(pinf.exp)) {
        type_t const* ty = tstore_get(s->ts, pinf.exp);
        if (ty->kind == TYPE_PROC) {
            inf_type_args = tstore_get_args(s->ts, ty->as.proc.args);
        }
    }

    slice_foreach(args, i) {
        node_ref_t node_ref = slice_at(args, i);
        ast_set_type(s->ast, node_ref, s->ts->builtins.void_);

        node_t const*     node = ast_get(s->ast, node_ref);
        node_arg_t const* arg = node_as_arg(node);
        type_ref_t        ty = {};

        if (node_ref_valid(arg->type)) {
            ty = sema_node_to_type(s, &head_ctx, inf, arg->type);
            ast_set_type(s->ast, arg->type, s->ts->builtins.type);
        } else if (i < inf_type_args.len) {
            ty = slice_at(inf_type_args, i).type;
        } else {
            report_error(s->er, node->span,
                         "missing type in procedure argument '%s'",
                         arg->name.ptr);
        }

        da_push_back(&type_args, (type_proc_arg_t){.type = ty});
        decl_val(s, &head_ctx, arg->name,
                 (decl_val_t){.where = node->span, .type = ty});
    }

    if (node_ref_valid(proc->ret)) {
        type_ret = sema_node_to_type(s, &head_ctx, inf, proc->ret);
        ast_set_type(s->ast, proc->ret, s->ts->builtins.type);

        span_ret = ast_get_span(s->ast, proc->ret);
    } else if (type_ref_valid(pinf.exp)) {
        type_t const* ty = tstore_get(s->ts, pinf.exp);
        if (ty->kind == TYPE_PROC) {
            type_ret = ty->as.proc.ret;
        }
    } else {
        type_ret = s->ts->builtins.void_;
    }

    type_ref_t type = tstore_add_proc(s->ts, tstore_add_args(s->ts, type_args),
                                      type_ret, proc_has_vararg(proc->flags));

    if (node_ref_valid(proc->body)) {
        proc_ctx_t proc_ctx;
        proc_ctx_init(&proc_ctx, type);

        ctx_t body_ctx;
        ctx_init(&body_ctx, &head_ctx, &proc_ctx);

        type_ref_t ty = sema_node_ref(s, &body_ctx, inf, proc->body);
        if (!tstore_type_is_void(s->ts, ty)) {
            report_warn(s->er, ast_get_span(s->ast, proc->body),
                        "INTERNAL: expected to get void in node of body of "
                        "procedure node, but got %s",
                        tstore_type_ref_str(s->ts, ty, s->temp_alloc).items);
        }

        if (!tstore_type_is_void(s->ts, type_ret) &&
            !proc_has_returned(&body_ctx)) {
            report_error(
                s->er, node->span,
                "procedure with non-void return type %s, returns "
                "implicitly",
                tstore_type_ref_str(s->ts, type_ret, s->temp_alloc).items);
            report_note(s->er, span_ret, "return type declared here");
        }
    } else if (!pctx->in_type_eval) {
        // in case there is no body, then this is a procedure type.
        type = s->ts->builtins.type;
    }

    return type;
}

static type_ref_t sema_node_blk(sema_t* s, ctx_t* pctx, inf_t pinf,
                                node_t const*            node,
                                node_w_children_t const* blk) {
    (void)node;
    (void)pinf;

    ctx_t ctx;
    ctx_init_scope(&ctx, pctx);

    inf_t            inf = {};
    slice_node_ref_t children = ast_get_arr(s->ast, blk->children);

    slice_foreach(children, i) {
        if (proc_has_returned(&ctx) && !proc_unreachable_reported(&ctx)) {
            mark_proc_unreachable_reported(&ctx);

            report_warn(s->er, ast_get_span(s->ast, slice_at(children, i)),
                        "unreachable code");
        }

        node_ref_t child = slice_at(children, i);
        type_ref_t ty = sema_node_ref(s, &ctx, inf, child);
        if (!tstore_type_is_void(s->ts, ty) &&
            ast_get(s->ast, child)->kind != NODE_DECL) {
            report_warn(
                s->er, ast_get_span(s->ast, slice_at(children, i)),
                "INTERNAL: expected to get void in blk node, but got %s",
                tstore_type_ref_str(s->ts, ty, s->temp_alloc).items);
        }
    }

    return s->ts->builtins.void_;
}

static type_ref_t sema_node_call(sema_t* s, ctx_t* ctx, inf_t pinf,
                                 node_t const* node, node_call_t const* call) {
    inf_t inf = pinf;

    type_ref_t callee_type = sema_node_ref(s, ctx, inf, call->callee);
    if (!tstore_type_is_proc(s->ts, callee_type)) {
        if (type_ref_valid(callee_type)) {
            report_error(
                s->er, ast_get_span(s->ast, call->callee),
                "can't call non-procedure of type %s",
                tstore_type_ref_str(s->ts, callee_type, s->temp_alloc).items);
        }

        return inf.exp;
    }

    type_proc_t const* callee_proc =
        type_as_proc(tstore_get(s->ts, callee_type));

    slice_args_t callee_proc_args = tstore_get_args(s->ts, callee_proc->args);
    slice_node_ref_t args = ast_get_arr(s->ast, call->args);

    if (callee_proc_args.len != args.len) {
        report_error(s->er, node->span,
                     "procedure expected %zu arguments, received %zu",
                     callee_proc_args.len, args.len);
    }

    slice_foreach(args, i) {
        inf_t ainf = {};
        if (i < callee_proc_args.len) {
            ainf.exp = slice_at(callee_proc_args, i).type;
        }

        type_ref_t arg = sema_node_ref(s, ctx, ainf, slice_at(args, i));
        if (type_ref_valid(ainf.exp) && !type_ref_eq(ainf.exp, arg)) {
            string_t expected =
                tstore_type_ref_str(s->ts, ainf.exp, s->temp_alloc);
            string_t received = tstore_type_ref_str(s->ts, arg, s->temp_alloc);

            report_error(s->er, ast_get_span(s->ast, slice_at(args, i)),
                         "incompatible type for argument %zu in call, expected "
                         "%s, but got %s",
                         i + 1, expected.items, received.items);
        }
    }

    return callee_proc->ret;
}

static type_ref_t sema_node_ret(sema_t* s, ctx_t* ctx, inf_t pinf,
                                node_t const* node, node_w_child_t const* ret) {
    (void)pinf;

    type_ref_t child = s->ts->builtins.void_;

    assert_not_null(ctx->proc);
    inf_t inf = {.exp = tstore_get_ret(s->ts, ctx->proc->type)};

    if (node_ref_valid(ret->child)) {
        child = sema_node_ref(s, ctx, inf, ret->child);
    }

    // FIXME: perform any needed conversions/auto-casts here
    if (type_ref_valid(child) && !type_ref_eq(child, inf.exp)) {
        report_error(s->er, node->span,
                     "procedure returns %s, but found %s in return statement",
                     tstore_type_ref_str(s->ts, inf.exp, s->temp_alloc).items,
                     tstore_type_ref_str(s->ts, child, s->temp_alloc).items);
    }

    mark_proc_returned(ctx);

    return s->ts->builtins.void_;
}

static type_ref_t sema_node_int(sema_t* s, ctx_t* ctx, inf_t inf,
                                node_t const* node, node_int_t const* int_) {
    (void)ctx;
    (void)inf;
    (void)node;
    (void)int_;

    return s->ts->builtins.i32;
}

static type_ref_t sema_node_ident(sema_t* s, ctx_t* ctx, inf_t inf,
                                  node_t const*       node,
                                  node_ident_t const* ident) {
    (void)inf;

    decl_val_t* val = find_val(ctx, ident->ident);
    if (!val) {
        report_error(s->er, node->span, "undefined identifier '%s'",
                     ident->ident.ptr);

        return (type_ref_t){};
    }

    return val->type;
}

static type_ref_t sema_node_binary(sema_t* s, ctx_t* ctx, inf_t pinf,
                                   node_t const*        node,
                                   node_binary_t const* bin) {
    inf_t inf = pinf;

    type_ref_t lhs = sema_node_ref(s, ctx, inf, bin->left);
    type_ref_t rhs = sema_node_ref(s, ctx, inf, bin->right);

    if (!type_ref_eq(lhs, rhs)) {
        char const* opname = "<internal error>";
        switch (node->kind) {
            case NODE_ADD: opname = "addition"; break;
            case NODE_SUB: opname = "subtraction"; break;
            case NODE_MUL: opname = "multiplication"; break;
            case NODE_DIV: opname = "division"; break;
            default: assert(false);
        }

        report_error(s->er, node->span, "incompatible types in %s", opname);
        report_note(s->er, ast_get_span(s->ast, bin->left), "left has type %s",
                    tstore_type_ref_str(s->ts, lhs, s->temp_alloc).items);
        report_note(s->er, ast_get_span(s->ast, bin->right),
                    "right has type %s",
                    tstore_type_ref_str(s->ts, rhs, s->temp_alloc).items);
    }

    type_ref_t res = {};

    if (type_ref_valid(lhs)) {
        res = lhs;
    } else if (type_ref_valid(rhs)) {
        res = rhs;
    } else {
        res = inf.exp;
    }

    return res;
}

static type_ref_t sema_node_if(sema_t* s, ctx_t* ctx, inf_t pinf,
                               node_t const* node, node_ternary_t const* ter) {
    (void)pinf;
    (void)node;

    inf_t inf = {.exp = s->ts->builtins.bool_};

    type_ref_t cond = sema_node_ref(s, ctx, inf, ter->cond);
    if (!tstore_type_is_bool(s->ts, cond)) {
        report_error(s->er, ast_get_span(s->ast, ter->cond),
                     "conditions expects boolean, found %s",
                     tstore_type_ref_str(s->ts, cond, s->temp_alloc).items);
    }

    inf = (inf_t){};

    clear_proc_returned(ctx);

    type_ref_t wtrue = sema_node_ref(s, ctx, inf, ter->wtrue);
    if (!tstore_type_is_void(s->ts, wtrue)) {
        report_warn(s->er, ast_get_span(s->ast, ter->wtrue),
                    "INTERNAL: expected to get void in node of body of "
                    "if node, but got %s",
                    tstore_type_ref_str(s->ts, wtrue, s->temp_alloc).items);
    }

    bool true_returned = proc_has_returned(ctx);
    clear_proc_returned(ctx);

    if (node_ref_valid(ter->wfalse)) {
        type_ref_t wfalse = sema_node_ref(s, ctx, inf, ter->wfalse);
        if (!tstore_type_is_void(s->ts, wfalse)) {
            report_warn(
                s->er, ast_get_span(s->ast, ter->wfalse),
                "INTERNAL: expected to get void in node of body of "
                "if node, but got %s",
                tstore_type_ref_str(s->ts, wfalse, s->temp_alloc).items);
        }
    }

    bool false_returned = proc_has_returned(ctx);
    clear_proc_returned(ctx);

    if (true_returned && false_returned) {
        mark_proc_returned(ctx);
    }

    return s->ts->builtins.void_;
}

static type_ref_t sema_node_while(sema_t* s, ctx_t* ctx, inf_t pinf,
                                  node_t const*         node,
                                  node_ternary_t const* ter) {
    (void)pinf;
    (void)node;

    inf_t inf = {.exp = s->ts->builtins.bool_};

    type_ref_t cond = sema_node_ref(s, ctx, inf, ter->cond);
    if (!tstore_type_is_bool(s->ts, cond)) {
        report_error(s->er, ast_get_span(s->ast, ter->cond),
                     "conditions expects boolean, found %s",
                     tstore_type_ref_str(s->ts, cond, s->temp_alloc).items);
    }

    inf = (inf_t){};

    type_ref_t wtrue = sema_node_ref(s, ctx, inf, ter->wtrue);
    if (!tstore_type_is_void(s->ts, wtrue)) {
        report_warn(s->er, ast_get_span(s->ast, ter->wtrue),
                    "INTERNAL: expected to get void in node of body of "
                    "while node, but got %s",
                    tstore_type_ref_str(s->ts, wtrue, s->temp_alloc).items);
    }

    return s->ts->builtins.void_;
}

static type_ref_t sema_node_stmt_expr(sema_t* s, ctx_t* ctx, inf_t pinf,
                                      node_t const*         node,
                                      node_w_child_t const* stmtexpr) {
    (void)pinf;
    (void)node;

    inf_t inf = {};

    type_ref_t child = sema_node_ref(s, ctx, inf, stmtexpr->child);
    if (!tstore_type_is_void(s->ts, child) && type_ref_valid(child)) {
        report_error(s->er, ast_get_span(s->ast, stmtexpr->child),
                     "expression result of type %s is ignored",
                     tstore_type_ref_str(s->ts, child, s->temp_alloc).items);
    }

    return s->ts->builtins.void_;
}

static type_ref_t sema_node_assign(sema_t* s, ctx_t* ctx, inf_t pinf,
                                   node_t const*        node,
                                   node_binary_t const* assign) {
    (void)pinf;
    (void)node;

    inf_t      inf = {};
    type_ref_t left = sema_node_ref(s, ctx, inf, assign->left);

    inf = (inf_t){.exp = left};
    type_ref_t right = sema_node_ref(s, ctx, inf, assign->right);

    if (!node_is_lvalue(s, assign->left)) {
        report_error(s->er, ast_get_span(s->ast, assign->left),
                     "can't assign to rvalue");
    }

    // FIXME: handle constants
    // FIXME: perform any need implicit conversions
    if (!type_ref_eq(left, right)) {
        report_error(s->er, node->span,
                     "incompatible types in assignment, expected %s but got %s",
                     tstore_type_ref_str(s->ts, left, s->temp_alloc).items,
                     tstore_type_ref_str(s->ts, right, s->temp_alloc).items);
        report_note(s->er, ast_get_span(s->ast, assign->left),
                    "receiver has type %s",
                    tstore_type_ref_str(s->ts, left, s->temp_alloc).items);
        report_note(s->er, ast_get_span(s->ast, assign->right),
                    "value has type %s",
                    tstore_type_ref_str(s->ts, right, s->temp_alloc).items);
    }

    return s->ts->builtins.void_;
}

static type_ref_t sema_node(sema_t* s, ctx_t* ctx, inf_t inf,
                            node_t const* node) {
    assert_not_null(node);

    switch (node->kind) {
        case NODE_INVAL:
            report_error(s->er, node->span, "found invalid node in sema");
            return (type_ref_t){0};
        case NODE_MOD:
            return sema_node_mod(s, ctx, inf, node, node_as_mod(node));
        case NODE_DECL:
            return sema_node_decl(s, ctx, inf, node, node_as_decl(node));
        case NODE_PROC:
            return sema_node_proc(s, ctx, inf, node, node_as_proc(node));
        case NODE_CALL:
            return sema_node_call(s, ctx, inf, node, node_as_call(node));
        case NODE_BLK:
            return sema_node_blk(s, ctx, inf, node, node_as_blk(node));
        case NODE_IF: return sema_node_if(s, ctx, inf, node, node_as_if(node));
        case NODE_WHILE:
            return sema_node_while(s, ctx, inf, node, node_as_while(node));
        case NODE_STMT_EXPR:
            return sema_node_stmt_expr(s, ctx, inf, node,
                                       node_as_stmt_expr(node));
        case NODE_ASSIGN:
            return sema_node_assign(s, ctx, inf, node, node_as_assign(node));
        case NODE_ARG: break;
        case NODE_ADD:
        case NODE_SUB:
        case NODE_MUL:
        case NODE_DIV:
            return sema_node_binary(s, ctx, inf, node, node_as_binary(node));
        case NODE_NEG:
        case NODE_NOT: break;
        case NODE_RET:
            return sema_node_ret(s, ctx, inf, node, node_as_ret(node));
        case NODE_PTR: break;
        case NODE_IDENT:
            return sema_node_ident(s, ctx, inf, node, node_as_ident(node));
        case NODE_INT:
            return sema_node_int(s, ctx, inf, node, node_as_int(node));
    }

    report_error(s->er, node->span, "node was not implemented: %s",
                 node_kind_str(node->kind));
    assert(false);
}

static type_ref_t sema_node_ref(sema_t* s, ctx_t* ctx, inf_t inf,
                                node_ref_t ref) {
    type_ref_t type = sema_node(s, ctx, inf, ast_get(s->ast, ref));
    ast_set_type(s->ast, ref, type);

    return type;
}

void sema_pass(sema_desc_t* desc, ast_t* ast, node_ref_t root) {
    assert_not_null(desc);
    assert_not_null(ast);

    sema_t s = {
        .alloc = desc->alloc,
        .temp_alloc = desc->temp_alloc,
        .ast = ast,
        .ts = desc->ts,
        .er = desc->er,
    };

    ctx_t root_ctx = {};
    inf_t inf = {};

    decl_val(&s, &root_ctx, str_from_lit("true"),
             (decl_val_t){.type = s.ts->builtins.bool_});
    decl_val(&s, &root_ctx, str_from_lit("false"),
             (decl_val_t){.type = s.ts->builtins.bool_});

    sema_node_ref(&s, &root_ctx, inf, root);
}
