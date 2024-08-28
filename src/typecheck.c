#include "typecheck.h"

#include <stdint.h>
#include <string.h>

#include "allocator.h"
#include "ast.h"
#include "common.h"
#include "da.h"
#include "errors.h"
#include "span.h"
#include "typestore.h"

typedef struct typename_table_entry {
    char const* name;
    type_id_t   type;
} typename_table_entry_t;

// NOLINTNEXTLINE
da_declare(typename_table_entry_t, typename_table_entry);

typedef struct typename_table {
    typename_table_entry_t* entries;
} typename_table_t;

static type_id_t typename_table_find(typename_table_t* tnt, char const* name) {
    size_t size = da_get_size(tnt->entries);
    for (size_t i = 0; i < size; ++i) {
        if (strcmp(tnt->entries[i].name, name) == 0)
            return tnt->entries[i].type;
    }

    return INVALID_TYPEID;
}

typedef struct typechecker {
    error_reporter_t* er;
    typestore_t*      ts;
    typename_table_t* tnt;

    allocator_t tempalloc;
    allocator_t alloc;
} typechecker_t;

typedef struct env {
    type_id_t expected_return;
    type_id_t curr_proc_type;

    span_t proc_type_span;

    bool has_returned;
    bool has_broken;
} env_t;

typedef struct value {
    type_id_t type;
    span_t    where;

    // FIXME: This is probably not how we should do this. Stores the extern name
    // of a procedure when declared
    char const* extern_name;
} value_t;

typedef struct scope_entry {
    char const* name;
    value_t     value;
} scope_entry_t;

// NOLINTNEXTLINE
da_declare(scope_entry_t, scope_entry);

typedef struct scope {
    struct scope*  parent;
    scope_entry_t* entries;

    bool is_inside_loop;
} scope_t;

static void scope_init(scope_t* s, allocator_t alloc, scope_t* parent,
                       bool is_inside_loop) {
    *s = (scope_t){.entries = da_init_scope_entry(alloc),
                   .parent = parent,
                   .is_inside_loop = is_inside_loop};
}

static value_t const* scope_find_direct(scope_t* s, char const* name) {
    size_t size = da_get_size(s->entries);
    for (size_t i = 0; i < size; ++i) {
        if (strcmp(s->entries[i].name, name) == 0) return &s->entries[i].value;
    }

    return NULL;
}

static value_t const* scope_find(scope_t* s, char const* name) {
    if (s == NULL) return NULL;

    value_t const* v = scope_find_direct(s, name);
    if (!v) return scope_find(s->parent, name);

    return v;
}

static void scope_add_unchecked(scope_t* s, allocator_t alloc, char const* name,
                                value_t const* value) {
    munit_assert_not_null(s);

    s->entries = da_append_scope_entry(
        s->entries, alloc, &(scope_entry_t){.name = name, .value = *value});
}

static value_t const* scope_add(scope_t* s, allocator_t alloc, char const* name,
                                value_t const* value) {
    value_t const* prev = scope_find(s, name);
    if (prev) return prev;

    scope_add_unchecked(s, alloc, name, value);
    return NULL;
}

static bool scope_is_inside_loop(scope_t* s) {
    if (s == NULL) return false;
    if (s->is_inside_loop) return true;
    if (scope_is_inside_loop(s->parent)) return true;

    return false;
}

static type_id_t typecheck_node(typechecker_t* tc, env_t* env, scope_t* scope,
                                node_t* node);

typedef enum typecheck_node_proc_opt {
    TC_NODE_PROC_OPT_ALLOW_NO_BODY = 1 << 0,
} typecheck_node_proc_opt_t;

static type_id_t typecheck_node_proc(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     typecheck_node_proc_opt_t opt);

static type_id_t eval_to_type(typechecker_t* tc, env_t* env, scope_t* scope,
                              node_t* node) {
    munit_assert_not_null(node);

    if (node->type == NODE_PTR) {
        type_id_t inner = eval_to_type(tc, env, scope, node->as.ptr.child);

        type_id_t ty = typestore_add_type(
            tc->ts, &(type_t){.tag = TYPE_PTR, .as.ptr = {.inner = inner}});
        node->type_id = ty;

        return ty;
    }

    if (node->type == NODE_MPTR) {
        type_id_t inner = eval_to_type(tc, env, scope, node->as.mptr.child);

        if (node->as.mptr.term) {
            report_error(tc->er, node->span,
                         "terminators have not been implemented");
        }

        type_id_t ty = typestore_add_type(
            tc->ts, &(type_t){.tag = TYPE_MPTR, .as.mptr = {.inner = inner}});
        node->type_id = ty;

        return ty;
    }

    if (node->type == NODE_PROC && !node->as.proc.body) {
        return typecheck_node_proc(tc, env, scope, node,
                                   TC_NODE_PROC_OPT_ALLOW_NO_BODY);
    }

    if (node->type != NODE_IDENT) {
        report_error(tc->er, node->span,
                     "expression could not be evaluated to a type");

        node->type_id = tc->ts->primitives.err;
        return node->type_id;
    }

    char const* name = node->as.ident.ident;
    type_id_t   type = typename_table_find(tc->tnt, name);
    if (type_id_eq(type, INVALID_TYPEID)) {
        report_error(tc->er, node->span, "undefined type: %s", name);
        node->type_id = tc->ts->primitives.err;
        return node->type_id;
    }

    node->type_id = tc->ts->primitives.type;
    return type;
}

static type_id_t typecheck_node_ident(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node) {
    (void)env;

    value_t const* v = scope_find(scope, node->as.ident.ident);
    if (!v) {
        report_error(tc->er, node->span, "undeclared identifier %s",
                     node->as.ident.ident);
        return tc->ts->primitives.err;
    }

    return v->type;
}

static type_id_t typecheck_node_binop(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node) {
    type_id_t lhs = typecheck_node(tc, env, scope, node->as.binop.left);
    type_id_t rhs = typecheck_node(tc, env, scope, node->as.binop.right);

    if (!type_id_eq(lhs, rhs)) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);
        char const* rhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, rhs);

        report_error(tc->er, node->span,
                     "incompatible types in %s, expected %s but got %s",
                     binop_to_str(node->as.binop.type), lhsstr, rhsstr);
        report_note(tc->er, node->as.binop.left->span, "this has type %s",
                    lhsstr);
        report_note(tc->er, node->as.binop.right->span, "this has type %s",
                    rhsstr);
    }

    type_t const* lhs_type = typestore_find_type(tc->ts, lhs);
    munit_assert_not_null(lhs_type);

    if (lhs_type->tag != TYPE_INT && lhs_type->tag != TYPE_FLOAT) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);

        report_error(tc->er, node->span, "type %s does not support %s", lhsstr,
                     binop_to_str(node->as.binop.type));
    }

    return lhs;
}

static type_id_t typecheck_node_unop(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    type_id_t child = typecheck_node(tc, env, scope, node->as.unop.child);

    type_t const* child_type = typestore_find_type(tc->ts, child);
    munit_assert_not_null(child_type);

    if (node->as.unop.type == UNOP_NEG && child_type->tag != TYPE_INT &&
        child_type->tag != TYPE_FLOAT) {
        char const* childstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, child);

        report_error(tc->er, node->span, "type %s does not support %s",
                     childstr, unop_to_str(node->as.unop.type));
    } else if (node->as.unop.type == UNOP_NOT && child_type->tag != TYPE_BOOL) {
        char const* childstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, child);

        report_error(tc->er, node->span, "type %s does not support %s",
                     childstr, unop_to_str(node->as.unop.type));
    }

    return child;
}

static type_id_t typecheck_node_logic(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node) {
    type_id_t lhs = typecheck_node(tc, env, scope, node->as.logic.left);
    type_id_t rhs = typecheck_node(tc, env, scope, node->as.logic.right);

    if (!type_id_eq(lhs, rhs)) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);
        char const* rhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, rhs);

        report_error(tc->er, node->span,
                     "incompatible types in %s, expected %s but got %s",
                     logic_to_str(node->as.logic.type), lhsstr, rhsstr);
        report_note(tc->er, node->as.logic.left->span, "this has type %s",
                    lhsstr);
        report_note(tc->er, node->as.logic.right->span, "this has type %s",
                    rhsstr);
    }

    type_t const* lhs_type = typestore_find_type(tc->ts, lhs);
    munit_assert_not_null(lhs_type);

    if (lhs_type->tag != TYPE_BOOL && lhs_type->tag != TYPE_BOOL) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);

        report_error(tc->er, node->span, "type %s does not support %s", lhsstr,
                     logic_to_str(node->as.logic.type));
    }

    return lhs;
}

static type_id_t typecheck_node_comp(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    type_id_t lhs = typecheck_node(tc, env, scope, node->as.comp.left);
    type_id_t rhs = typecheck_node(tc, env, scope, node->as.comp.right);

    if (!type_id_eq(lhs, rhs)) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);
        char const* rhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, rhs);

        report_error(tc->er, node->span,
                     "incompatible types in %s, expected %s but got %s",
                     comp_to_str(node->as.comp.type), lhsstr, rhsstr);
        report_note(tc->er, node->as.comp.left->span, "this has type %s",
                    lhsstr);
        report_note(tc->er, node->as.comp.right->span, "this has type %s",
                    rhsstr);
    }

    type_t const* lhs_type = typestore_find_type(tc->ts, lhs);
    munit_assert_not_null(lhs_type);

    if (lhs_type->tag != TYPE_INT && lhs_type->tag != TYPE_FLOAT) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);

        report_error(tc->er, node->span, "type %s does not support %s", lhsstr,
                     comp_to_str(node->as.comp.type));
    }

    return tc->ts->primitives.bool_;
}

static type_id_t typecheck_node_call(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    node_call_t* call = &node->as.call;

    type_id_t     callee = typecheck_node(tc, env, scope, call->callee);
    type_t const* callee_type = typestore_find_type(tc->ts, callee);
    munit_assert_not_null(callee_type);

    if (callee_type->tag != TYPE_PROC) {
        char const* calleestr =
            typestore_type_to_str(tc->ts, tc->tempalloc, callee_type);

        report_error(tc->er, call->callee->span,
                     "can't call non procedure value of type %s", calleestr);

        return tc->ts->primitives.err;
    }

    size_t proc_argc = da_get_size(callee_type->as.proc.args);
    size_t argc = da_get_size(call->args);
    if (argc != proc_argc) {
        report_error(tc->er, node->span,
                     "procedure expects %d arguments, but %d were given",
                     proc_argc, argc);
    }

    argc = min(argc, proc_argc);
    for (size_t i = 0; i < argc; ++i) {
        type_id_t expectedt = callee_type->as.proc.args[i];
        type_id_t argt = typecheck_node(tc, env, scope, call->args[i]);

        if (!type_id_eq(expectedt, argt)) {
            char const* expectedstr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, expectedt);
            char const* argstr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, argt);

            report_error(tc->er, call->args[i]->span,
                         "procedure expects type %s at position %d, got %s",
                         expectedstr, i + 1, argstr);
        }
    }

    // FIXME: This is probably not how we want to do this
    if (call->callee->type == NODE_IDENT) {
        value_t const* v = scope_find(scope, call->callee->as.ident.ident);
        munit_assert_not_null(v);

        call->call_extern_name = v->extern_name;
    }

    return callee_type->as.proc.return_type;
}

static type_id_t typecheck_node_ref(typechecker_t* tc, env_t* env,
                                    scope_t* scope, node_t* node) {
    type_id_t child = typecheck_node(tc, env, scope, node->as.ref.child);

    return typestore_add_type(
        tc->ts, &(type_t){.tag = TYPE_PTR, .as.ptr = {.inner = child}});
}

static type_id_t typecheck_node_deref(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node) {
    type_id_t     inner = typecheck_node(tc, env, scope, node->as.deref.child);
    type_t const* inner_type = typestore_find_type(tc->ts, inner);
    munit_assert_not_null(inner_type);

    if (inner_type->tag != TYPE_PTR) {
        char const* innerstr =
            typestore_type_to_str(tc->ts, tc->tempalloc, inner_type);

        report_error(tc->er, node->as.deref.child->span,
                     "can't dereference non-pointer value of type %s",
                     innerstr);

        return tc->ts->primitives.err;
    }

    return inner_type->as.ptr.inner;
}

static type_id_t typecheck_node_cast(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    type_id_t child = typecheck_node(tc, env, scope, node->as.cast.child);
    type_id_t target = eval_to_type(tc, env, scope, node->as.cast.type);

    if (type_id_eq(child, target)) {
        char const* childstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, child);
        char const* targetstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, target);

        report_error(tc->er, node->span, "can't cast from %s to itself",
                     childstr);

        report_note(tc->er, node->as.cast.child->span,
                    "this already has type %s", targetstr);

        return target;
    }

    type_t const* child_type = typestore_find_type(tc->ts, child);
    munit_assert_not_null(child_type);
    type_t const* target_type = typestore_find_type(tc->ts, target);
    munit_assert_not_null(target_type);

    if (child_type->tag != target_type->tag) {
        char const* childstr =
            typestore_type_to_str(tc->ts, tc->tempalloc, child_type);
        char const* targetstr =
            typestore_type_to_str(tc->ts, tc->tempalloc, target_type);

        report_error(tc->er, node->span, "can't cast from %s to %s", childstr,
                     targetstr);

        report_note(tc->er, node->as.cast.child->span, "this has type %s",
                    childstr);
        report_note(tc->er, node->as.cast.type->span, "this has type %s",
                    targetstr);

        return target;
    }

    if (child_type->tag == TYPE_INT) {
        return target;
    }

    char const* childstr =
        typestore_type_to_str(tc->ts, tc->tempalloc, child_type);
    char const* targetstr =
        typestore_type_to_str(tc->ts, tc->tempalloc, target_type);

    report_error(tc->er, node->span, "can't cast from %s to %s", childstr,
                 targetstr);

    report_note(tc->er, node->as.cast.child->span, "this has type %s",
                childstr);
    report_note(tc->er, node->as.cast.type->span, "this has type %s",
                targetstr);

    return target;
}

static type_id_t typecheck_node_index(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node) {
    type_id_t receiver =
        typecheck_node(tc, env, scope, node->as.index.receiver);
    type_id_t index = typecheck_node(tc, env, scope, node->as.index.index);

    type_t const* receiver_type = typestore_find_type(tc->ts, receiver);
    munit_assert_not_null(receiver_type);
    type_t const* index_type = typestore_find_type(tc->ts, index);
    munit_assert_not_null(index_type);

    if (index_type->tag != TYPE_INT) {
        char const* indexstr =
            typestore_type_to_str(tc->ts, tc->tempalloc, index_type);
        report_error(tc->er, node->as.index.index->span,
                     "index must be an integer, got %s", indexstr);
    }

    if (receiver_type->tag != TYPE_ARRAY) {
        char const* receiverstr =
            typestore_type_to_str(tc->ts, tc->tempalloc, receiver_type);
        report_error(tc->er, node->as.index.receiver->span,
                     "can't index into non-array %s", receiverstr);

        return tc->ts->primitives.err;
    }

    return receiver_type->as.array.inner;
}

static type_id_t typecheck_node_stmt_expr(typechecker_t* tc, env_t* env,
                                          scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t err = tc->ts->primitives.err;

    type_id_t expr = typecheck_node(tc, env, scope, node->as.stmt_expr.expr);
    if (!type_id_eq(expr, void_) && !type_id_eq(expr, err)) {
        char const* exprstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, expr);

        report_error(tc->er, node->span, "expression result unused");
        report_note(tc->er, node->as.stmt_expr.expr->span,
                    "this expression has type %s", exprstr);
    }

    return void_;
}

static type_id_t typecheck_node_stmt_ret(typechecker_t* tc, env_t* env,
                                         scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t err = tc->ts->primitives.err;

    if (!type_id_is_valid(env->curr_proc_type)) {
        report_error(tc->er, node->span,
                     "can't return outside of procedure body");
        return err;
    }

    // if (env->has_returned) {
    //     // todo: Allow for more than one return
    //     report_error(tc->er, tc->filename, tc->source, node->span,
    //                  "each function can only return once");
    // }

    type_id_t expr = void_;
    if (node->as.stmt_ret.child) {
        expr = typecheck_node(tc, env, scope, node->as.stmt_ret.child);
    }

    if (!type_id_eq(env->expected_return, expr)) {
        char const* retstr = typestore_type_id_to_str(tc->ts, tc->tempalloc,
                                                      env->expected_return);
        char const* exprstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, expr);

        report_error(tc->er, node->span,
                     "incompatible types in return, expected %s but got %s",
                     retstr, exprstr);
        report_note(tc->er, env->proc_type_span, "return type declared here");
    }

    env->has_returned = true;
    return void_;
}

static type_id_t typecheck_node_stmt_break(typechecker_t* tc, env_t* env,
                                           scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;

    if (node->as.stmt_break.child) {
        typecheck_node(tc, env, scope, node->as.stmt_break.child);
    }

    if (node->as.stmt_break.child) {
        report_error(tc->er, node->span,
                     "breaks with payloads have not been implemented");
    }

    if (!scope_is_inside_loop(scope)) {
        report_error(tc->er, node->span, "can't break when outside of loop");
    }

    env->has_broken = true;

    return void_;
}

static type_id_t typecheck_node_stmt_if(typechecker_t* tc, env_t* env,
                                        scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t bool_ = tc->ts->primitives.bool_;

    // bool had_returned = env->has_returned;
    env->has_returned = false;

    type_id_t condition =
        typecheck_node(tc, env, scope, node->as.stmt_if.condition);
    if (!type_id_eq(condition, bool_)) {
        char const* conditionstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, condition);
        report_error(
            tc->er, node->as.stmt_if.condition->span,
            "incompatible types in comparison, expected bool but got %s",
            conditionstr);
    }

    typecheck_node(tc, env, scope, node->as.stmt_if.when_true);
    bool wt_returned = env->has_returned;
    bool wf_returned = false;

    env->has_returned = false;

    if (node->as.stmt_if.when_false) {
        typecheck_node(tc, env, scope, node->as.stmt_if.when_false);
        wf_returned = env->has_returned;
    }

    // env->has_returned = (wt_returned && wf_returned) || had_returned;
    env->has_returned = wt_returned && wf_returned;

    return void_;
}

static type_id_t typecheck_node_stmt_while(typechecker_t* tc, env_t* env,
                                           scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t bool_ = tc->ts->primitives.bool_;

    // bool had_returned = env->has_returned;
    env->has_returned = false;
    bool had_broken = env->has_broken;
    env->has_broken = false;

    type_id_t condition =
        typecheck_node(tc, env, scope, node->as.stmt_while.condition);
    if (!type_id_eq(condition, bool_)) {
        char const* conditionstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, condition);
        report_error(
            tc->er, node->as.stmt_while.condition->span,
            "incompatible types in comparison, expected bool but got %s",
            conditionstr);
    }

    scope_t blkscope;
    scope_init(&blkscope, tc->tempalloc, scope, true);

    typecheck_node(tc, env, &blkscope, node->as.stmt_while.body);

    // if a break was found, then any returns found inside the while are no
    // longer valid.
    if (env->has_broken) {
        env->has_returned = false;
    }

    env->has_broken = had_broken;

    return void_;
}

static type_id_t typecheck_node_stmt_blk(typechecker_t* tc, env_t* env,
                                         scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;

    scope_t blkscope;
    scope_init(&blkscope, tc->tempalloc, scope, false);

    uint32_t size = da_get_size(node->as.stmt_blk.stmts);
    for (uint32_t i = 0; i < size; ++i) {
        type_id_t id =
            typecheck_node(tc, env, &blkscope, node->as.stmt_blk.stmts[i]);
        if (!type_id_eq(id, void_)) {
            char const* typestr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, id);
            report_error(tc->er, node->span,
                         "invalid type for block scope declaration: %s",
                         typestr);
        }
    }

    return void_;
}

static type_id_t typecheck_node_mod(typechecker_t* tc, env_t* env,
                                    scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;

    uint32_t size = da_get_size(node->as.mod.decls);
    for (uint32_t i = 0; i < size; ++i) {
        node_t* it = node->as.mod.decls[i];
        if (it->type != NODE_DECL) {
            report_error(tc->er, it->span,
                         "only declarations are allowed at module "
                         "scope, found %s",
                         node_type_to_str(it->type));
            continue;
        }

        type_id_t id = typecheck_node(tc, env, scope, it);
        if (!type_id_eq(id, void_)) {
            char const* typestr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, id);
            report_error(tc->er, node->span,
                         "invalid type for module scope declaration: %s",
                         typestr);
        }
    }

    return void_;
}

static type_id_t typecheck_node_decl(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    type_id_t    void_ = tc->ts->primitives.void_;
    type_id_t    err = tc->ts->primitives.err;
    node_decl_t* decl = &node->as.decl;

    // FIXME: view other places that talk about extern name as for why this is
    // wrong
    char const* extern_name = decl->is_extern ? decl->extern_name : decl->name;

    type_id_t type = INVALID_TYPEID;
    if (decl->type) type = eval_to_type(tc, env, scope, decl->type);

    type_id_t init = INVALID_TYPEID;
    if (decl->init) {
        init = typecheck_node(tc, env, scope, decl->init);
        if (decl->is_extern) {
            report_error(tc->er, node->span,
                         "extern declarations can't have initializers");
            init = err;
        }
    }

    if (type_id_is_valid(type) && type_id_is_valid(init) &&
        !type_id_eq(type, init)) {
        char const* typestr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, type);
        char const* initstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, init);

        report_error(tc->er, node->span,
                     "incompatible types in declaration of %s, "
                     "expected %s but got %s",
                     decl->name, typestr, initstr);
        report_note(tc->er, decl->type->span, "this has type %s", typestr);
        report_note(tc->er, decl->init->span, "this has type %s", initstr);
    }

    if (type_id_eq(type, INVALID_TYPEID)) {
        type = init;
    }

    if (!type_id_is_valid(type)) {
        // save as void in case we have no type or initializer
        type = void_;
    }

    // FIXME: Check if using temp_alloc here is what we want
    value_t const* prev = scope_add(scope, tc->tempalloc, decl->name,
                                    &(value_t){.type = type,
                                               .where = decl->name_span,
                                               .extern_name = extern_name});
    if (prev) {
        report_error(tc->er, node->span, "identifier %s already defined",
                     decl->name);
        if (prev->where.start != 0 && prev->where.end != 0)
            report_note(tc->er, prev->where, "declared here");
    }

    return void_;
}

static type_id_t typecheck_node_assign(typechecker_t* tc, env_t* env,
                                       scope_t* scope, node_t* node) {
    node_t*   lhs_node = node->as.assign.lhs;
    type_id_t lhs = typecheck_node(tc, env, scope, lhs_node);
    type_id_t rhs = typecheck_node(tc, env, scope, node->as.assign.rhs);

    bool lhs_is_lvalue = false;
    switch (lhs_node->type) {
        case NODE_DEREF:
        case NODE_IDENT: lhs_is_lvalue = true; break;
        default: break;
    }

    if (!lhs_is_lvalue) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);
        report_error(tc->er, lhs_node->span,
                     "can't assign to rvalue of type %s", lhsstr);

        return tc->ts->primitives.void_;
    }

    if (!type_id_eq(lhs, rhs)) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, lhs);
        char const* rhsstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, rhs);
        report_error(tc->er, node->span,
                     "incompatible types in assignment, expected %s but got %s",
                     lhsstr, rhsstr);
        report_note(tc->er, node->as.binop.left->span, "this has type %s",
                    lhsstr);
        report_note(tc->er, node->as.binop.right->span, "this has type %s",
                    rhsstr);
    }

    return tc->ts->primitives.void_;
}

static type_id_t typecheck_node_proc(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     typecheck_node_proc_opt_t opt) {
    (void)env;

    type_id_t void_ = tc->ts->primitives.void_;

    node_proc_t* proc = &node->as.proc;
    type_id_t*   args = da_init_type_id(tc->alloc);

    scope_t proc_scope;
    scope_init(&proc_scope, tc->tempalloc, scope, false);

    size_t argc = da_get_size(proc->args);
    for (size_t i = 0; i < argc; ++i) {
        node_t* arg_node = proc->args[i];
        munit_assert_uint8(arg_node->type, ==, NODE_ARG);

        arg_node->type_id = void_;

        node_arg_t* arg = &arg_node->as.arg;
        if (arg->type) {
            type_id_t argtype = eval_to_type(tc, env, &proc_scope, arg->type);
            scope_add(&proc_scope, tc->tempalloc, arg->name,
                      &(value_t){.type = argtype, .where = arg_node->span});

            args = da_append_type_id(args, tc->alloc, &argtype);
        } else {
            report_error(tc->er, arg_node->span,
                         "arguments without types (using inference) "
                         "are not supported (yet)");
        }
    }

    type_id_t ret = void_;
    if (proc->return_type) {
        ret = eval_to_type(tc, env, &proc_scope, proc->return_type);
    }

    type_id_t result = typestore_add_type(
        tc->ts,
        &(type_t){
            .tag = TYPE_PROC, .as.proc = {.return_type = ret, .args = args}
    });

    type_id_t body = INVALID_TYPEID;

    if (proc->body) {
        env_t body_env = {.expected_return = ret,
                          .curr_proc_type = result,
                          .proc_type_span = proc->return_type
                                                ? proc->return_type->span
                                                : node->span};
        body = typecheck_node(tc, &body_env, &proc_scope, proc->body);
        if (!type_id_eq(body, void_)) {
            char const* bodystr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, body);
            report_error(tc->er, node->span,
                         "invalid type for procedure body: %s", bodystr);
        }

        if (!body_env.has_returned &&
            !type_id_eq(body_env.expected_return, void_)) {
            char const* retstr = typestore_type_id_to_str(
                tc->ts, tc->tempalloc, body_env.expected_return);

            report_error(tc->er, node->span,
                         "procedure with non-void return type returns "
                         "implicitly. Expected to return value of type %s",
                         retstr);

            report_note(tc->er, body_env.proc_type_span,
                        "return type declared here");
        }

        proc->uses_implicit_return = !body_env.has_returned;
    }

    if (!proc->body && (opt & TC_NODE_PROC_OPT_ALLOW_NO_BODY) == 0) {
        report_error(tc->er, node->span, "procedure is missing a body");
    }

    return result;
}

static type_id_t typecheck_node_array(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node) {
    type_id_t     err = tc->ts->primitives.err;
    node_array_t* array = &node->as.array;
    munit_assert_not_null(array);

    if (array->len->type != NODE_IDENT ||
        strcmp(array->len->as.ident.ident, "_") != 0) {
        report_error(tc->er, array->len->span,
                     "arrays with explicit size are not supported (yet)");
        return err;
    }

    type_id_t type = eval_to_type(tc, env, scope, array->type);
    size_t    count = da_get_size(array->initializer_list);
    for (size_t i = 0; i < count; ++i) {
        type_id_t expr =
            typecheck_node(tc, env, scope, array->initializer_list[i]);
        if (!type_id_eq(type, expr)) {
            char const* typestr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, type);
            char const* exprstr =
                typestore_type_id_to_str(tc->ts, tc->tempalloc, expr);

            report_error(tc->er, node->span,
                         "incompatible types in array, expected %s but got %s",
                         typestr, exprstr);
            report_note(tc->er, array->type->span, "array has type %s",
                        typestr);
            report_note(tc->er, array->initializer_list[i]->span,
                        "this has type %s", exprstr);
        }
    }

    return typestore_add_type(
        tc->ts,
        &(type_t){
            .tag = TYPE_ARRAY, .as.array = {.len = count, .inner = type}
    });
}

static type_id_t typecheck_node(typechecker_t* tc, env_t* env, scope_t* scope,
                                node_t* node) {
    munit_assert_not_null(node);

    type_id_t err = tc->ts->primitives.err;
    type_id_t result = {0xFE};

    switch (node->type) {
        case NODE_ERR:
            report_error(tc->er, node->span, "found error node in typecheck");
            result = err;
            break;
        case NODE_INT: result = tc->ts->primitives.i32; break;
        case NODE_FLOAT: result = tc->ts->primitives.f64; break;
        case NODE_STR: result = tc->ts->primitives.err; break;
        case NODE_IDENT:
            result = typecheck_node_ident(tc, env, scope, node);
            break;
        case NODE_BINOP:
            result = typecheck_node_binop(tc, env, scope, node);
            break;
        case NODE_UNOP:
            result = typecheck_node_unop(tc, env, scope, node);
            break;
        case NODE_LOGIC:
            result = typecheck_node_logic(tc, env, scope, node);
            break;
        case NODE_COMP:
            result = typecheck_node_comp(tc, env, scope, node);
            break;
        case NODE_CALL:
            result = typecheck_node_call(tc, env, scope, node);
            break;
        case NODE_REF: result = typecheck_node_ref(tc, env, scope, node); break;
        case NODE_DEREF:
            result = typecheck_node_deref(tc, env, scope, node);
            break;
        case NODE_CAST:
            result = typecheck_node_cast(tc, env, scope, node);
            break;
        case NODE_INDEX:
            result = typecheck_node_index(tc, env, scope, node);
            break;
        case NODE_STMT_EXPR:
            result = typecheck_node_stmt_expr(tc, env, scope, node);
            break;
        case NODE_STMT_RET:
            result = typecheck_node_stmt_ret(tc, env, scope, node);
            break;
        case NODE_STMT_BREAK:
            result = typecheck_node_stmt_break(tc, env, scope, node);
            break;
        case NODE_STMT_IF:
            result = typecheck_node_stmt_if(tc, env, scope, node);
            break;
        case NODE_STMT_WHILE:
            result = typecheck_node_stmt_while(tc, env, scope, node);
            break;
        case NODE_STMT_BLK:
            result = typecheck_node_stmt_blk(tc, env, scope, node);
            break;
        case NODE_MOD: result = typecheck_node_mod(tc, env, scope, node); break;
        case NODE_DECL:
            result = typecheck_node_decl(tc, env, scope, node);
            break;
        case NODE_ASSIGN:
            result = typecheck_node_assign(tc, env, scope, node);
            break;
        case NODE_ARG: break;
        case NODE_PROC:
            result = typecheck_node_proc(tc, env, scope, node, 0);
            break;
        case NODE_ARRAY:
            result = typecheck_node_array(tc, env, scope, node);
            break;
        case NODE_PTR: break;
        case NODE_MPTR: break;
    }

    node->type_id = result;

    if (type_id_eq(result, (type_id_t){0xFE})) {
        report_error(tc->er, node->span, "unimplemented node type: '%s'",
                     node_type_to_str(node->type));

        // munit_assert(false);
    }

    return result;
}

void pass_typecheck(typecheck_params_t const* params) {
    allocator_t temp_alloc;
    Arena       temp_arena = {0};
    allocator_init_arena(&temp_alloc, &temp_arena);

    typename_table_t tnt = {
        .entries = da_init_typename_table_entry(temp_alloc),
    };

    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "i8",
                                  .type = params->ts->primitives.i8});
    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "i32",
                                  .type = params->ts->primitives.i32});
    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "void",
                                  .type = params->ts->primitives.void_});
    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "f32",
                                  .type = params->ts->primitives.f32});
    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "f64",
                                  .type = params->ts->primitives.f64});
    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "bool",
                                  .type = params->ts->primitives.bool_});

    typechecker_t tc = {
        .er = params->er,
        .ts = params->ts,
        .tnt = &tnt,
        .tempalloc = temp_alloc,
        .alloc = params->alloc,
    };

    env_t root_env = {};

    // FIXME: Check if temp_alloc is what we want for this
    scope_t root_scope = {};
    scope_init(&root_scope, tc.tempalloc, NULL, false);
    scope_add(&root_scope, tc.tempalloc, "true",
              &(value_t){.type = tc.ts->primitives.bool_});
    scope_add(&root_scope, tc.tempalloc, "false",
              &(value_t){.type = tc.ts->primitives.bool_});

    typecheck_node(&tc, &root_env, &root_scope, params->ast);

    arena_free(&temp_arena);
}
