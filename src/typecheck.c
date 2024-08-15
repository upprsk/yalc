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
    char const*       filename;
    char const*       source;
    uint32_t          source_len;

    typestore_t*      ts;
    typename_table_t* tnt;

    allocator_t temp_alloc;
} typechecker_t;

typedef struct env {
    type_id_t expected_return;
    type_id_t curr_proc_type;

    span_t proc_type_span;

    // TODO: Allow more than one return per procedure
    bool has_returned;
} env_t;

typedef struct value {
    type_id_t type;
    span_t    where;
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
} scope_t;

static void scope_init(scope_t* s, allocator_t alloc, scope_t* parent) {
    *s = (scope_t){.entries = da_init_scope_entry(alloc), .parent = parent};
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
            report_error(tc->er, tc->filename, tc->source, node->span,
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
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "expression could not be evaluated to a type");

        node->type_id = tc->ts->primitives.err;
        return node->type_id;
    }

    char const* name = node->as.ident.ident;
    type_id_t   type = typename_table_find(tc->tnt, name);
    if (type_id_eq(type, INVALID_TYPEID)) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "undefined type: %s", name);
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
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "undeclared identifier %s", node->as.ident.ident);
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
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, lhs);
        char const* rhsstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, rhs);

        report_error(tc->er, tc->filename, tc->source, node->span,
                     "incompatible types in %s, expected %s but got %s",
                     binop_to_str(node->as.binop.type), lhsstr, rhsstr);
        report_note(tc->er, tc->filename, tc->source, node->as.binop.left->span,
                    "this has type %s", lhsstr);
        report_note(tc->er, tc->filename, tc->source,
                    node->as.binop.right->span, "this has type %s", rhsstr);
    }

    type_t const* lhs_type = typestore_find_type(tc->ts, lhs);
    munit_assert_not_null(lhs_type);

    if (lhs_type->tag != TYPE_INT && lhs_type->tag != TYPE_FLOAT) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, lhs);

        report_error(tc->er, tc->filename, tc->source, node->span,
                     "type %s does not support %s", lhsstr,
                     binop_to_str(node->as.binop.type));
    }

    return lhs;
}

static type_id_t typecheck_node_unop(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    type_id_t child = typecheck_node(tc, env, scope, node->as.unop.child);

    type_t const* child_type = typestore_find_type(tc->ts, child);
    munit_assert_not_null(child_type);

    if (child_type->tag != TYPE_INT && child_type->tag != TYPE_FLOAT) {
        char const* childstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, child);

        report_error(tc->er, tc->filename, tc->source, node->span,
                     "type %s does not support %s", childstr,
                     unop_to_str(node->as.unop.type));
    }

    return child;
}

static type_id_t typecheck_node_call(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node) {
    node_call_t* call = &node->as.call;

    type_id_t     callee = typecheck_node(tc, env, scope, call->callee);
    type_t const* callee_type = typestore_find_type(tc->ts, callee);
    munit_assert_not_null(callee_type);

    if (callee_type->tag != TYPE_PROC) {
        char const* calleestr =
            typestore_type_to_str(tc->ts, tc->temp_alloc, callee_type);

        report_error(tc->er, tc->filename, tc->source, call->callee->span,
                     "can't call non procedure value of type %s", calleestr);

        return tc->ts->primitives.err;
    }

    size_t proc_argc = da_get_size(callee_type->as.proc.args);
    size_t argc = da_get_size(call->args);
    if (argc != proc_argc) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "procedure expects %d arguments, but %d were given",
                     proc_argc, argc);
    }

    argc = min(argc, proc_argc);
    for (size_t i = 0; i < argc; ++i) {
        type_id_t expectedt = callee_type->as.proc.args[i];
        type_id_t argt = typecheck_node(tc, env, scope, call->args[i]);

        if (!type_id_eq(expectedt, argt)) {
            char const* expectedstr =
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, expectedt);
            char const* argstr =
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, argt);

            report_error(tc->er, tc->filename, tc->source, call->args[i]->span,
                         "procedure expects type %s at position %d, got %s",
                         expectedstr, i + 1, argstr);
        }
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
            typestore_type_to_str(tc->ts, tc->temp_alloc, inner_type);

        report_error(
            tc->er, tc->filename, tc->source, node->as.deref.child->span,
            "can't dereference non-pointer value of type %s", innerstr);

        return tc->ts->primitives.err;
    }

    return inner_type->as.ptr.inner;
}

static type_id_t typecheck_node_stmt_expr(typechecker_t* tc, env_t* env,
                                          scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t err = tc->ts->primitives.err;

    type_id_t expr = typecheck_node(tc, env, scope, node->as.stmt_expr.expr);
    if (!type_id_eq(expr, void_) && !type_id_eq(expr, err)) {
        char const* exprstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, expr);

        report_error(tc->er, tc->filename, tc->source, node->span,
                     "expression result unused");
        report_note(tc->er, tc->filename, tc->source,
                    node->as.stmt_expr.expr->span,
                    "this expression has type %s", exprstr);
    }

    return void_;
}

static type_id_t typecheck_node_stmt_ret(typechecker_t* tc, env_t* env,
                                         scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t err = tc->ts->primitives.err;

    if (!type_id_is_valid(env->curr_proc_type)) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "can't return outside of procedure body");
        return err;
    }

    if (env->has_returned) {
        // TODO: Allow for more than one return
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "each function can only return once");
    }

    type_id_t expr = void_;
    if (node->as.stmt_ret.child) {
        expr = typecheck_node(tc, env, scope, node->as.stmt_ret.child);
    }

    if (!type_id_eq(env->expected_return, expr)) {
        char const* retstr = typestore_type_id_to_str(tc->ts, tc->temp_alloc,
                                                      env->expected_return);
        char const* exprstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, expr);

        report_error(tc->er, tc->filename, tc->source, node->span,
                     "incompatible types in return, expected %s but got %s",
                     retstr, exprstr);
        report_note(tc->er, tc->filename, tc->source, env->proc_type_span,
                    "return type declared here");
    }

    env->has_returned = true;
    return void_;
}

static type_id_t typecheck_node_stmt_if(typechecker_t* tc, env_t* env,
                                        scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t bool_ = tc->ts->primitives.bool_;

    type_id_t condition =
        typecheck_node(tc, env, scope, node->as.stmt_if.condition);
    if (!type_id_eq(condition, bool_)) {
        char const* conditionstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, condition);
        report_error(
            tc->er, tc->filename, tc->source, node->as.stmt_if.condition->span,
            "incompatible types in comparison, expected bool but got %s",
            conditionstr);
    }

    typecheck_node(tc, env, scope, node->as.stmt_if.when_true);
    if (node->as.stmt_if.when_false)
        typecheck_node(tc, env, scope, node->as.stmt_if.when_false);

    return void_;
}

static type_id_t typecheck_node_stmt_blk(typechecker_t* tc, env_t* env,
                                         scope_t* scope, node_t* node) {
    type_id_t void_ = tc->ts->primitives.void_;

    scope_t blkscope;
    scope_init(&blkscope, tc->temp_alloc, scope);

    uint32_t size = da_get_size(node->as.stmt_blk.stmts);
    for (uint32_t i = 0; i < size; ++i) {
        type_id_t id =
            typecheck_node(tc, env, &blkscope, node->as.stmt_blk.stmts[i]);
        if (!type_id_eq(id, void_)) {
            char const* typestr =
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, id);
            report_error(tc->er, tc->filename, tc->source, node->span,
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
            report_error(tc->er, tc->filename, tc->source, it->span,
                         "only declarations are allowed at module "
                         "scope, found %s",
                         node_type_to_str(it->type));
            continue;
        }

        type_id_t id = typecheck_node(tc, env, scope, it);
        if (!type_id_eq(id, void_)) {
            char const* typestr =
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, id);
            report_error(tc->er, tc->filename, tc->source, node->span,
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

    type_id_t type = INVALID_TYPEID;
    if (decl->type) type = eval_to_type(tc, env, scope, decl->type);

    type_id_t init = INVALID_TYPEID;
    if (decl->init) {
        init = typecheck_node(tc, env, scope, decl->init);
        if (decl->is_extern) {
            report_error(tc->er, tc->filename, tc->source, node->span,
                         "extern declarations can't have initializers");
            init = err;
        }
    }

    if (type_id_is_valid(type) && type_id_is_valid(init) &&
        !type_id_eq(type, init)) {
        char const* typestr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, type);
        char const* initstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, init);

        report_error(tc->er, tc->filename, tc->source, node->span,
                     "incompatible types in declaration of %s, "
                     "expected %s but got %s",
                     decl->name, typestr, initstr);
        report_note(tc->er, tc->filename, tc->source, decl->type->span,
                    "this has type %s", typestr);
        report_note(tc->er, tc->filename, tc->source, decl->init->span,
                    "this has type %s", initstr);
    }

    if (type_id_eq(type, INVALID_TYPEID)) {
        type = init;
    }

    if (!type_id_is_valid(type)) {
        // save as void in case we have no type or initializer
        type = void_;
    }

    // FIXME: Check if using temp_alloc here is what we want
    value_t const* prev =
        scope_add(scope, tc->temp_alloc, decl->name,
                  &(value_t){.type = type, .where = decl->name_span});
    if (prev) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "identifier %s already defined", decl->name);
        if (prev->where.start != 0 && prev->where.end != 0)
            report_note(tc->er, tc->filename, tc->source, prev->where,
                        "declared here");
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
        case NODE_ERR:
        case NODE_INT:
        case NODE_FLOAT:
        case NODE_STR:
        case NODE_BINOP:
        case NODE_UNOP:
        case NODE_CALL:
        case NODE_STMT_EXPR:
        case NODE_STMT_RET:
        case NODE_STMT_IF:
        case NODE_STMT_BLK:
        case NODE_MOD:
        case NODE_DECL:
        case NODE_ASSIGN:
        case NODE_ARG:
        case NODE_PROC:
        case NODE_ARRAY:
        case NODE_PTR:
        case NODE_REF:
        case NODE_MPTR: break;
        case NODE_DEREF:
        case NODE_IDENT: lhs_is_lvalue = true; break;
    }

    if (!lhs_is_lvalue) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, lhs);
        report_error(tc->er, tc->filename, tc->source, lhs_node->span,
                     "can't assign to rvalue of type %s", lhsstr);

        return tc->ts->primitives.void_;
    }

    if (!type_id_eq(lhs, rhs)) {
        char const* lhsstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, lhs);
        char const* rhsstr =
            typestore_type_id_to_str(tc->ts, tc->temp_alloc, rhs);
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "incompatible types in assignment, expected %s but got %s",
                     lhsstr, rhsstr);
        report_note(tc->er, tc->filename, tc->source, node->as.binop.left->span,
                    "this has type %s", lhsstr);
        report_note(tc->er, tc->filename, tc->source,
                    node->as.binop.right->span, "this has type %s", rhsstr);
    }

    return tc->ts->primitives.void_;
}

static type_id_t typecheck_node_proc(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     typecheck_node_proc_opt_t opt) {
    (void)env;

    type_id_t void_ = tc->ts->primitives.void_;

    node_proc_t* proc = &node->as.proc;
    type_id_t*   args = da_init_type_id(tc->temp_alloc);

    scope_t proc_scope;
    scope_init(&proc_scope, tc->temp_alloc, scope);

    size_t argc = da_get_size(proc->args);
    for (size_t i = 0; i < argc; ++i) {
        node_t* arg_node = proc->args[i];
        munit_assert_uint8(arg_node->type, ==, NODE_ARG);

        arg_node->type_id = void_;

        node_arg_t* arg = &arg_node->as.arg;
        if (arg->type) {
            type_id_t argtype = eval_to_type(tc, env, &proc_scope, arg->type);
            scope_add(&proc_scope, tc->temp_alloc, arg->name,
                      &(value_t){.type = argtype, .where = arg_node->span});

            args = da_append_type_id(args, tc->temp_alloc, &argtype);
        } else {
            report_error(tc->er, tc->filename, tc->source, arg_node->span,
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
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, body);
            report_error(tc->er, tc->filename, tc->source, node->span,
                         "invalid type for procedure body: %s", bodystr);
        }

        if (!body_env.has_returned &&
            !type_id_eq(body_env.expected_return, void_)) {
            char const* retstr = typestore_type_id_to_str(
                tc->ts, tc->temp_alloc, body_env.expected_return);

            report_error(tc->er, tc->filename, tc->source, node->span,
                         "procedure with non-void return type returns "
                         "implicitly. Expected to return value of type %s",
                         retstr);

            report_note(tc->er, tc->filename, tc->source,
                        body_env.proc_type_span, "return type declared here");
        }
    }

    if (!proc->body && (opt & TC_NODE_PROC_OPT_ALLOW_NO_BODY) == 0) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "procedure is missing a body");
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
        report_error(tc->er, tc->filename, tc->source, array->len->span,
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
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, type);
            char const* exprstr =
                typestore_type_id_to_str(tc->ts, tc->temp_alloc, expr);

            report_error(tc->er, tc->filename, tc->source, node->span,
                         "incompatible types in array, expected %s but got %s",
                         typestr, exprstr);
            report_note(tc->er, tc->filename, tc->source, array->type->span,
                        "array has type %s", typestr);
            report_note(tc->er, tc->filename, tc->source,
                        array->initializer_list[i]->span, "this has type %s",
                        exprstr);
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
            report_error(tc->er, tc->filename, tc->source, node->span,
                         "found error node in typecheck");
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
        case NODE_CALL:
            result = typecheck_node_call(tc, env, scope, node);
            break;
        case NODE_REF: result = typecheck_node_ref(tc, env, scope, node); break;
        case NODE_DEREF:
            result = typecheck_node_deref(tc, env, scope, node);
            break;
        case NODE_STMT_EXPR:
            result = typecheck_node_stmt_expr(tc, env, scope, node);
            break;
        case NODE_STMT_RET:
            result = typecheck_node_stmt_ret(tc, env, scope, node);
            break;
        case NODE_STMT_IF:
            result = typecheck_node_stmt_if(tc, env, scope, node);
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
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "unimplemented node type: '%s'",
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
        &(typename_table_entry_t){.name = "i32",
                                  .type = params->ts->primitives.i32});
    tnt.entries = da_append_typename_table_entry(
        tnt.entries, temp_alloc,
        &(typename_table_entry_t){.name = "void",
                                  .type = params->ts->primitives.void_});

    typechecker_t tc = {
        .filename = params->filename,
        .source = params->source,
        .source_len = params->source_len,
        .er = params->er,
        .ts = params->ts,
        .tnt = &tnt,
        .temp_alloc = temp_alloc,
    };

    env_t root_env = {};

    // FIXME: Check if temp_alloc is what we want for this
    scope_t root_scope = {};
    scope_init(&root_scope, tc.temp_alloc, NULL);
    scope_add(&root_scope, tc.temp_alloc, "true",
              &(value_t){.type = tc.ts->primitives.bool_});
    scope_add(&root_scope, tc.temp_alloc, "false",
              &(value_t){.type = tc.ts->primitives.bool_});

    typecheck_node(&tc, &root_env, &root_scope, params->ast);

    arena_free(&temp_arena);
}
