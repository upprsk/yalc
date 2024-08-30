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

typedef struct generic_instance {
    type_id_t generic_id;
    node_t*   proc;
} generic_instance_t;
da_declare(generic_instance_t, generic_instance);

typedef struct generic_bind {
    type_id_t placeholder;
    type_id_t bound_to;
} generic_bind_t;
da_declare(generic_bind_t, generic_bind);

typedef struct typechecker {
    error_reporter_t* er;
    typestore_t*      ts;

    allocator_t tempalloc;
    allocator_t alloc;

    generic_instance_t* generic_instances;
} typechecker_t;

typedef struct env {
    struct env* parent;
    char const* name;

    type_id_t expected_return;
    type_id_t curr_proc_type;

    span_t proc_type_span;

    bool has_returned;
    bool has_broken;
    bool in_type_context;
} env_t;

static inline char* env_gen_full_name_iter(env_t* env, allocator_t alloc,
                                           char* buf) {
    munit_assert_not_null(env);

    size_t len = env->name != NULL ? strlen(env->name) : 0;

    if (env->parent != NULL) {
        buf = env_gen_full_name_iter(env->parent, alloc, buf);

        if (len) buf = da_extend_char(buf, alloc, "_", 1);
    }

    buf = da_extend_char(buf, alloc, env->name, len);

    return buf;
}

static inline char* env_gen_full_name(env_t* env, allocator_t alloc) {
    char* buf = env_gen_full_name_iter(env, alloc, da_init_char(alloc));

    return da_extend_char(buf, alloc, "\0", 1);
}

static inline char* env_gen_module_name_iter(env_t* env, allocator_t alloc,
                                             char* buf) {
    munit_assert_not_null(env);

    size_t len = env->name != NULL ? strlen(env->name) : 0;

    if (env->parent != NULL) {
        buf = env_gen_full_name_iter(env->parent, alloc, buf);

        if (len) buf = da_extend_char(buf, alloc, ".", 1);
    }

    buf = da_extend_char(buf, alloc, env->name, len);

    return buf;
}

static inline char* env_gen_module_name(env_t* env, allocator_t alloc) {
    char* buf = env_gen_module_name_iter(env, alloc, da_init_char(alloc));

    return da_extend_char(buf, alloc, "\0", 1);
}

typedef struct value {
    type_id_t type;
    span_t    where;

    // in case of types, we also want to store it's actual underlying type.
    // `i32` has type `type` but when used in a type context, it is `i32`.
    type_id_t type_payload;

    // FIXME: This is probably not how we should do this. Stores the extern name
    // of a procedure when declared
    char const* extern_name;
} value_t;

static inline type_id_t value_get_type(typestore_t* ts, value_t const* v) {
    munit_assert_not_null(ts);
    munit_assert_not_null(v);
    munit_assert_uint32(v->type.id, ==, ts->primitives.type.id);

    return v->type_payload;
}

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

typedef struct inference {
    type_id_t expression_expected;
} inference_t;

static inline inference_t infer_with(type_id_t type) {
    return (inference_t){.expression_expected = type};
}

static generic_instance_t* find_generic_instance(typechecker_t* tc,
                                                 type_id_t      id) {
    size_t count = da_get_size(tc->generic_instances);
    for (size_t i = 0; i < count; i++) {
        if (type_id_eq(tc->generic_instances[i].generic_id, id))
            return &tc->generic_instances[i];
    }

    return NULL;
}

static void define_generic_instance(typechecker_t* tc, type_id_t id,
                                    node_t* node) {
    munit_assert_null(find_generic_instance(tc, id));

    tc->generic_instances = da_append_generic_instance(
        tc->generic_instances, tc->tempalloc,
        &(generic_instance_t){.generic_id = id, .proc = node});
}

static generic_bind_t* find_generic_bound(generic_bind_t* binds,
                                          type_id_t       placeholder) {
    size_t count = da_get_size(binds);
    for (size_t i = 0; i < count; i++) {
        if (type_id_eq(binds[i].placeholder, placeholder)) return &binds[i];
    }

    return NULL;
}

static type_id_t typecheck_node(typechecker_t* tc, env_t* env, scope_t* scope,
                                node_t* node, inference_t inf);
static type_id_t eval_to_type(typechecker_t* tc, env_t* env, scope_t* scope,
                              node_t* node);

typedef enum typecheck_node_proc_opt {
    TC_NODE_PROC_OPT_ALLOW_NO_BODY = 1 << 0,
} typecheck_node_proc_opt_t;

static type_id_t typecheck_node_proc(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     inference_t               inf,
                                     typecheck_node_proc_opt_t opt);

static value_t const* eval_node_record(typechecker_t* tc, env_t* env,
                                       scope_t* outer_scope, node_t* node) {
    node_record_t* record = &node->as.record;
    if (record->blk->type != NODE_STMT_BLK) {
        report_error(tc->er, record->blk->span,
                     "expected block as child of record, found %s",
                     node_type_to_str(record->blk->type));
        return NULL;
    }

    type_record_t rec_type = {
        .fields = da_init_record_field(tc->alloc),
        .extern_name = env_gen_full_name(env, tc->alloc),
        .inferred_name = env_gen_module_name(env, tc->alloc),
    };

    scope_t scope;
    scope_init(&scope, tc->tempalloc, outer_scope, false);

    node_stmt_block_t* blk = &record->blk->as.stmt_blk;
    size_t             count = da_get_size(blk->stmts);
    for (size_t i = 0; i < count; i++) {
        node_t* node = blk->stmts[i];
        if (node->type != NODE_DECL) {
            report_error(tc->er, node->span, "expected declaration, found %s",
                         node_type_to_str(record->blk->type));
            continue;
        }

        // TODO: validate the declaration, forbid default initializers and add
        // the record scope

        node_decl_t* decl = &node->as.decl;

        if (decl->is_extern) {
            report_error(tc->er, node->span,
                         "fields can not be declared extern");
        }

        char const* field_name = decl->name;
        type_id_t   field_type = eval_to_type(tc, env, &scope, decl->type);

        if (decl->init) {
            report_error(
                tc->er, decl->init->span,
                "default initializers for fields have not been implemented");
        }

        rec_type.fields = da_append_record_field(
            rec_type.fields, tc->alloc,
            &(record_field_t){.name = field_name, .type = field_type});
    }

    type_id_t ty = typestore_add_type(
        tc->ts, &(type_t){.tag = TYPE_RECORD, .as.record = rec_type});

    value_t* v = allocator_alloc(tc->tempalloc, sizeof(*v));
    *v = (value_t){.type = tc->ts->primitives.type,
                   .type_payload = ty,
                   .where = node->span};

    return v;
}

static value_t const* eval_node(typechecker_t* tc, env_t* env, scope_t* scope,
                                node_t* node) {
    munit_assert_not_null(node);

    type_id_t type = tc->ts->primitives.type;

    switch (node->type) {
        case NODE_IDENT: {
            value_t const* v = scope_find(scope, node->as.ident.ident);
            if (v) return v;
        } break;
        case NODE_PROC: {
            type_id_t ty = node->type_id;
            munit_assert_uint8(typestore_find_type(tc->ts, ty)->tag, ==,
                               TYPE_PROC);

            value_t* v = allocator_alloc(tc->tempalloc, sizeof(*v));
            *v = (value_t){
                .type = type, .type_payload = ty, .where = node->span};

            return v;
        } break;
        case NODE_RECORD: return eval_node_record(tc, env, scope, node);
        case NODE_PTR: {
            value_t const* v = eval_node(tc, env, scope, node->as.ptr.child);
            if (v) {
                type_id_t ty = typestore_add_type(
                    tc->ts,
                    &(type_t){.tag = TYPE_PTR,
                              .as.ptr = {.inner = value_get_type(tc->ts, v)}});
                value_t* v = allocator_alloc(tc->tempalloc, sizeof(*v));
                *v = (value_t){
                    .type = type, .type_payload = ty, .where = node->span};

                return v;
            }
        } break;
        case NODE_MPTR: {
            // TODO: Implement terminators

            value_t const* inner =
                eval_node(tc, env, scope, node->as.mptr.child);
            if (inner) {
                type_id_t ty = typestore_add_type(
                    tc->ts, &(type_t){.tag = TYPE_MPTR,
                                      .as.mptr = {.inner = value_get_type(
                                                      tc->ts, inner)}});
                value_t* v = allocator_alloc(tc->tempalloc, sizeof(*v));
                *v = (value_t){
                    .type = type, .type_payload = ty, .where = node->span};

                return v;
            }
        } break;
        default: report_error(tc->er, node->span, "can't evaluate to a type");
    }

    return NULL;
}

static type_id_t eval_to_type(typechecker_t* tc, env_t* outer_env,
                              scope_t* scope, node_t* node) {
    // report_note(tc->er, node->span, "eval_to_type", node->span);

    munit_assert_not_null(node);

    env_t env = *outer_env;
    env.in_type_context = true;

    type_id_t     type = tc->ts->primitives.type;
    type_id_t     err = tc->ts->primitives.err;
    type_id_t     ty = typecheck_node(tc, &env, scope, node, infer_with(type));
    type_t const* ty_inst = typestore_find_type(tc->ts, ty);

    if (!type_id_eq(ty, type) && ty_inst->tag != TYPE_PROC &&
        ty_inst->tag != TYPE_PLACEHOLDER) {
        report_error(tc->er, node->span,
                     "expected a type, got value of type %s",
                     typestore_type_id_to_str(tc->ts, tc->tempalloc, ty));
        return err;
    }

    value_t const* value = eval_node(tc, &env, scope, node);
    if (!value) {
        report_error(tc->er, node->span, "failed to evaluate to type");
        return INVALID_TYPEID;
    }

    return value->type_payload;
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

static type_id_t typecheck_node_kw(typechecker_t* tc, env_t* env,
                                   scope_t* scope, node_t* node) {
    (void)env;
    (void)scope;

    // FIXME: Not allocating a new string for the type and using the same as the
    // node
    type_id_t ty = typestore_add_type(
        tc->ts,
        &(type_t){.tag = TYPE_KW, .as.kw = {.ident = node->as.kw.ident}});

    return ty;
}

static type_id_t typecheck_node_binop(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
    type_id_t lhs = typecheck_node(tc, env, scope, node->as.binop.left, inf);
    type_id_t rhs =
        typecheck_node(tc, env, scope, node->as.binop.right, infer_with(lhs));

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
                                     scope_t* scope, node_t* node,
                                     inference_t inf) {
    type_id_t child = typecheck_node(tc, env, scope, node->as.unop.child, inf);

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
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
    type_id_t lhs = typecheck_node(tc, env, scope, node->as.logic.left, inf);
    type_id_t rhs =
        typecheck_node(tc, env, scope, node->as.logic.right, infer_with(lhs));

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
                                     scope_t* scope, node_t* node,
                                     inference_t inf) {
    type_id_t lhs = typecheck_node(tc, env, scope, node->as.comp.left, inf);
    type_id_t rhs =
        typecheck_node(tc, env, scope, node->as.comp.right, infer_with(lhs));

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
                                     scope_t* scope, node_t* node,
                                     inference_t inf) {
    node_call_t* call = &node->as.call;

    type_id_t     callee = typecheck_node(tc, env, scope, call->callee, inf);
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

    generic_instance_t* generic_instance = NULL;
    if (callee_type->as.proc.generic_count) {
        generic_instance = find_generic_instance(tc, callee);
        if (generic_instance == NULL) {
            report_error(
                tc->er, call->callee->span,
                "procedure %s is marked as generic, but was not found on the "
                "generic instances array",
                typestore_type_to_str(tc->ts, tc->tempalloc, callee_type));

            return tc->ts->primitives.err;
        }
    }

    type_id_t type_ = tc->ts->primitives.type;

    generic_bind_t* binds = da_init_generic_bind(tc->tempalloc);

    for (size_t i = 0; i < argc; ++i) {
        type_id_t expectedt = callee_type->as.proc.args[i];
        type_id_t argt = typecheck_node(tc, env, scope, call->args[i],
                                        infer_with(expectedt));

        if (type_id_eq(expectedt, type_)) {
            type_t const* ty = typestore_find_type(tc->ts, expectedt);
            munit_assert_uint8(ty->tag, ==, TYPE_TYPE);

            munit_assert_size(da_get_size(callee_type->as.proc.args), ==,
                              da_get_size(callee_type->as.proc.generic_args));

            type_id_t p = callee_type->as.proc.generic_args[i];

            binds = da_append_generic_bind(
                binds, tc->tempalloc,
                &(generic_bind_t){
                    .bound_to = eval_to_type(tc, env, scope, call->args[i]),
                    .placeholder = p});
        }

        type_t const* ty = typestore_find_type(tc->ts, expectedt);
        if (ty->tag == TYPE_PLACEHOLDER) {
            generic_bind_t* bind = find_generic_bound(binds, expectedt);
            munit_assert_not_null(bind);

            expectedt = bind->bound_to;
        }

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

    type_id_t ret = callee_type->as.proc.return_type;

    type_t const* ty = typestore_find_type(tc->ts, ret);
    if (ty->tag == TYPE_PLACEHOLDER) {
        generic_bind_t* bind = find_generic_bound(binds, ret);
        munit_assert_not_null(bind);

        ret = bind->bound_to;
    }

    return ret;
}

static type_id_t typecheck_node_ref(typechecker_t* tc, env_t* env,
                                    scope_t* scope, node_t* node,
                                    inference_t inf) {
    type_id_t child = typecheck_node(tc, env, scope, node->as.ref.child, inf);

    return typestore_add_type(
        tc->ts, &(type_t){.tag = TYPE_PTR, .as.ptr = {.inner = child}});
}

static type_id_t typecheck_node_deref(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
    type_id_t inner = typecheck_node(tc, env, scope, node->as.deref.child, inf);
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
                                     scope_t* scope, node_t* node,
                                     inference_t inf) {
    (void)inf;

    type_id_t target = eval_to_type(tc, env, scope, node->as.cast.type);
    type_id_t child =
        typecheck_node(tc, env, scope, node->as.cast.child, infer_with(target));

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
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
    type_id_t receiver =
        typecheck_node(tc, env, scope, node->as.index.receiver, inf);
    type_id_t index = typecheck_node(tc, env, scope, node->as.index.index, inf);

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
                                          scope_t* scope, node_t* node,
                                          inference_t inf) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t err = tc->ts->primitives.err;

    type_id_t expr =
        typecheck_node(tc, env, scope, node->as.stmt_expr.expr, inf);
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
                                         scope_t* scope, node_t* node,
                                         inference_t inf) {
    (void)inf;

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
        expr = typecheck_node(tc, env, scope, node->as.stmt_ret.child,
                              infer_with(env->expected_return));
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
                                           scope_t* scope, node_t* node,
                                           inference_t inf) {
    type_id_t void_ = tc->ts->primitives.void_;

    if (node->as.stmt_break.child) {
        typecheck_node(tc, env, scope, node->as.stmt_break.child, inf);
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
                                        scope_t* scope, node_t* node,
                                        inference_t inf) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t bool_ = tc->ts->primitives.bool_;

    // bool had_returned = env->has_returned;
    env->has_returned = false;

    type_id_t condition =
        typecheck_node(tc, env, scope, node->as.stmt_if.condition, inf);
    if (!type_id_eq(condition, bool_)) {
        char const* conditionstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, condition);
        report_error(
            tc->er, node->as.stmt_if.condition->span,
            "incompatible types in comparison, expected bool but got %s",
            conditionstr);
    }

    typecheck_node(tc, env, scope, node->as.stmt_if.when_true, inf);
    bool wt_returned = env->has_returned;
    bool wf_returned = false;

    env->has_returned = false;

    if (node->as.stmt_if.when_false) {
        typecheck_node(tc, env, scope, node->as.stmt_if.when_false, inf);
        wf_returned = env->has_returned;
    }

    // env->has_returned = (wt_returned && wf_returned) || had_returned;
    env->has_returned = wt_returned && wf_returned;

    return void_;
}

static type_id_t typecheck_node_stmt_while(typechecker_t* tc, env_t* env,
                                           scope_t* scope, node_t* node,
                                           inference_t inf) {
    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t bool_ = tc->ts->primitives.bool_;

    // bool had_returned = env->has_returned;
    env->has_returned = false;
    bool had_broken = env->has_broken;
    env->has_broken = false;

    type_id_t condition =
        typecheck_node(tc, env, scope, node->as.stmt_while.condition, inf);
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

    typecheck_node(tc, env, &blkscope, node->as.stmt_while.body, inf);

    // if a break was found, then any returns found inside the while are no
    // longer valid.
    if (env->has_broken) {
        env->has_returned = false;
    }

    env->has_broken = had_broken;

    return void_;
}

static type_id_t typecheck_node_stmt_blk(typechecker_t* tc, env_t* env,
                                         scope_t* scope, node_t* node,
                                         inference_t inf) {
    (void)inf;

    type_id_t void_ = tc->ts->primitives.void_;

    scope_t blkscope;
    scope_init(&blkscope, tc->tempalloc, scope, false);

    uint32_t size = da_get_size(node->as.stmt_blk.stmts);
    for (uint32_t i = 0; i < size; ++i) {
        type_id_t id = typecheck_node(
            tc, env, &blkscope, node->as.stmt_blk.stmts[i], infer_with(void_));
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
                                    scope_t* scope, node_t* node,
                                    inference_t inf) {
    (void)inf;
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

        type_id_t id = typecheck_node(tc, env, scope, it, infer_with(void_));
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

static type_id_t typecheck_node_decl(typechecker_t* tc, env_t* outer_env,
                                     scope_t* scope, node_t* node,
                                     inference_t inf) {
    (void)inf;

    type_id_t    void_ = tc->ts->primitives.void_;
    type_id_t    err = tc->ts->primitives.err;
    node_decl_t* decl = &node->as.decl;

    env_t env = *outer_env;
    env.parent = outer_env;
    env.name = decl->name;

    // FIXME: view other places that talk about extern name as for why this is
    // wrong
    char const* extern_name = decl->is_extern
                                  ? decl->extern_name
                                  : env_gen_full_name(&env, tc->alloc);

    type_id_t type_payload = INVALID_TYPEID;

    type_id_t type = INVALID_TYPEID;
    if (decl->type) type = eval_to_type(tc, &env, scope, decl->type);

    type_id_t init = INVALID_TYPEID;
    if (decl->init) {
        init = typecheck_node(tc, &env, scope, decl->init, infer_with(type));
        if (decl->is_extern) {
            report_error(tc->er, node->span,
                         "extern declarations can't have initializers");
            init = err;
        }

        if (type_id_eq(init, tc->ts->primitives.type)) {
            type_payload = eval_to_type(tc, &env, scope, decl->init);
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
                                               .extern_name = extern_name,
                                               .type_payload = type_payload});
    if (prev) {
        report_error(tc->er, node->span, "identifier %s already defined",
                     decl->name);
        if (prev->where.start != 0 && prev->where.end != 0)
            report_note(tc->er, prev->where, "declared here");
    }

    return void_;
}

static type_id_t typecheck_node_assign(typechecker_t* tc, env_t* env,
                                       scope_t* scope, node_t* node,
                                       inference_t inf) {
    node_t*   lhs_node = node->as.assign.lhs;
    type_id_t lhs = typecheck_node(tc, env, scope, lhs_node, inf);
    type_id_t rhs =
        typecheck_node(tc, env, scope, node->as.assign.rhs, infer_with(lhs));

    bool lhs_is_lvalue = false;
    switch (lhs_node->type) {
        case NODE_DEREF:
        case NODE_IDENT:
        case NODE_FIELD: lhs_is_lvalue = true; break;
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
        report_note(tc->er, node->as.assign.lhs->span, "this has type %s",
                    rhsstr);
        report_note(tc->er, node->as.assign.rhs->span, "this has type %s",
                    rhsstr);
    }

    return tc->ts->primitives.void_;
}

static uint32_t typecheck_node_proc_args(typechecker_t* tc, env_t* env,
                                         scope_t* scope, node_proc_t* proc,
                                         type_id_t** pargs,
                                         type_id_t** pgargs) {
    uint32_t generic_count = 0;

    type_id_t* args = *pargs;
    type_id_t* gargs = *pgargs;

    size_t argc = da_get_size(proc->args);
    for (size_t i = 0; i < argc; ++i) {
        node_t* arg_node = proc->args[i];
        munit_assert_uint8(arg_node->type, ==, NODE_ARG);

        arg_node->type_id = tc->ts->primitives.void_;

        node_arg_t* arg = &arg_node->as.arg;
        if (arg->type) {
            type_id_t argtype = eval_to_type(tc, env, scope, arg->type);
            type_id_t type_payload = INVALID_TYPEID;

            if (type_id_eq(argtype, tc->ts->primitives.type)) {
                // report_success(tc->er, arg_node->span,
                //                "got type type, creating new thing");

                if (generic_count == 0) {
                    // this is the first generic argument, add all previous as
                    // non-generic arguments
                    gargs = da_init_type_id(tc->alloc);

                    for (size_t j = 0; j < i; j++) {
                        gargs = da_append_type_id(gargs, tc->alloc,
                                                  &INVALID_TYPEID);
                    }
                }

                ++generic_count;
                type_payload = typestore_add_type(
                    tc->ts, &(type_t){.tag = TYPE_PLACEHOLDER});

                gargs = da_append_type_id(gargs, tc->alloc, &type_payload);
            } else if (generic_count > 0) {
                gargs = da_append_type_id(gargs, tc->alloc, &argtype);
            }

            scope_add(scope, tc->tempalloc, arg->name,
                      &(value_t){.type = argtype,
                                 .where = arg_node->span,
                                 .type_payload = type_payload});

            args = da_append_type_id(args, tc->alloc, &argtype);
        } else {
            report_error(tc->er, arg_node->span,
                         "arguments without types (using inference) "
                         "are not supported (yet)");
        }
    }

    *pargs = args;
    *pgargs = gargs;

    // if (generic_count > 0) {
    //     munit_assert_size(da_get_size(args), ==, da_get_size(gargs));
    //
    //     for (size_t i = 0; i < da_get_size(args); i++) {
    //         report_success(
    //             tc->er, (span_t){}, "got arg [%d] %s", args[i].id,
    //             typestore_type_id_to_str(tc->ts, tc->tempalloc, args[i]));
    //     }
    //
    //     for (size_t i = 0; i < da_get_size(gargs); i++) {
    //         report_success(
    //             tc->er, (span_t){}, "got garg [%d] %s", gargs[i].id,
    //             typestore_type_id_to_str(tc->ts, tc->tempalloc, gargs[i]));
    //     }
    // }

    return generic_count;
}

static void typecheck_node_proc_body(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     node_proc_t* proc, type_id_t ret,
                                     type_id_t result) {
    type_id_t void_ = tc->ts->primitives.void_;

    env_t body_env = {
        .parent = env,
        .expected_return = ret,
        .curr_proc_type = result,
        .proc_type_span =
            proc->return_type ? proc->return_type->span : node->span,
    };

    type_id_t body =
        typecheck_node(tc, &body_env, scope, proc->body, infer_with(void_));
    if (!type_id_eq(body, void_)) {
        char const* bodystr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, body);
        report_error(tc->er, node->span, "invalid type for procedure body: %s",
                     bodystr);
    }

    if (!body_env.has_returned &&
        !type_id_eq(body_env.expected_return, void_)) {
        char const* retstr = typestore_type_id_to_str(tc->ts, tc->tempalloc,
                                                      body_env.expected_return);

        report_error(tc->er, node->span,
                     "procedure with non-void return type returns "
                     "implicitly. Expected to return value of type %s",
                     retstr);

        report_note(tc->er, body_env.proc_type_span,
                    "return type declared here");
    }

    proc->uses_implicit_return = !body_env.has_returned;
}

static type_id_t typecheck_node_proc(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     inference_t               inf,
                                     typecheck_node_proc_opt_t opt) {
    (void)inf;
    (void)env;

    type_id_t void_ = tc->ts->primitives.void_;

    node_proc_t* proc = &node->as.proc;
    type_id_t*   args = da_init_type_id(tc->alloc);
    type_id_t*   gargs = NULL;

    scope_t proc_scope;
    scope_init(&proc_scope, tc->tempalloc, scope, false);

    uint32_t generic_count =
        typecheck_node_proc_args(tc, env, &proc_scope, proc, &args, &gargs);

    type_id_t ret = void_;
    if (proc->return_type) {
        ret = eval_to_type(tc, env, &proc_scope, proc->return_type);
    }

    type_t type = {
        .tag = TYPE_PROC,
        .as.proc = {.return_type = ret,
                    .args = args,
                    .generic_args = gargs,
                    .generic_count = generic_count},
    };
    type_id_t result = typestore_add_type(tc->ts, &type);

    if (generic_count) {
        define_generic_instance(tc, result, node);
    }

    if (proc->body && !generic_count) {
        typecheck_node_proc_body(tc, env, &proc_scope, node, proc, ret, result);
    }

    if (!proc->body && (opt & TC_NODE_PROC_OPT_ALLOW_NO_BODY) == 0) {
        report_error(tc->er, node->span, "procedure is missing a body");
    }

    return result;
}

static type_id_t typecheck_node_record(typechecker_t* tc, env_t* env,
                                       scope_t* scope, node_t* node,
                                       inference_t inf) {
    typecheck_node(tc, env, scope, node->as.record.blk, inf);

    return tc->ts->primitives.type;
}

static type_id_t typecheck_node_cinit(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
    node_cinit_t* cinit = &node->as.cinit;

    bool is_valid = type_id_is_valid(inf.expression_expected);
    if (!is_valid) {
        report_error(tc->er, node->span,
                     "can't infer type of compound initializer");
        return tc->ts->primitives.err;
    }

    type_t const* expression_expected =
        typestore_find_type(tc->ts, inf.expression_expected);
    if (expression_expected->tag != TYPE_RECORD) {
        report_error(tc->er, node->span,
                     "incompatible types, expected record but got %s",
                     typestore_type_id_to_str(tc->ts, tc->tempalloc,
                                              inf.expression_expected));
        return tc->ts->primitives.err;
    }

    size_t* found_fields = da_init_size(tc->tempalloc);

    size_t kids_count = da_get_size(cinit->kids);
    for (size_t i = 0; i < kids_count; i++) {
        node_t* node = cinit->kids[i];

        if (node->type != NODE_CINITF) {
            // still type check it?
            type_id_t ty = typecheck_node(tc, env, scope, node, inf);

            report_error(
                tc->er, node->span,
                "expected field initializer, but got expression of type %s",
                typestore_type_id_to_str(tc->ts, tc->tempalloc, ty));
            continue;
        }

        node_cinitf_t* cinitf = &node->as.cinitf;
        type_id_t      name = typecheck_node(tc, env, scope, cinitf->name, inf);

        type_t const* name_type = typestore_find_type(tc->ts, name);
        if (name_type->tag != TYPE_KW) {
            report_error(
                tc->er, cinitf->name->span, "expected field name, found %s",
                typestore_type_to_str(tc->ts, tc->tempalloc, name_type));
            continue;
        }

        size_t                idx = 0;
        record_field_t const* field = type_record_find_field(
            &expression_expected->as.record, name_type->as.kw.ident, &idx);
        if (field == NULL) {
            report_error(tc->er, cinitf->name->span,
                         "no field named %s in record %s",
                         name_type->as.kw.ident,
                         expression_expected->as.record.inferred_name);
            continue;
        }

        type_id_t expr =
            typecheck_node(tc, env, scope, cinitf->init, infer_with(name));
        if (!type_id_eq(field->type, expr)) {
            report_error(
                tc->er, node->span,
                "incompatible types, expected %s but got %s",
                typestore_type_id_to_str(tc->ts, tc->tempalloc, field->type),
                typestore_type_id_to_str(tc->ts, tc->tempalloc, expr));
        }

        found_fields = da_append_size(found_fields, tc->tempalloc, &idx);
    }

    size_t nfound = da_get_size(found_fields);
    size_t nexpected = da_get_size(expression_expected->as.record.fields);
    munit_assert_size(nfound, <=, nexpected);
    if (nfound < nexpected) {
        for (size_t i = 0; i < nexpected; i++) {
            bool found = false;
            for (size_t j = 0; j < nfound; j++) {
                if (found_fields[j] == i) {
                    found = true;
                    break;
                }
            }

            if (found) continue;

            report_error(tc->er, node->span, "missing field %s",
                         expression_expected->as.record.fields[i].name);
        }
    }

    return inf.expression_expected;
}

static type_id_t typecheck_node_field(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
    type_id_t receiver =
        typecheck_node(tc, env, scope, node->as.field.receiver, inf);

    type_t const* receiver_type = typestore_find_type(tc->ts, receiver);
    if (receiver_type->tag != TYPE_RECORD) {
        if (receiver_type->tag != TYPE_PTR) {
            report_error(
                tc->er, node->span, "can't access field of non-record type %s",
                typestore_type_to_str(tc->ts, tc->tempalloc, receiver_type));

            return tc->ts->primitives.err;
        }

        type_t const* inner_type =
            typestore_find_type(tc->ts, receiver_type->as.ptr.inner);
        if (inner_type->tag != TYPE_RECORD) {
            report_error(
                tc->er, node->span, "can't access field of non-record type %s",
                typestore_type_to_str(tc->ts, tc->tempalloc, receiver_type));

            return tc->ts->primitives.err;
        }

        receiver_type = inner_type;
    }

    record_field_t const* field = type_record_find_field(
        &receiver_type->as.record, node->as.field.field, NULL);
    if (field == NULL) {
        report_error(tc->er, node->span, "no field named %s in record %s",
                     node->as.field.field,
                     receiver_type->as.record.inferred_name);

        return tc->ts->primitives.err;
    }

    return field->type;
}

static type_id_t typecheck_node_array(typechecker_t* tc, env_t* env,
                                      scope_t* scope, node_t* node,
                                      inference_t inf) {
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
            typecheck_node(tc, env, scope, array->initializer_list[i], inf);
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

static type_id_t typecheck_node_ptr(typechecker_t* tc, env_t* env,
                                    scope_t* scope, node_t* node,
                                    inference_t inf) {
    (void)inf;

    type_id_t type = tc->ts->primitives.type;
    type_id_t ty =
        typecheck_node(tc, env, scope, node->as.ptr.child, infer_with(type));
    if (!type_id_eq(ty, type)) {
        char const* exprstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, ty);
        report_error(tc->er, node->span, "expected type for pointer, found %s",
                     exprstr);
        report_note(tc->er, node->as.ptr.child->span, "expression has type %s",
                    exprstr);
    }

    return type;
}

static type_id_t typecheck_node_mptr(typechecker_t* tc, env_t* env,
                                     scope_t* scope, node_t* node,
                                     inference_t inf) {
    (void)inf;

    if (node->as.mptr.term) {
        report_error(
            tc->er, node->span,
            "multi-pointers with terminators have not been implemented");
    }

    type_id_t type = tc->ts->primitives.type;
    type_id_t ty =
        typecheck_node(tc, env, scope, node->as.mptr.child, infer_with(type));
    if (!type_id_eq(ty, type)) {
        char const* exprstr =
            typestore_type_id_to_str(tc->ts, tc->tempalloc, ty);
        report_error(tc->er, node->span,
                     "expected type for multi-pointer, found %s", exprstr);
        report_note(tc->er, node->as.mptr.child->span, "expression has type %s",
                    exprstr);
    }

    return type;
}

static type_id_t typecheck_node(typechecker_t* tc, env_t* env, scope_t* scope,
                                node_t* node, inference_t inf) {
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
        case NODE_STR: result = tc->ts->primitives.str; break;
        case NODE_IDENT:
            result = typecheck_node_ident(tc, env, scope, node);
            break;
        case NODE_KW: result = typecheck_node_kw(tc, env, scope, node); break;
        case NODE_BINOP:
            result = typecheck_node_binop(tc, env, scope, node, inf);
            break;
        case NODE_UNOP:
            result = typecheck_node_unop(tc, env, scope, node, inf);
            break;
        case NODE_LOGIC:
            result = typecheck_node_logic(tc, env, scope, node, inf);
            break;
        case NODE_COMP:
            result = typecheck_node_comp(tc, env, scope, node, inf);
            break;
        case NODE_CALL:
            result = typecheck_node_call(tc, env, scope, node, inf);
            break;
        case NODE_REF:
            result = typecheck_node_ref(tc, env, scope, node, inf);
            break;
        case NODE_DEREF:
            result = typecheck_node_deref(tc, env, scope, node, inf);
            break;
        case NODE_CAST:
            result = typecheck_node_cast(tc, env, scope, node, inf);
            break;
        case NODE_INDEX:
            result = typecheck_node_index(tc, env, scope, node, inf);
            break;
        case NODE_STMT_EXPR:
            result = typecheck_node_stmt_expr(tc, env, scope, node, inf);
            break;
        case NODE_STMT_RET:
            result = typecheck_node_stmt_ret(tc, env, scope, node, inf);
            break;
        case NODE_STMT_BREAK:
            result = typecheck_node_stmt_break(tc, env, scope, node, inf);
            break;
        case NODE_STMT_IF:
            result = typecheck_node_stmt_if(tc, env, scope, node, inf);
            break;
        case NODE_STMT_WHILE:
            result = typecheck_node_stmt_while(tc, env, scope, node, inf);
            break;
        case NODE_STMT_BLK:
            result = typecheck_node_stmt_blk(tc, env, scope, node, inf);
            break;
        case NODE_MOD:
            result = typecheck_node_mod(tc, env, scope, node, inf);
            break;
        case NODE_DECL:
            result = typecheck_node_decl(tc, env, scope, node, inf);
            break;
        case NODE_ASSIGN:
            result = typecheck_node_assign(tc, env, scope, node, inf);
            break;
        case NODE_ARG: break;
        case NODE_PROC: {
            typecheck_node_proc_opt_t opts = 0;
            if (env->in_type_context) opts |= TC_NODE_PROC_OPT_ALLOW_NO_BODY;

            result = typecheck_node_proc(tc, env, scope, node, inf, opts);
        } break;
        case NODE_RECORD:
            result = typecheck_node_record(tc, env, scope, node, inf);
            break;
        case NODE_CINITF: break;
        case NODE_CINIT:
            result = typecheck_node_cinit(tc, env, scope, node, inf);
            break;
        case NODE_FIELD:
            result = typecheck_node_field(tc, env, scope, node, inf);
            break;
        case NODE_ARRAY:
            result = typecheck_node_array(tc, env, scope, node, inf);
            break;
        case NODE_PTR:
            result = typecheck_node_ptr(tc, env, scope, node, inf);
            break;
        case NODE_MPTR:
            result = typecheck_node_mptr(tc, env, scope, node, inf);
            break;
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

    typechecker_t tc = {
        .er = params->er,
        .ts = params->ts,
        .tempalloc = temp_alloc,
        .alloc = params->alloc,
        .generic_instances = da_init_generic_instance(temp_alloc),
    };

    munit_assert_not_null(params->module_name);
    env_t root_env = {.name = params->module_name};

    // FIXME: Check if temp_alloc is what we want for this
    scope_t root_scope = {};
    scope_init(&root_scope, tc.tempalloc, NULL, false);
    scope_add(&root_scope, tc.tempalloc, "true",
              &(value_t){.type = tc.ts->primitives.bool_});
    scope_add(&root_scope, tc.tempalloc, "false",
              &(value_t){.type = tc.ts->primitives.bool_});

    {
        type_id_t type_ = tc.ts->primitives.type;
        scope_add(
            &root_scope, tc.tempalloc, "i32",
            &(value_t){.type = type_, .type_payload = tc.ts->primitives.i32});
        scope_add(
            &root_scope, tc.tempalloc, "i8",
            &(value_t){.type = type_, .type_payload = tc.ts->primitives.i8});
        scope_add(
            &root_scope, tc.tempalloc, "f32",
            &(value_t){.type = type_, .type_payload = tc.ts->primitives.f32});
        scope_add(
            &root_scope, tc.tempalloc, "f64",
            &(value_t){.type = type_, .type_payload = tc.ts->primitives.f64});
        scope_add(
            &root_scope, tc.tempalloc, "bool",
            &(value_t){.type = type_, .type_payload = tc.ts->primitives.bool_});
        scope_add(
            &root_scope, tc.tempalloc, "void",
            &(value_t){.type = type_, .type_payload = tc.ts->primitives.void_});
        scope_add(&root_scope, tc.tempalloc, "type",
                  &(value_t){.type = type_, .type_payload = type_});
    }

    scope_t scope;
    scope_init(&scope, tc.tempalloc, &root_scope, false);
    typecheck_node(&tc, &root_env, &scope, params->ast,
                   infer_with(tc.ts->primitives.err));

    arena_free(&temp_arena);
}
