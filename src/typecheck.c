#include "typecheck.h"

#include <stdint.h>
#include <string.h>

#include "allocator.h"
#include "ast.h"
#include "da.h"
#include "errors.h"
#include "span.h"
#include "typestore.h"

typedef struct typename_table_entry {
    char const* name;
    type_id_t   type;
} typename_table_entry_t;

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

static type_id_t eval_to_type(typechecker_t* tc, node_t* node) {
    if (node->type != NODE_IDENT) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "expression could not be evaluated to a type");

        node->type_id = tc->ts->primitives.err;
        return INVALID_TYPEID;
    }

    char const* name = node->as.ident.ident;
    type_id_t   type = typename_table_find(tc->tnt, name);
    if (type_id_eq(type, INVALID_TYPEID)) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "undefined type: %s", name);
        node->type_id = tc->ts->primitives.err;
        return INVALID_TYPEID;
    }

    node->type_id = tc->ts->primitives.type;
    return type;
}

static type_id_t typecheck_node(typechecker_t* tc, env_t* env, scope_t* scope,
                                node_t* node) {
    munit_assert_not_null(node);

    type_id_t void_ = tc->ts->primitives.void_;
    type_id_t result = INVALID_TYPEID;

    switch (node->type) {
        case NODE_ERR: {
            report_error(tc->er, tc->filename, tc->source, node->span,
                         "found error node in typecheck");
            result = tc->ts->primitives.err;
        } break;
        case NODE_INT: {
            result = tc->ts->primitives.i32;
        } break;
        case NODE_FLOAT: {
        } break;
        case NODE_IDENT: {
            value_t const* v = scope_find(scope, node->as.ident.ident);
            if (!v) {
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "undeclared identifier %s", node->as.ident.ident);
                result = tc->ts->primitives.err;
                break;
            }

            result = v->type;
        } break;
        case NODE_BINOP: {
            type_id_t lhs = typecheck_node(tc, env, scope, node->as.binop.left);
            type_id_t rhs =
                typecheck_node(tc, env, scope, node->as.binop.right);

            if (!type_id_eq(lhs, rhs)) {
                char const* lhsstr =
                    typestore_type_id_to_str(tc->ts, tc->temp_alloc, lhs);
                char const* rhsstr =
                    typestore_type_id_to_str(tc->ts, tc->temp_alloc, rhs);

                report_error(tc->er, tc->filename, tc->source, node->span,
                             "incompatible types in %s, expected %s but got %s",
                             binop_to_str(node->as.binop.type), lhsstr, rhsstr);
                report_note(tc->er, tc->filename, tc->source,
                            node->as.binop.left->span, "this has type %s",
                            lhsstr);
                report_note(tc->er, tc->filename, tc->source,
                            node->as.binop.right->span, "this has type %s",
                            rhsstr);
            }

            result = lhs;
        } break;
        case NODE_UNOP: {
        } break;
        case NODE_STMT_EXPR: {
            type_id_t expr =
                typecheck_node(tc, env, scope, node->as.stmt_expr.expr);
            if (!type_id_eq(expr, void_)) {
                char const* exprstr =
                    typestore_type_id_to_str(tc->ts, tc->temp_alloc, expr);

                report_error(tc->er, tc->filename, tc->source, node->span,
                             "expression result unused");
                report_note(tc->er, tc->filename, tc->source,
                            node->as.stmt_expr.expr->span,
                            "this expression has type %s", exprstr);
            }

            result = void_;
        } break;
        case NODE_STMT_RET: {
            if (!type_id_is_valid(env->curr_proc_type)) {
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "can't return outside of procedure body");
                break;
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
                char const* retstr = typestore_type_id_to_str(
                    tc->ts, tc->temp_alloc, env->expected_return);
                char const* exprstr =
                    typestore_type_id_to_str(tc->ts, tc->temp_alloc, expr);

                report_error(
                    tc->er, tc->filename, tc->source, node->span,
                    "incompatible types in return, expected %s but got %s",
                    retstr, exprstr);
                report_note(tc->er, tc->filename, tc->source,
                            env->proc_type_span, "return type declared here");
            }

            env->has_returned = true;
            result = void_;
        } break;
        case NODE_STMT_BLK: {
            scope_t blkscope;
            scope_init(&blkscope, tc->temp_alloc, scope);

            uint32_t size = da_get_size(node->as.stmt_blk.stmts);
            for (uint32_t i = 0; i < size; ++i) {
                type_id_t id = typecheck_node(tc, env, &blkscope,
                                              node->as.stmt_blk.stmts[i]);
                if (!type_id_eq(id, void_)) {
                    char const* typestr =
                        typestore_type_id_to_str(tc->ts, tc->temp_alloc, id);
                    report_error(tc->er, tc->filename, tc->source, node->span,
                                 "invalid type for block scope declaration: %s",
                                 typestr);
                }
            }

            result = void_;
        } break;
        case NODE_MOD: {
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
                    report_error(
                        tc->er, tc->filename, tc->source, node->span,
                        "invalid type for module scope declaration: %s",
                        typestr);
                }
            }

            result = void_;
        } break;
        case NODE_DECL: {
            node_decl_t* decl = &node->as.decl;

            type_id_t type = INVALID_TYPEID;
            if (decl->type) type = eval_to_type(tc, decl->type);

            type_id_t init = typecheck_node(tc, env, scope, decl->init);
            if (type_id_is_valid(type) && !type_id_eq(type, init)) {
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

            type = init;

            // FIXME: Check if using temp_alloc here is what we want
            value_t const* prev =
                scope_add(scope, tc->temp_alloc, decl->name,
                          &(value_t){.type = type, .where = decl->name_span});
            if (prev) {
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "duplicate identifier %s", decl->name);
                report_note(tc->er, tc->filename, tc->source, prev->where,
                            "declared here");
            }

            result = void_;
        } break;
        case NODE_ASSIGN: {
        } break;
        case NODE_ARG: {
        } break;
        case NODE_PROC: {
            node_proc_t* proc = &node->as.proc;
            type_id_t*   args = NULL;

            if (da_get_size(proc->args) > 0) {
                report_error(
                    tc->er, tc->filename, tc->source, node->span,
                    "no support for procedure arguments was implemented");
            }

            // NOTE: process args before return type

            type_id_t ret = void_;
            if (proc->return_type) {
                ret = eval_to_type(tc, proc->return_type);
            }

            result = typestore_add_type(
                tc->ts, &(type_t){
                            .tag = TYPE_PROC,
                            .as.proc = {.return_type = ret, .args = args}
            });

            env_t     body_env = {.expected_return = ret,
                                  .curr_proc_type = result,
                                  .proc_type_span = proc->return_type
                                                        ? proc->return_type->span
                                                        : node->span};
            type_id_t body = typecheck_node(tc, &body_env, scope, proc->body);
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
                            body_env.proc_type_span,
                            "return type declared here");
            }
        }
        case NODE_PTR: {
        } break;
        case NODE_MPTR: {
        } break;
    }

    node->type_id = result;

    if (type_id_eq(result, INVALID_TYPEID)) {
        report_error(tc->er, tc->filename, tc->source, node->span,
                     "unimplemented node type: '%s'",
                     node_type_to_str(node->type));

        munit_assert(false);
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

    typecheck_node(&tc, &root_env, &root_scope, params->ast);

    arena_free(&temp_arena);
}
