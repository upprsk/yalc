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
} typechecker_t;

typedef struct env {
    type_id_t expected_return;
    type_id_t curr_proc_type;

    // TODO: Allow more than one return per procedure
    bool has_returned;
} env_t;

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

static type_id_t typecheck_node(typechecker_t* tc, env_t* env, node_t* node) {
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
        } break;
        case NODE_BINOP: {
        } break;
        case NODE_UNOP: {
        } break;
        case NODE_STMT_EXPR: {
        } break;
        case NODE_STMT_RET: {
            if (!type_id_is_valid(env->curr_proc_type)) {
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "can't return outside of procedure body");
            }

            if (env->has_returned) {
                // TODO: Allow for more than one return
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "each function can only return once");
            }

            type_id_t expr = typecheck_node(tc, env, node->as.stmt_ret.child);
            if (!type_id_eq(env->expected_return, expr)) {
                // TODO: Report type name instead of id
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "incompatible types %d and %d in return",
                             env->expected_return.id, expr.id);
            }

            env->has_returned = true;
            result = void_;
        } break;
        case NODE_STMT_BLK: {
            uint32_t size = da_get_size(node->as.stmt_blk.stmts);
            for (uint32_t i = 0; i < size; ++i) {
                type_id_t id =
                    typecheck_node(tc, env, node->as.stmt_blk.stmts[i]);
                if (!type_id_eq(id, void_)) {
                    // TODO: Report type name instead of id
                    report_error(tc->er, tc->filename, tc->source, node->span,
                                 "value of expression was unused: %d", id.id);
                }
            }

            result = void_;
        } break;
        case NODE_MOD: {
            uint32_t size = da_get_size(node->as.mod.decls);
            for (uint32_t i = 0; i < size; ++i) {
                type_id_t id = typecheck_node(tc, env, node->as.mod.decls[i]);
                if (!type_id_eq(id, void_)) {
                    // TODO: Report type name instead of id
                    report_error(tc->er, tc->filename, tc->source, node->span,
                                 "invalid type for top-level declaration, "
                                 "expected void, got %d",
                                 id.id);
                }
            }

            result = void_;
        } break;
        case NODE_DECL: {
            node_decl_t* decl = &node->as.decl;

            type_id_t init = typecheck_node(tc, env, decl->init);
            if (decl->type) {
                type_id_t type = eval_to_type(tc, decl->type);
                if (!type_id_eq(type, init)) {
                    // TODO: Report type name instead of id
                    report_error(
                        tc->er, tc->filename, tc->source, node->span,
                        "incompatible types %d and %d in declaration of %s",
                        type.id, init.id, decl->name);
                }
            }

            // TODO: Add to symbol table

            result = void_;
        } break;
        case NODE_ASSIGN: {
        } break;
        case NODE_ARG: {
        } break;
        case NODE_PROC: {
            node_proc_t* proc = &node->as.proc;

            type_id_t* args = NULL;

            type_id_t ret = void_;
            if (proc->return_type) {
                ret = eval_to_type(tc, proc->return_type);
            }

            if (da_get_size(proc->args) > 0) {
                report_error(
                    tc->er, tc->filename, tc->source, node->span,
                    "no support for procedure arguments was implemented");
            }

            result = typestore_add_type(
                tc->ts, &(type_t){
                            .tag = TYPE_PROC,
                            .as.proc = {.return_type = ret, .args = args}
            });

            env_t body_env = {.expected_return = ret, .curr_proc_type = result};
            type_id_t body = typecheck_node(tc, &body_env, proc->body);
            if (!type_id_eq(body, void_)) {
                // TODO: Report type name instead of id
                report_error(
                    tc->er, tc->filename, tc->source, node->span,
                    "invalid type for procedure body, expected void, got %d",
                    body.id);
            }

            if (!body_env.has_returned &&
                !type_id_eq(body_env.expected_return, void_)) {
                // TODO: Report type name instead of id
                report_error(tc->er, tc->filename, tc->source, node->span,
                             "procedure with non-void return type returns "
                             "implicitly. Expected to return type %d",
                             body_env.expected_return);
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
    allocator_t alloc;
    Arena       arena = {0};
    allocator_init_arena(&alloc, &arena);

    typename_table_t tnt = {
        .entries = da_init(typename_table_entry_t, alloc),
    };

    tnt.entries =
        da_append(tnt.entries, alloc,
                  &(typename_table_entry_t){
                      .name = "i32", .type = params->ts->primitives.i32});
    tnt.entries =
        da_append(tnt.entries, alloc,
                  &(typename_table_entry_t){
                      .name = "void", .type = params->ts->primitives.void_});

    typechecker_t tc = {
        .filename = params->filename,
        .source = params->source,
        .source_len = params->source_len,
        .er = params->er,
        .ts = params->ts,
        .tnt = &tnt,
    };

    env_t root_env = {};

    typecheck_node(&tc, &root_env, params->ast);
}
