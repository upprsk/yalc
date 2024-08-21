#include "depg.h"

#include <stdio.h>

#include "allocator.h"
#include "ast.h"
#include "common.h"
#include "da.h"
#include "errors.h"
#include "span.h"

typedef struct scope_item {
    char const* key;
    node_t*     node;
    bool        is_top;
} scope_item_t;

da_declare(scope_item_t, scope_item);

typedef struct scope {
    struct scope* parent;
    scope_item_t* items;

    bool is_top;
} scope_t;

static scope_item_t* scope_find_here(scope_t const* s, char const* key) {
    munit_assert_not_null(s);

    size_t size = da_get_size(s->items);
    for (size_t i = 0; i < size; i++) {
        if (streq(s->items[i].key, key)) return &s->items[i];
    }

    return NULL;
}

static scope_item_t* scope_find(scope_t const* s, char const* key) {
    if (!s) return NULL;

    scope_item_t* item = scope_find_here(s, key);
    if (!item) return scope_find(s->parent, key);

    return item;
}

static bool scope_add_item(scope_t* s, allocator_t alloc, char const* key,
                           node_t* node, bool is_top) {
    munit_assert_not_null(s);
    if (scope_find(s, key)) return false;

    s->items = da_append_scope_item(
        s->items, alloc,
        &(scope_item_t){.key = key, .node = node, .is_top = is_top});

    return true;
}

typedef enum graph_node_mark : uint8_t {
    GNMARK_NONE,
    GNMARK_PERM,
    GNMARK_TEMP,
} graph_node_mark_t;

typedef struct graph_node {
    node_t*             node;
    struct graph_node** dependencies;
    graph_node_mark_t   mark;
} graph_node_t;

da_declare(graph_node_t, graph_node);
da_declare(graph_node_t*, graph_node_ptr);

typedef struct state {
    error_reporter_t* er;
    allocator_t       tempalloc;
    scope_t*          curr_scope;
    node_t*           curr_decl;
    graph_node_t*     graph;
} state_t;

static graph_node_t* find_node_in_graph(state_t* s, node_t* node) {
    size_t count = da_get_size(s->graph);
    for (size_t i = 0; i < count; i++) {
        if (s->graph[i].node == node) return &s->graph[i];
    }

    return NULL;
}

static graph_node_t* add_node_to_graph(state_t* s, node_t* node) {
    s->graph = da_append_graph_node(
        s->graph, s->tempalloc,
        &(graph_node_t){.node = node,
                        .dependencies = da_init_graph_node_ptr(s->tempalloc)});

    return &s->graph[da_get_size(s->graph) - 1];
}

static graph_node_t* find_node_in_graph_or_add(state_t* s, node_t* node) {
    graph_node_t* g = find_node_in_graph(s, node);
    if (!g) g = add_node_to_graph(s, node);

    munit_assert_not_null(g);
    return g;
}

static graph_node_t* find_dependency(graph_node_t* gn, graph_node_t* node) {
    size_t count = da_get_size(gn->dependencies);
    for (size_t i = 0; i < count; i++) {
        if (gn->dependencies[i] == node) return gn->dependencies[i];
    }

    return NULL;
}

static void add_dependency(graph_node_t* gn, allocator_t alloc,
                           graph_node_t* node) {
    if (find_dependency(gn, node)) return;

    gn->dependencies = da_append_graph_node_ptr(gn->dependencies, alloc, &node);
}

static void declare(state_t* s, char const* key, node_t* node) {
    munit_assert_not_null(s);
    munit_assert_not_null(node);
    munit_assert_not_null(s->curr_scope);
    munit_assert(node->type == NODE_DECL || node->type == NODE_ARG);

    // fprintf(stderr, "declaring %p (%s) (%s)\n", node, key,
    //         s->curr_scope->is_top ? "top" : "untracked");

    scope_add_item(s->curr_scope, s->tempalloc, key, node,
                   s->curr_scope->is_top);
}

static scope_item_t* find_declared(state_t* s, char const* key) {
    return scope_find(s->curr_scope, key);
}

static void mark_depends(state_t* s, scope_item_t const* item) {
    munit_assert_not_null(item);
    munit_assert_not_null(item->node);
    munit_assert_not_null(s->curr_decl);
    munit_assert_uint8(s->curr_decl->type, ==, NODE_DECL);

    // fprintf(stderr, "marking %p (%s) depends: %s %p (%s)\n", s->curr_decl,
    //         s->curr_decl->as.decl.name, item->key, item->node,
    //         item->is_top ? "top" : "untracked");

    if (!item->is_top) return;

    munit_assert_uint8(s->curr_decl->type, ==, NODE_DECL);
    munit_assert_uint8(item->node->type, ==, NODE_DECL);

    // report_note(s->er, s->filename, s->source, s->curr_decl->span,
    //             "this depends on");
    // report_note(s->er, s->filename, s->source, item->node->span, "this");

    graph_node_t* g = find_node_in_graph_or_add(s, s->curr_decl);
    graph_node_t* dep = find_node_in_graph_or_add(s, item->node);

    add_dependency(g, s->tempalloc, dep);
}

static void check_node(state_t* s, node_t* node) {
    if (!node) return;

    switch (node->type) {
        case NODE_ERR:         // nop
        case NODE_INT:         // nop
        case NODE_FLOAT:       // nop
        case NODE_STR: break;  // nop
        case NODE_BINOP: {
            check_node(s, node->as.binop.left);
            check_node(s, node->as.binop.right);
        } break;
        case NODE_UNOP: {
            check_node(s, node->as.unop.child);
        } break;
        case NODE_LOGIC: {
            check_node(s, node->as.logic.left);
            check_node(s, node->as.logic.right);
        } break;
        case NODE_COMP: {
            check_node(s, node->as.comp.left);
            check_node(s, node->as.comp.right);
        } break;
        case NODE_INDEX: {
            check_node(s, node->as.index.receiver);
            check_node(s, node->as.index.index);
        } break;
        case NODE_CALL: {
            check_node(s, node->as.call.callee);

            size_t count = da_get_size(node->as.call.args);
            for (size_t i = 0; i < count; i++) {
                check_node(s, node->as.call.args[i]);
            }
        } break;
        case NODE_REF: {
            check_node(s, node->as.ref.child);
        } break;
        case NODE_DEREF: {
            check_node(s, node->as.deref.child);
        } break;
        case NODE_CAST: {
            check_node(s, node->as.cast.child);
            check_node(s, node->as.cast.type);
        } break;
        case NODE_STMT_EXPR: {
            check_node(s, node->as.stmt_expr.expr);
        } break;
        case NODE_STMT_RET: {
            check_node(s, node->as.stmt_ret.child);
        } break;
        case NODE_STMT_IF: {
            check_node(s, node->as.stmt_if.condition);
            check_node(s, node->as.stmt_if.when_true);
            check_node(s, node->as.stmt_if.when_false);
        } break;
        case NODE_STMT_WHILE: {
            check_node(s, node->as.stmt_while.condition);
            check_node(s, node->as.stmt_while.body);
        } break;
        case NODE_STMT_BLK: {
            scope_t mod_scope = {.parent = s->curr_scope,
                                 .items = da_init_scope_item(s->tempalloc)};
            s->curr_scope = &mod_scope;

            size_t count = da_get_size(node->as.stmt_blk.stmts);
            for (size_t i = 0; i < count; i++) {
                check_node(s, node->as.stmt_blk.stmts[i]);
            }

            s->curr_scope = mod_scope.parent;
        } break;
        case NODE_MOD: {
            scope_t mod_scope = {.parent = s->curr_scope,
                                 .items = da_init_scope_item(s->tempalloc),
                                 .is_top = true};
            s->curr_scope = &mod_scope;

            size_t count = da_get_size(node->as.mod.decls);
            for (size_t i = 0; i < count; i++) {
                node_t* decl_node = node->as.mod.decls[i];

                munit_assert_uint8(decl_node->type, ==, NODE_DECL);
                declare(s, decl_node->as.decl.name, decl_node);
            }

            for (size_t i = 0; i < count; i++) {
                check_node(s, node->as.mod.decls[i]);
            }

            s->curr_scope = mod_scope.parent;
        } break;
        case NODE_DECL: {
            node_t* prev_decl = s->curr_decl;

            // do not change the reference decl in case we are not in the
            // top-level.
            if (s->curr_scope->is_top) s->curr_decl = node;

            declare(s, node->as.decl.name, node);

            check_node(s, node->as.decl.type);
            check_node(s, node->as.decl.init);

            s->curr_decl = prev_decl;
        } break;
        case NODE_ASSIGN: {
            check_node(s, node->as.assign.lhs);
            check_node(s, node->as.assign.rhs);
        } break;
        case NODE_ARG: {
            check_node(s, node->as.arg.type);
            declare(s, node->as.arg.name, node);
        } break;
        case NODE_PROC: {
            scope_t proc_scope = {.parent = s->curr_scope,
                                  .items = da_init_scope_item(s->tempalloc)};
            s->curr_scope = &proc_scope;

            size_t count = da_get_size(node->as.proc.args);
            for (size_t i = 0; i < count; i++) {
                check_node(s, node->as.proc.args[i]);
            }

            check_node(s, node->as.proc.return_type);
            check_node(s, node->as.proc.body);

            s->curr_scope = proc_scope.parent;
        } break;
        case NODE_ARRAY: {
            check_node(s, node->as.array.len);
            check_node(s, node->as.array.type);

            size_t count = da_get_size(node->as.array.initializer_list);
            for (size_t i = 0; i < count; i++) {
                check_node(s, node->as.array.initializer_list[i]);
            }
        } break;
        case NODE_PTR: {
            check_node(s, node->as.ptr.child);
        } break;
        case NODE_MPTR: {
            check_node(s, node->as.mptr.child);
            check_node(s, node->as.mptr.term);
        } break;
        case NODE_IDENT: {
            scope_item_t* item = find_declared(s, node->as.ident.ident);
            if (item) mark_depends(s, item);

            // else {
            //     report_error(s->er, s->filename, s->source, node->span,
            //                  "identifier is undefined");
            // }
        } break;
    }
}

static node_t** sort(graph_node_t* gn, allocator_t alloc, error_reporter_t* er);

static node_t** append_missing_decls(node_t** nodes, allocator_t alloc,
                                     node_t* ast) {
    munit_assert_uint8(ast->type, ==, NODE_MOD);

    size_t modcount = da_get_size(ast->as.mod.decls);
    size_t count = da_get_size(nodes);
    munit_assert_size(modcount, >=, count);

    // in case they are the same size, all declarations have been used
    if (modcount == count) return nodes;

    // find all declarations not in the node graph
    for (size_t i = 0; i < modcount; i++) {
        node_t* decl = ast->as.mod.decls[i];
        munit_assert_uint8(decl->type, ==, NODE_DECL);

        bool found = false;
        for (size_t i = 0; i < count; i++) {
            if (decl == nodes[i]) {
                // is present
                found = true;
                break;
            }
        }

        // not present in the graph, append to the end
        if (!found) nodes = da_append_node(nodes, alloc, &decl);
    }

    munit_assert_size(modcount, ==, da_get_size(nodes));

    return nodes;
}

node_t** pass_depgraph(depg_params_t const* params) {
    Arena       temp_arena = {};
    allocator_t tempalloc = {};
    allocator_init_arena(&tempalloc, &temp_arena);

    scope_t root_scope = {.items = da_init_scope_item(tempalloc)};
    // scope_add_item(&root_scope, tempalloc, "i32", NULL);
    // scope_add_item(&root_scope, tempalloc, "void", NULL);

    state_t s = {
        .tempalloc = tempalloc,
        .curr_scope = &root_scope,
        .er = params->er,
        .graph = da_init_graph_node(tempalloc),
    };

    check_node(&s, params->ast);
    arena_free(&temp_arena);

    node_t** nodes = sort(s.graph, params->alloc, s.er);
    return append_missing_decls(nodes, params->alloc, params->ast);
}

typedef struct sort_state {
    bool     found_cycle;
    node_t** l;

    error_reporter_t* er;
} sort_state_t;

static void add_to_list(sort_state_t* s, allocator_t alloc, node_t* node) {
    // get rid of duplicates
    size_t count = da_get_size(s->l);
    for (size_t i = 0; i < count; i++) {
        if (s->l[i] == node) return;
    }

    s->l = da_append_node(s->l, alloc, &node);
}

static void sort_visit(sort_state_t* s, allocator_t alloc, graph_node_t* n) {
    if (n->mark == GNMARK_PERM) return;
    if (n->mark == GNMARK_TEMP) {
        // we got cycle
        s->found_cycle = true;

        report_error(s->er, n->node->span, "found dependency cycle");
        return;
    }

    n->mark = GNMARK_TEMP;

    size_t count = da_get_size(n->dependencies);
    for (size_t i = 0; i < count; i++) {
        sort_visit(s, alloc, n->dependencies[i]);
    }

    n->mark = GNMARK_PERM;
    add_to_list(s, alloc, n->node);
}

static node_t** sort(graph_node_t* gn, allocator_t alloc,
                     error_reporter_t* er) {
    sort_state_t s = {.l = da_init_node(alloc), .er = er};

    while (!s.found_cycle) {
        graph_node_t node = {};
        if (!da_pop_graph_node(gn, &node)) break;

        sort_visit(&s, alloc, &node);

        if (node.mark != GNMARK_PERM)
            gn = da_append_graph_node(gn, alloc, &node);
    }

    if (da_get_size(gn) > 0) {
        // go a cycle
        da_free(s.l, alloc);

        return NULL;
    }

    return s.l;
}
