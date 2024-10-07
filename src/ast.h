#pragma once

#include <stdint.h>

#include "alloc/allocator.h"
#include "da/da.h"
#include "munit.h"
#include "slice/slice.h"

typedef struct node_ref {
    uint32_t id;
} node_ref_t;

#define node_ref_valid(_ref) (!!(_ref).id)

typedef struct node_arr {
    uint32_t start;
    uint32_t len;
} node_arr_t;

typedef enum node_kind : uint8_t {
    NODE_INVAL,

    NODE_MOD,   // w_children
    NODE_DECL,  // decl
    NODE_PROC,  // proc
    NODE_BLK,   // w_children

    NODE_ADD,  // binary
    NODE_SUB,  // binary
    NODE_RET,  // w_child

    NODE_IDENT,  // ident
    NODE_INT,    // int
} node_kind_t;

static inline char const* node_kind_str(node_kind_t nk) {
    switch (nk) {
        case NODE_INVAL: return "NODE_INVAL";
        case NODE_MOD: return "NODE_MOD";
        case NODE_DECL: return "NODE_DECL";
        case NODE_PROC: return "NODE_PROC";
        case NODE_BLK: return "NODE_BLK";
        case NODE_ADD: return "NODE_ADD";
        case NODE_SUB: return "NODE_SUB";
        case NODE_RET: return "NODE_RET";
        case NODE_IDENT: return "NODE_IDENT";
        case NODE_INT: return "NODE_INT";
    }

    return "?";
}

typedef struct node_w_child {
    node_ref_t child;
} node_w_child_t;

typedef struct node_w_children {
    node_arr_t children;
} node_w_children_t;

typedef struct node_binary {
    node_ref_t left;
    node_ref_t right;
} node_binary_t;

typedef struct node_decl {
    str_t      name;
    node_ref_t type;
    node_ref_t init;
} node_decl_t;

typedef struct node_proc {
    node_arr_t args;
    node_ref_t ret;
    node_ref_t body;
} node_proc_t;

typedef struct node_ident {
    str_t ident;
} node_ident_t;

typedef struct node_int {
    uint64_t value;
} node_int_t;

typedef struct node {
    node_kind_t kind;
    // we have 7 bytes free here
    union {
        node_w_child_t    w_child;
        node_w_children_t w_children;
        node_binary_t     binary;

        node_decl_t decl;
        node_proc_t proc;

        node_ident_t ident;
        node_int_t   int_;
    } as;
} node_t;

typedef da_t(node_t) da_node_t;
typedef da_t(node_ref_t) da_node_ref_t;
typedef slice_t(node_ref_t) slice_node_ref_t;

typedef struct ast {
    da_node_t     nodes;
    da_node_ref_t refs;
    allocator_t   alloc;
} ast_t;

static inline void ast_init(ast_t* a, allocator_t alloc) {
    assert_not_null(a);

    *a = (ast_t){
        .nodes = da_init(alloc), .refs = da_init(alloc), .alloc = alloc};

    // push a dummy node to occupy index zero
    da_push_back(&a->nodes, (node_t){});
}

static inline node_ref_t ast_alloc_node(ast_t* a, node_t** node) {
    assert_not_null(a);

    uint32_t idx = a->nodes.size;
    da_push_back(&a->nodes, (node_t){});

    if (node) *node = &a->nodes.items[idx];

    return (node_ref_t){idx};
}

static inline node_t* ast_get(ast_t const* a, node_ref_t ref) {
    assert_uint32(ref.id, >, 0);

    return &a->nodes.items[ref.id];
}

static inline slice_node_ref_t ast_get_arr(ast_t const* a, node_arr_t arr) {
    slice_node_ref_t s = da_to_slice(a->refs);
    return slice_s(s, arr.start, arr.start + arr.len);
}

static inline node_arr_t ast_add_refs(ast_t* a, da_node_ref_t refs) {
    uint32_t idx = a->refs.size;
    da_append(&a->refs, refs.size, refs.items);

    return (node_arr_t){idx, refs.size};
}

string_t ast_dump(ast_t const* a, node_ref_t node, allocator_t alloc);

static inline void node_init_mod(node_t* n, node_arr_t children) {
    *n = (node_t){.kind = NODE_MOD, .as.w_children = {.children = children}};
}

static inline void node_init_decl(node_t* n, str_t name, node_ref_t type,
                                  node_ref_t init) {
    *n = (node_t){
        .kind = NODE_DECL,
        .as.decl = {.name = name, .type = type, .init = init}
    };
}

static inline void node_init_proc(node_t* n, node_arr_t args, node_ref_t ret,
                                  node_ref_t body) {
    *n = (node_t){
        .kind = NODE_PROC, .as.proc = {.args = args, .ret = ret, .body = body}
    };
}

static inline void node_init_blk(node_t* n, node_arr_t children) {
    *n = (node_t){.kind = NODE_BLK, .as.w_children = {.children = children}};
}

static inline void node_init_binary(node_t* n, node_kind_t kind,
                                    node_ref_t left, node_ref_t right) {
    *n = (node_t){
        .kind = kind, .as.binary = {.left = left, .right = right}
    };
}

static inline void node_init_ret(node_t* n, node_ref_t child) {
    *n = (node_t){.kind = NODE_RET, .as.w_child = {.child = child}};
}

static inline void node_init_ident(node_t* n, str_t ident) {
    *n = (node_t){.kind = NODE_RET, .as.ident = {.ident = ident}};
}

static inline void node_init_int(node_t* n, uint64_t value) {
    *n = (node_t){.kind = NODE_INT, .as.int_ = {.value = value}};
}
