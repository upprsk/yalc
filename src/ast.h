#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "alloc/allocator.h"
#include "common.h"
#include "da/da.h"
#include "munit.h"
#include "slice/slice.h"
#include "span.h"
#include "tstore.h"

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
    NODE_CALL,  // call
    NODE_BLK,   // w_children

    NODE_IF,         // ternary
    NODE_WHILE,      // ternary
    NODE_STMT_EXPR,  // w_child
    NODE_ASSIGN,     // binary

    NODE_ARG,  // arg

    NODE_ADD,    // binary
    NODE_SUB,    // binary
    NODE_MUL,    // binary
    NODE_DIV,    // binary
    NODE_NEG,    // w_child
    NODE_NOT,    // w_child
    NODE_RET,    // w_child
    NODE_REF,    // w_child
    NODE_DEREF,  // w_child

    NODE_PTR,  // ptr

    NODE_IDENT,  // ident
    NODE_INT,    // int
} node_kind_t;

static inline char const* node_kind_str(node_kind_t nk) {
    switch (nk) {
        case NODE_INVAL: return "NODE_INVAL";
        case NODE_MOD: return "NODE_MOD";
        case NODE_DECL: return "NODE_DECL";
        case NODE_PROC: return "NODE_PROC";
        case NODE_CALL: return "NODE_CALL";
        case NODE_BLK: return "NODE_BLK";
        case NODE_IF: return "NODE_IF";
        case NODE_WHILE: return "NODE_WHILE";
        case NODE_STMT_EXPR: return "NODE_STMT_EXPR";
        case NODE_ASSIGN: return "NODE_ASSIGN";
        case NODE_ADD: return "NODE_ADD";
        case NODE_ARG: return "NODE_ARG";
        case NODE_SUB: return "NODE_SUB";
        case NODE_MUL: return "NODE_MUL";
        case NODE_DIV: return "NODE_DIV";
        case NODE_NEG: return "NODE_NEG";
        case NODE_NOT: return "NODE_NOT";
        case NODE_RET: return "NODE_RET";
        case NODE_REF: return "NODE_REF";
        case NODE_DEREF: return "NODE_DEREF";
        case NODE_PTR: return "NODE_PTR";
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

typedef struct node_arg {
    str_t      name;
    node_ref_t type;
} node_arg_t;

typedef struct node_binary {
    node_ref_t left;
    node_ref_t right;
} node_binary_t;

typedef struct node_ternary {
    node_ref_t cond;
    node_ref_t wtrue;
    node_ref_t wfalse;
} node_ternary_t;

typedef enum node_decl_flags {
    DECL_EXTERN = 1 << 0,
    DECL_VAR = 1 << 1,
} node_decl_flags_t;

static inline bool decl_is_extern(node_decl_flags_t f) {
    return f & DECL_EXTERN;
}

static inline bool decl_is_var(node_decl_flags_t f) { return f & DECL_VAR; }

typedef struct node_decl {
    str_t             extern_name;
    str_t             name;
    node_ref_t        type;
    node_ref_t        init;
    node_decl_flags_t flags;
} node_decl_t;

typedef enum node_proc_flags {
    PROC_HAS_VARARG = 1 << 0,
} node_proc_flags_t;

static inline bool proc_has_vararg(node_proc_flags_t f) {
    return f & PROC_HAS_VARARG;
}

typedef struct node_proc {
    node_proc_flags_t flags;
    node_arr_t        args;
    node_ref_t        ret;
    node_ref_t        body;
} node_proc_t;

typedef struct node_call {
    node_ref_t callee;
    node_arr_t args;
} node_call_t;

typedef struct node_ptr {
    node_ref_t       term;
    node_ref_t       child;
    node_ptr_flags_t flags;
} node_ptr_t;

typedef struct node_ident {
    str_t ident;
} node_ident_t;

typedef struct node_int {
    uint64_t value;
} node_int_t;

typedef struct node {
    span_t      span;
    node_kind_t kind;
    // we have 7 bytes free here
    union {
        node_w_child_t    w_child;
        node_w_children_t w_children;
        node_arg_t        arg;
        node_binary_t     binary;
        node_ternary_t    ternary;

        node_decl_t decl;
        node_proc_t proc;
        node_call_t call;
        node_ptr_t  ptr;

        node_ident_t ident;
        node_int_t   int_;
    } as;
} node_t;

typedef da_t(node_t) da_node_t;
typedef da_t(node_ref_t) da_node_ref_t;
typedef da_t(type_ref_t) da_type_ref_t;
typedef slice_t(node_ref_t) slice_node_ref_t;

typedef struct ast {
    da_node_t     nodes;
    da_node_ref_t refs;
    da_type_ref_t types;
    // allocator_t   alloc;
} ast_t;

static inline void ast_init(ast_t* a, allocator_t alloc) {
    assert_not_null(a);

    *a = (ast_t){.nodes = da_init(alloc),
                 .refs = da_init(alloc),
                 .types = da_init(alloc)};

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
    assert_uint32(ref.id, <, a->nodes.size);

    return &a->nodes.items[ref.id];
}

static inline span_t ast_get_span(ast_t const* a, node_ref_t ref) {
    return ast_get(a, ref)->span;
}

static inline type_ref_t ast_get_type(ast_t const* a, node_ref_t ref) {
    assert_uint32(ref.id, >, 0);

    return da_at(a->types, ref.id);
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

static inline void ast_set_type(ast_t* a, node_ref_t ref, type_ref_t type) {
    assert_uint32(ref.id, >, 0);
    assert_uint32(ref.id, <, a->nodes.size);

    da_resize(&a->types, a->nodes.size);
    a->types.items[ref.id] = type;
}

string_t ast_dump(ast_t const* a, node_ref_t node, allocator_t alloc);
string_t ast_dump_with_types(ast_t const* a, tstore_t* ts, node_ref_t node,
                             allocator_t alloc);

static inline void node_init_mod(node_t* n, span_t span, node_arr_t children) {
    *n = (node_t){.kind = NODE_MOD,
                  .span = span,
                  .as.w_children = {.children = children}};
}

static inline void node_init_decl(node_t* n, span_t span,
                                  node_decl_flags_t flags, str_t extern_name,
                                  str_t name, node_ref_t type,
                                  node_ref_t init) {
    *n = (node_t){
        .kind = NODE_DECL,
        .span = span,
        .as.decl = {.flags = flags,
                    .extern_name = extern_name,
                    .name = name,
                    .type = type,
                    .init = init}
    };
}

static inline void node_init_proc(node_t* n, span_t span,
                                  node_proc_flags_t flags, node_arr_t args,
                                  node_ref_t ret, node_ref_t body) {
    *n = (node_t){
        .kind = NODE_PROC,
        .span = span,
        .as.proc = {.args = args, .ret = ret, .body = body, .flags = flags}
    };
}

static inline void node_init_call(node_t* n, span_t span, node_ref_t callee,
                                  node_arr_t args) {
    *n = (node_t){
        .kind = NODE_CALL,
        .span = span,
        .as.call = {.callee = callee, .args = args}
    };
}

static inline void node_init_arg(node_t* n, span_t span, str_t name,
                                 node_ref_t type) {
    *n = (node_t){
        .kind = NODE_ARG, .span = span, .as.arg = {.name = name, .type = type}
    };
}

static inline void node_init_blk(node_t* n, span_t span, node_arr_t children) {
    *n = (node_t){.kind = NODE_BLK,
                  .span = span,
                  .as.w_children = {.children = children}};
}

static inline void node_init_binary(node_t* n, span_t span, node_kind_t kind,
                                    node_ref_t left, node_ref_t right) {
    *n = (node_t){
        .kind = kind,
        .span = span,
        .as.binary = {.left = left, .right = right}
    };
}

static inline void node_init_if(node_t* n, span_t span, node_ref_t cond,
                                node_ref_t wtrue, node_ref_t wfalse) {
    *n = (node_t){
        .kind = NODE_IF,
        .span = span,
        .as.ternary = {.cond = cond, .wtrue = wtrue, .wfalse = wfalse}
    };
}

static inline void node_init_while(node_t* n, span_t span, node_ref_t cond,
                                   node_ref_t body) {
    *n = (node_t){
        .kind = NODE_WHILE,
        .span = span,
        .as.ternary = {.cond = cond, .wtrue = body}
    };
}

static inline void node_init_stmt_expr(node_t* n, span_t span,
                                       node_ref_t child) {
    *n = (node_t){
        .kind = NODE_STMT_EXPR, .span = span, .as.w_child = {.child = child}};
}

static inline void node_init_assign(node_t* n, span_t span, node_ref_t left,
                                    node_ref_t right) {
    *n = (node_t){
        .kind = NODE_ASSIGN,
        .span = span,
        .as.binary = {.left = left, .right = right}
    };
}

static inline void node_init_unary(node_t* n, span_t span, node_kind_t kind,
                                   node_ref_t child) {
    *n = (node_t){.kind = kind, .span = span, .as.w_child = {.child = child}};
}

static inline void node_init_ret(node_t* n, span_t span, node_ref_t child) {
    *n = (node_t){
        .kind = NODE_RET, .span = span, .as.w_child = {.child = child}};
}

static inline void node_init_ref(node_t* n, span_t span, node_ref_t child) {
    *n = (node_t){
        .kind = NODE_REF, .span = span, .as.w_child = {.child = child}};
}

static inline void node_init_deref(node_t* n, span_t span, node_ref_t child) {
    *n = (node_t){
        .kind = NODE_DEREF, .span = span, .as.w_child = {.child = child}};
}

static inline void node_init_ident(node_t* n, span_t span, str_t ident) {
    *n = (node_t){
        .kind = NODE_IDENT, .span = span, .as.ident = {.ident = ident}};
}

static inline void node_init_ptr(node_t* n, span_t span, node_ptr_flags_t flags,
                                 node_ref_t child, node_ref_t term) {
    *n = (node_t){
        .kind = NODE_PTR,
        .span = span,
        .as.ptr = {.flags = flags, .child = child, .term = term}
    };
}

static inline void node_init_int(node_t* n, span_t span, uint64_t value) {
    *n = (node_t){.kind = NODE_INT, .span = span, .as.int_ = {.value = value}};
}

static inline node_w_children_t* node_as_mod_mut(node_t* n) {
    assert_int(n->kind, ==, NODE_MOD);
    return &n->as.w_children;
}

static inline node_w_children_t const* node_as_mod(node_t const* n) {
    assert_int(n->kind, ==, NODE_MOD);
    return &n->as.w_children;
}

static inline node_decl_t const* node_as_decl(node_t const* n) {
    assert_int(n->kind, ==, NODE_DECL);
    return &n->as.decl;
}

static inline node_proc_t const* node_as_proc(node_t const* n) {
    assert_int(n->kind, ==, NODE_PROC);
    return &n->as.proc;
}

static inline node_call_t const* node_as_call(node_t const* n) {
    assert_int(n->kind, ==, NODE_CALL);
    return &n->as.call;
}

static inline node_arg_t const* node_as_arg(node_t const* n) {
    assert_int(n->kind, ==, NODE_ARG);
    return &n->as.arg;
}

static inline node_w_children_t const* node_as_blk(node_t const* n) {
    assert_int(n->kind, ==, NODE_BLK);
    return &n->as.w_children;
}

static inline node_binary_t const* node_as_binary(node_t const* n) {
    assert(n->kind == NODE_ADD || n->kind == NODE_SUB || n->kind == NODE_MUL ||
           n->kind == NODE_DIV);
    return &n->as.binary;
}

static inline node_ternary_t const* node_as_if(node_t const* n) {
    assert_int(n->kind, ==, NODE_IF);
    return &n->as.ternary;
}

static inline node_ternary_t const* node_as_while(node_t const* n) {
    assert_int(n->kind, ==, NODE_WHILE);
    return &n->as.ternary;
}

static inline node_w_child_t const* node_as_stmt_expr(node_t const* n) {
    assert_int(n->kind, ==, NODE_STMT_EXPR);
    return &n->as.w_child;
}

static inline node_binary_t const* node_as_assign(node_t const* n) {
    assert_int(n->kind, ==, NODE_ASSIGN);
    return &n->as.binary;
}

static inline node_w_child_t const* node_as_unary(node_t const* n) {
    assert(n->kind == NODE_NOT || n->kind == NODE_NEG);
    return &n->as.w_child;
}

static inline node_w_child_t const* node_as_ret(node_t const* n) {
    assert_int(n->kind, ==, NODE_RET);
    return &n->as.w_child;
}

static inline node_w_child_t const* node_as_ref(node_t const* n) {
    assert_int(n->kind, ==, NODE_REF);
    return &n->as.w_child;
}

static inline node_w_child_t const* node_as_deref(node_t const* n) {
    assert_int(n->kind, ==, NODE_DEREF);
    return &n->as.w_child;
}

static inline node_ptr_t const* node_as_ptr(node_t const* n) {
    assert_int(n->kind, ==, NODE_PTR);
    return &n->as.ptr;
}

static inline node_ident_t const* node_as_ident(node_t const* n) {
    assert_int(n->kind, ==, NODE_IDENT);
    return &n->as.ident;
}

static inline node_int_t const* node_as_int(node_t const* n) {
    assert_int(n->kind, ==, NODE_INT);
    return &n->as.int_;
}
