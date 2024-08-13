#pragma once

#include <stdint.h>
#include <stdio.h>

#include "span.h"

typedef struct node node_t;

typedef struct node_int {
    uint64_t value;
} node_int_t;

typedef struct node_float {
    double value;
} node_float_t;

typedef struct node_ident {
    char const* ident;
} node_ident_t;

typedef enum binop_type {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
} binop_type_t;

static inline char const* binop_to_str(binop_type_t op) {
    switch (op) {
        case BINOP_ADD: return "BINOP_ADD";
        case BINOP_SUB: return "BINOP_SUB";
        case BINOP_MUL: return "BINOP_MUL";
        case BINOP_DIV: return "BINOP_DIV";
    }

    return "?";
}

typedef struct node_binop {
    binop_type_t type;
    node_t*      left;
    node_t*      right;
} node_binop_t;

typedef enum unop_type {
    UNOP_NEG,
} unop_type_t;

static inline char const* unop_to_str(unop_type_t op) {
    switch (op) {
        case UNOP_NEG: return "UNOP_NEG";
    }

    return "?";
}

typedef struct node_unop {
    unop_type_t type;
    node_t*     child;
} node_unop_t;

typedef struct node_stmt_expr {
    node_t* expr;
} node_stmt_expr_t;

typedef struct node_stmt_return {
    // may be null in case it is a bare return.
    node_t* child;
} node_stmt_return_t;

typedef struct node_stmt_block {
    node_t** stmts;
} node_stmt_block_t;

typedef struct node_decl {
    char const* name;
    span_t      name_span;
    // may be null in case of type inference
    node_t* type;
    node_t* init;
} node_decl_t;

typedef struct node_arg {
    char const* name;
    // may be null in case of type inference
    node_t* type;
} node_arg_t;

typedef struct node_assign {
    node_t* lhs;
    node_t* rhs;
} node_assign_t;

typedef struct node_proc {
    // array of arguments
    node_t** args;
    node_t*  return_type;

    // body
    node_t* body;
} node_proc_t;

typedef enum node_type {
    NODE_ERR,

    NODE_INT,
    NODE_FLOAT,
    NODE_IDENT,

    NODE_BINOP,
    NODE_UNOP,

    NODE_STMT_EXPR,
    NODE_STMT_RET,
    NODE_STMT_BLK,

    NODE_DECL,
    NODE_ASSIGN,

    NODE_ARG,
    NODE_PROC,
} node_type_t;

static inline char const* node_type_to_str(node_type_t type) {
    switch (type) {
        case NODE_ERR: return "NODE_ERR";
        case NODE_INT: return "NODE_INT";
        case NODE_FLOAT: return "NODE_FLOAT";
        case NODE_IDENT: return "NODE_IDENT";
        case NODE_BINOP: return "NODE_BINOP";
        case NODE_UNOP: return "NODE_UNOP";
        case NODE_STMT_EXPR: return "NODE_STMT_EXPR";
        case NODE_STMT_RET: return "NODE_STMT_RET";
        case NODE_STMT_BLK: return "NODE_STMT_BLK";
        case NODE_DECL: return "NODE_DECL";
        case NODE_ASSIGN: return "NODE_ASSIGN";
        case NODE_ARG: return "NODE_ARG";
        case NODE_PROC: return "NODE_PROC";
    }

    return "?";
}

struct node {
    node_type_t type;
    span_t      span;
    union {
        node_int_t         int_;
        node_float_t       float_;
        node_ident_t       ident;
        node_binop_t       binop;
        node_unop_t        unop;
        node_stmt_expr_t   stmt_expr;
        node_stmt_return_t stmt_ret;
        node_stmt_block_t  stmt_blk;
        node_decl_t        decl;
        node_assign_t      assign;
        node_arg_t         arg;
        node_proc_t        proc;
    } as;
};

void dump_node(FILE* f, node_t* node, int indent);
