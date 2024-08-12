#pragma once

#include <stdint.h>

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

typedef struct node_binop {
    binop_type_t type;
    node_t*      left;
    node_t*      right;
} node_binop_t;

typedef enum unop_type {
    UNOP_NEG,
} unop_type_t;

typedef struct node_unop {
    unop_type_t type;
    node_t*     child;
} node_unop_t;

typedef enum node_type {
    NODE_ERR,

    NODE_INT,
    NODE_FLOAT,
    NODE_IDENT,

    NODE_BINOP,
    NODE_UNOP,
} node_type_t;

struct node {
    node_type_t type;
    span_t      span;
    union {
        node_int_t   int_;
        node_float_t float_;
        node_ident_t ident;
        node_binop_t binop;
        node_unop_t  unop;
    } as;
};
