#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "da.h"
#include "span.h"
#include "typestore.h"

typedef struct node node_t;

typedef struct node_int {
    uint64_t value;
} node_int_t;

typedef struct node_float {
    double value;
} node_float_t;

typedef struct node_str {
    uint32_t    len;
    char const* str;
} node_str_t;

typedef struct node_ident {
    char const* ident;
} node_ident_t;

typedef struct node_kw {
    char const* ident;
} node_kw_t;

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
    UNOP_NOT,
} unop_type_t;

static inline char const* unop_to_str(unop_type_t op) {
    switch (op) {
        case UNOP_NEG: return "UNOP_NEG";
        case UNOP_NOT: return "UNOP_NOT";
    }

    return "?";
}

typedef struct node_unop {
    unop_type_t type;
    node_t*     child;
} node_unop_t;

typedef enum node_logic_type {
    LOGIC_AND,
    LOGIC_OR,
} node_logic_type_t;

static inline char const* logic_to_str(node_logic_type_t op) {
    switch (op) {
        case LOGIC_AND: return "LOGIC_AND";
        case LOGIC_OR: return "LOGIC_OR";
    }

    return "?";
}

typedef struct node_logic {
    node_logic_type_t type;
    node_t*           left;
    node_t*           right;
} node_logic_t;

typedef enum node_comp_type {
    COMP_EQ,
    COMP_NEQ,
    COMP_LT,
    COMP_LTE,
    COMP_GT,
    COMP_GTE,
} node_comp_type_t;

static inline char const* comp_to_str(node_comp_type_t op) {
    switch (op) {
        case COMP_EQ: return "COMP_EQ";
        case COMP_NEQ: return "COMP_NEQ";
        case COMP_LT: return "COMP_LT";
        case COMP_LTE: return "COMP_LTE";
        case COMP_GT: return "COMP_GT";
        case COMP_GTE: return "COMP_GTE";
    }

    return "?";
}

typedef struct node_comp {
    node_comp_type_t type;
    node_t*          left;
    node_t*          right;
} node_comp_t;

typedef struct node_index {
    node_t* receiver;
    node_t* index;
} node_index_t;

typedef struct node_call {
    node_t*  callee;
    node_t** args;

    // FIXME: this is not how this should be handled
    char const* call_extern_name;
} node_call_t;

typedef struct node_ref {
    node_t* child;
} node_ref_t;

typedef struct node_deref {
    node_t* child;
} node_deref_t;

typedef struct node_cast {
    node_t* child;
    node_t* type;
} node_cast_t;

typedef struct node_stmt_expr {
    node_t* expr;
} node_stmt_expr_t;

typedef struct node_stmt_return {
    // may be null in case it is a bare return.
    node_t* child;
} node_stmt_return_t;

typedef struct node_stmt_break {
    // may be null in case it is a bare break.
    node_t* child;
} node_stmt_break_t;

typedef struct node_stmt_if {
    node_t* condition;
    node_t* when_true;
    // may be null in case there is no else
    node_t* when_false;
} node_stmt_if_t;

typedef struct node_stmt_while {
    node_t* condition;
    node_t* body;
} node_stmt_while_t;

typedef struct node_stmt_block {
    node_t** stmts;
} node_stmt_block_t;

typedef struct node_mod {
    node_t** decls;
} node_mod_t;

typedef struct node_decl {
    char const* name;
    char const* extern_name;
    span_t      name_span;
    // may be null in case of type inference
    node_t* type;
    // may be null in case of zero-initialization
    node_t* init;

    bool is_extern;
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

    bool uses_implicit_return;
} node_proc_t;

typedef struct node_record {
    node_t* blk;
} node_record_t;

typedef struct node_cinitf {
    node_t* name;
    node_t* init;
} node_cinitf_t;

typedef struct node_cinit {
    node_t** kids;
} node_cinit_t;

typedef struct node_array {
    node_t*  len;
    node_t*  type;
    node_t** initializer_list;
} node_array_t;

typedef struct node_ptr {
    node_t* child;
} node_ptr_t;

typedef struct node_mptr {
    node_t* child;
    node_t* term;
} node_mptr_t;

typedef enum node_type : uint8_t {
    NODE_ERR,

    NODE_INT,
    NODE_FLOAT,
    NODE_STR,
    NODE_IDENT,
    NODE_KW,

    NODE_BINOP,
    NODE_UNOP,
    NODE_LOGIC,
    NODE_COMP,
    NODE_INDEX,
    NODE_CALL,
    NODE_REF,
    NODE_DEREF,
    NODE_CAST,

    NODE_STMT_EXPR,
    NODE_STMT_RET,
    NODE_STMT_BREAK,
    NODE_STMT_IF,
    NODE_STMT_WHILE,
    NODE_STMT_BLK,

    NODE_MOD,
    NODE_DECL,
    NODE_ASSIGN,

    NODE_ARG,
    NODE_PROC,
    NODE_RECORD,
    NODE_CINITF,
    NODE_CINIT,
    NODE_ARRAY,
    NODE_PTR,
    NODE_MPTR,
} node_type_t;

static inline char const* node_type_to_str(node_type_t type) {
    switch (type) {
        case NODE_ERR: return "NODE_ERR";
        case NODE_INT: return "NODE_INT";
        case NODE_FLOAT: return "NODE_FLOAT";
        case NODE_STR: return "NODE_STR";
        case NODE_IDENT: return "NODE_IDENT";
        case NODE_KW: return "NODE_KW";
        case NODE_BINOP: return "NODE_BINOP";
        case NODE_UNOP: return "NODE_UNOP";
        case NODE_LOGIC: return "NODE_LOGIC";
        case NODE_COMP: return "NODE_COMP";
        case NODE_INDEX: return "NODE_INDEX";
        case NODE_CALL: return "NODE_CALL";
        case NODE_REF: return "NODE_REF";
        case NODE_DEREF: return "NODE_DEREF";
        case NODE_CAST: return "NODE_CAST";
        case NODE_STMT_EXPR: return "NODE_STMT_EXPR";
        case NODE_STMT_RET: return "NODE_STMT_RET";
        case NODE_STMT_BREAK: return "NODE_STMT_BREAK";
        case NODE_STMT_IF: return "NODE_STMT_IF";
        case NODE_STMT_WHILE: return "NODE_STMT_WHILE";
        case NODE_STMT_BLK: return "NODE_STMT_BLK";
        case NODE_MOD: return "NODE_MOD";
        case NODE_DECL: return "NODE_DECL";
        case NODE_ASSIGN: return "NODE_ASSIGN";
        case NODE_ARG: return "NODE_ARG";
        case NODE_PROC: return "NODE_PROC";
        case NODE_RECORD: return "NODE_RECORD";
        case NODE_CINITF: return "NODE_CINITF";
        case NODE_CINIT: return "NODE_CINIT";
        case NODE_ARRAY: return "NODE_ARRAY";
        case NODE_PTR: return "NODE_PTR";
        case NODE_MPTR: return "NODE_MPTR";
    }

    return "?";
}

struct node {
    node_type_t type;
    span_t      span;
    type_id_t   type_id;
    union {
        node_int_t         int_;
        node_float_t       float_;
        node_str_t         str;
        node_ident_t       ident;
        node_kw_t          kw;
        node_binop_t       binop;
        node_unop_t        unop;
        node_logic_t       logic;
        node_comp_t        comp;
        node_index_t       index;
        node_call_t        call;
        node_ref_t         ref;
        node_deref_t       deref;
        node_cast_t        cast;
        node_stmt_expr_t   stmt_expr;
        node_stmt_if_t     stmt_if;
        node_stmt_while_t  stmt_while;
        node_stmt_return_t stmt_ret;
        node_stmt_break_t  stmt_break;
        node_stmt_block_t  stmt_blk;
        node_mod_t         mod;
        node_decl_t        decl;
        node_assign_t      assign;
        node_arg_t         arg;
        node_proc_t        proc;
        node_record_t      record;
        node_cinitf_t      cinitf;
        node_cinit_t       cinit;
        node_array_t       array;
        node_ptr_t         ptr;
        node_mptr_t        mptr;
    } as;
};

da_declare(node_t*, node);

void dump_node(FILE* f, node_t* node, int indent);
