#pragma once

#include "ast.h"
typedef enum irop {
    IROP_ERR,
} irop_t;

typedef struct irins {
    irop_t op;
} irins_t;

typedef struct pass_to_ir_params {
    node_t* ast;
} pass_to_ir_params_t;

void pass_to_ir(pass_to_ir_params_t const* params);
