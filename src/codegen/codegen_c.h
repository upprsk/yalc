#pragma once

#include "ast.h"
#include "errors.h"

typedef struct codegen_c_params {
    error_reporter_t* er;
    char const*       filename;

    node_t*      ast;
    typestore_t* ts;

    FILE* out;
} codegen_c_params_t;

void codegen_c(codegen_c_params_t const* params);
