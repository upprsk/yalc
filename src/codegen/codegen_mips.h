#pragma once

#include "ast.h"
#include "errors.h"

typedef struct codegen_mips_params {
    error_reporter_t* er;
    char const*       filename;

    char const* source;
    uint32_t    source_len;

    node_t*      ast;
    typestore_t* ts;

    FILE* out;
} codegen_mips_params_t;

void codegen_mips(codegen_mips_params_t const* params);
