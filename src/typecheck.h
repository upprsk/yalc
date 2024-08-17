#pragma once

#include "ast.h"
#include "errors.h"
#include "typestore.h"

typedef struct typecheck_params {
    error_reporter_t* er;
    char const*       filename;

    char const* source;
    uint32_t    source_len;

    node_t*      ast;
    typestore_t* ts;

    allocator_t alloc;
} typecheck_params_t;

void pass_typecheck(typecheck_params_t const* params);
