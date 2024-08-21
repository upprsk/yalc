#pragma once

#include "allocator.h"
#include "ast.h"
#include "errors.h"

typedef struct depg_params {
    error_reporter_t* er;
    char const*       filename;
    char const*       source;
    uint32_t          source_len;

    node_t*     ast;
    allocator_t alloc;
} depg_params_t;

// returns a sorted list of the top level declaration nodes.
node_t** pass_depgraph(depg_params_t const* params);
