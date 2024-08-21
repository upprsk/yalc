#pragma once

#include <stdint.h>

#include "allocator.h"
#include "ast.h"
#include "tokenizer.h"

typedef struct parse_params {
    error_reporter_t* er;

    char const* source;
    uint32_t    source_len;

    token_t const* tokens;

    allocator_t node_alloc;
} parse_params_t;

node_t* parse(parse_params_t* params);
