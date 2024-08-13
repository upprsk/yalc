#pragma once

#include <stdint.h>

#include "allocator.h"
#include "ast.h"
#include "tokenizer.h"

typedef struct parse_params {
    error_reporter_t* er;
    char const*       filename;

    char const* source;
    uint32_t    source_len;

    token_t const* tokens;

    Arena* arena;
} parse_params_t;

node_t* parse(parse_params_t* params);
