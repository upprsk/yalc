#pragma once

#include "alloc/allocator.h"
#include "ast.h"
#include "errors.h"
#include "slice/slice.h"
#include "tokenizer.h"

typedef struct parser_desc {
    allocator_t alloc;
    allocator_t temp_alloc;

    str_t         source;
    slice_token_t tokens;

    error_reporter_t* er;
} parser_desc_t;

node_ref_t parse(parser_desc_t* desc, ast_t* ast);
