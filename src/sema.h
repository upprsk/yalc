#pragma once

#include "alloc/allocator.h"
#include "ast.h"
#include "errors.h"
#include "tstore.h"

typedef struct sema_desc {
    allocator_t alloc;
    allocator_t temp_alloc;

    tstore_t* ts;

    error_reporter_t* er;
} sema_desc_t;

void sema_pass(sema_desc_t* desc, ast_t* ast, node_ref_t root);
