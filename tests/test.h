#pragma once

#include "allocator.h"
#include "da.h"
#include "munit.h"

typedef struct ctx {
    MunitTest*  tests;
    allocator_t alloc;
} ctx_t;

static inline void ctx_add_test(ctx_t* ctx, MunitTest const* test) {
    ctx->tests = da_append(ctx->tests, ctx->alloc, test);
}

// ============================================================================

void test_tokenizer_add_tests(ctx_t* ctx);
