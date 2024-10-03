#pragma once

#include <stdlib.h>
#include <string.h>

#include "munit.h"

typedef struct ctx {
    MunitTest* tests;
    size_t     test_count;
    size_t     test_capacity;
} ctx_t;

static inline void ctx_add_test(ctx_t* ctx, MunitTest const* test) {
    assert_size(ctx->test_count, <=, ctx->test_capacity);

    memcpy(&ctx->tests[ctx->test_count++], test, sizeof(*test));
}

// ============================================================================

// void test_tokenizer_add_tests(ctx_t* ctx);
void test_lib_allocator_add_tests(ctx_t* ctx);
void test_lib_da_add_tests(ctx_t* ctx);
void test_lib_lst_add_tests(ctx_t* ctx);
