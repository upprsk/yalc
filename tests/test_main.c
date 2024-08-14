#include "allocator.h"
#include "da.h"
#include "munit.h"
#include "test.h"

static MunitResult my_test(MunitParameter const params[],
                           void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_OK;
}

void add_tests_a(ctx_t* ctx) {
    MunitTest test = {
        "/my-test", my_test, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL,
    };

    ctx->tests = da_append(ctx->tests, ctx->alloc, &test);
}

int main(int argc, char* argv[]) {
    allocator_t alloc;
    allocator_init_stdc(&alloc);

    ctx_t ctx = {.alloc = alloc, .tests = da_init(MunitTest, alloc)};

    test_tokenizer_add_tests(&ctx);
    ctx_add_test(&ctx, &(MunitTest){});

    MunitSuite suite = {"/yal", ctx.tests, NULL, 1, MUNIT_SUITE_OPTION_NONE};
    int        r = munit_suite_main(&suite, NULL, argc, argv);

    da_free(ctx.tests, ctx.alloc);

    return r;
}
