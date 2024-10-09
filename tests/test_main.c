#include "munit.h"
#include "test.h"

int main(int argc, char* argv[]) {
    size_t test_capacity = 64;
    ctx_t  ctx = {.tests = calloc(test_capacity, sizeof(*ctx.tests)),
                  .test_capacity = test_capacity};

    test_lib_allocator_add_tests(&ctx);
    test_lib_da_add_tests(&ctx);
    test_lib_lst_add_tests(&ctx);
    test_lib_map_add_tests(&ctx);

    test_tokenizer_add_tests(&ctx);
    test_parser_add_tests(&ctx);
    test_tstore_add_tests(&ctx);

    ctx_add_test(&ctx, &(MunitTest){});

    MunitSuite suite = {"/yal", ctx.tests, NULL, 1, MUNIT_SUITE_OPTION_NONE};
    int        r = munit_suite_main(&suite, NULL, argc, argv);

    free(ctx.tests);

    return r;
}
