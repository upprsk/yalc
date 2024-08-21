#include <stdint.h>
#include <stdio.h>

#include "allocator.h"
#include "da.h"
#include "errors.h"
#include "span.h"
#include "test.h"
#include "tokenizer.h"

static MunitResult test_token_token_to_str(MunitParameter const params[],
                                           void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    munit_assert_string_equal("TT_ERR", token_to_str(TT_ERR));
    munit_assert_string_equal("TT_INT", token_to_str(TT_INT));
    munit_assert_string_equal("TT_FLOAT", token_to_str(TT_FLOAT));
    munit_assert_string_equal("TT_IDENT", token_to_str(TT_IDENT));
    munit_assert_string_equal("TT_PLUS", token_to_str(TT_PLUS));
    munit_assert_string_equal("TT_MINUS", token_to_str(TT_MINUS));
    munit_assert_string_equal("TT_STAR", token_to_str(TT_STAR));
    munit_assert_string_equal("TT_SLASH", token_to_str(TT_SLASH));
    munit_assert_string_equal("TT_DOT", token_to_str(TT_DOT));
    munit_assert_string_equal("TT_COMMA", token_to_str(TT_COMMA));
    munit_assert_string_equal("TT_COLON", token_to_str(TT_COLON));
    munit_assert_string_equal("TT_SEMICOLON", token_to_str(TT_SEMICOLON));
    munit_assert_string_equal("TT_EQUAL", token_to_str(TT_EQUAL));
    munit_assert_string_equal("TT_LPAREN", token_to_str(TT_LPAREN));
    munit_assert_string_equal("TT_RPAREN", token_to_str(TT_RPAREN));
    munit_assert_string_equal("TT_LBRACKET", token_to_str(TT_LBRACKET));
    munit_assert_string_equal("TT_RBRACKET", token_to_str(TT_RBRACKET));
    munit_assert_string_equal("TT_LBRACE", token_to_str(TT_LBRACE));
    munit_assert_string_equal("TT_RBRACE", token_to_str(TT_RBRACE));
    munit_assert_string_equal("TT_DOT_LPAREN", token_to_str(TT_DOT_LPAREN));
    munit_assert_string_equal("TT_ARROW", token_to_str(TT_ARROW));
    munit_assert_string_equal("TT_RETURN", token_to_str(TT_RETURN));
    munit_assert_string_equal("TT_EOF", token_to_str(TT_EOF));

    return MUNIT_OK;
}

static MunitResult test_tokenize_empty(MunitParameter const params[],
                                       void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    FILE*            errstream = tmpfile();
    error_reporter_t er = {.stream = errstream};

    char const source[] = "";
    uint32_t   source_len = sizeof(source) - 1;

    allocator_t alloc;
    allocator_init_stdc(&alloc);

    token_t* tokens = tokenize(&er, alloc, source, source_len);
    munit_assert_not_null(tokens);

    munit_assert_uint32(da_get_size(tokens), ==, 1);
    munit_assert_uint8(tokens[0].type, ==, TT_EOF);
    munit_assert_uint32(tokens[0].span.start, ==, 0);
    munit_assert_uint32(tokens[0].span.end, ==, 0);

    long errstream_size = ftell(errstream);
    munit_assert_long(errstream_size, ==, 0);

    da_free(tokens, alloc);
    fclose(errstream);

    return MUNIT_OK;
}

static MunitResult test_tokenize_numbers(MunitParameter const params[],
                                         void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    FILE*            errstream = tmpfile();
    error_reporter_t er = {.stream = errstream};

    char const source[] = "12 091 12.0 420.69";
    uint32_t   source_len = sizeof(source) - 1;

    allocator_t alloc;
    allocator_init_stdc(&alloc);

    token_t* tokens = tokenize(&er, alloc, source, source_len);
    munit_assert_not_null(tokens);

    token_type_t expected_types[] = {TT_INT, TT_INT, TT_FLOAT, TT_FLOAT,
                                     TT_EOF};
    char const*  expected_strs[] = {"12", "091", "12.0", "420.69", ""};
    munit_assert_size(sizeof(expected_types) / sizeof(expected_types[0]), ==,
                      sizeof(expected_strs) / sizeof(expected_strs[0]));

    size_t count = sizeof(expected_types) / sizeof(expected_types[0]);
    munit_assert_uint32(da_get_size(tokens), ==, count);

    for (size_t i = 0; i < count; ++i) {
        munit_assert_uint8(tokens[i].type, ==, expected_types[i]);

        uint32_t    slice_len;
        char const* slice =
            span_str_parts(tokens[i].span, source, source_len, &slice_len);

        fprintf(stderr, "[%zu] %s: '%.*s'\n", i, token_to_str(tokens[i].type),
                slice_len, slice);

        uint32_t expected_len = strlen(expected_strs[i]);
        munit_assert_uint32(slice_len, ==, expected_len);
        munit_assert_memory_equal(slice_len, slice, expected_strs[i]);
    }

    long errstream_size = ftell(errstream);
    munit_assert_long(errstream_size, ==, 0);

    da_free(tokens, alloc);
    fclose(errstream);

    return MUNIT_OK;
}

void test_tokenizer_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/token/token-to-str",
                          test_token_token_to_str,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tokenize-empty",
                          test_tokenize_empty,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tokenize-numbers",
                          test_tokenize_numbers,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
