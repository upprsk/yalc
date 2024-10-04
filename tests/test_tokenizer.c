#include <stdint.h>
#include <stdio.h>

#include "alloc/allocator.h"
#include "da/da.h"
#include "span.h"
#include "test.h"
#include "tokenizer.h"

static MunitResult test_token_token_to_str(MunitParameter const params[],
                                           void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    assert_string_equal("TT_ERR", token_to_str(TT_ERR));
    assert_string_equal("TT_INT", token_to_str(TT_INT));
    assert_string_equal("TT_FLOAT", token_to_str(TT_FLOAT));
    assert_string_equal("TT_IDENT", token_to_str(TT_IDENT));
    assert_string_equal("TT_PLUS", token_to_str(TT_PLUS));
    assert_string_equal("TT_MINUS", token_to_str(TT_MINUS));
    assert_string_equal("TT_STAR", token_to_str(TT_STAR));
    assert_string_equal("TT_SLASH", token_to_str(TT_SLASH));
    assert_string_equal("TT_DOT", token_to_str(TT_DOT));
    assert_string_equal("TT_COMMA", token_to_str(TT_COMMA));
    assert_string_equal("TT_COLON", token_to_str(TT_COLON));
    assert_string_equal("TT_SEMICOLON", token_to_str(TT_SEMICOLON));
    assert_string_equal("TT_EQUAL", token_to_str(TT_EQUAL));
    assert_string_equal("TT_LPAREN", token_to_str(TT_LPAREN));
    assert_string_equal("TT_RPAREN", token_to_str(TT_RPAREN));
    assert_string_equal("TT_LBRACKET", token_to_str(TT_LBRACKET));
    assert_string_equal("TT_RBRACKET", token_to_str(TT_RBRACKET));
    assert_string_equal("TT_LBRACE", token_to_str(TT_LBRACE));
    assert_string_equal("TT_RBRACE", token_to_str(TT_RBRACE));
    assert_string_equal("TT_DOT_LPAREN", token_to_str(TT_DOT_LPAREN));
    assert_string_equal("TT_ARROW", token_to_str(TT_ARROW));
    assert_string_equal("TT_RETURN", token_to_str(TT_RETURN));
    assert_string_equal("TT_EOF", token_to_str(TT_EOF));

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

    allocator_t alloc = c_allocator();

    da_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.items);

    assert_uint32(tokens.size, ==, 1);
    assert_uint8(da_at(tokens, 0).type, ==, TT_EOF);
    assert_uint32(da_at(tokens, 0).span.start, ==, 0);
    assert_uint32(da_at(tokens, 0).span.len, ==, 0);

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);

    da_free(&tokens);
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

    allocator_t alloc = c_allocator();

    da_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.items);

    token_type_t expected_types[] = {TT_INT, TT_INT, TT_FLOAT, TT_FLOAT,
                                     TT_EOF};
    char const*  expected_strs[] = {"12", "091", "12.0", "420.69", ""};
    assert_size(sizeof(expected_types) / sizeof(expected_types[0]), ==,
                sizeof(expected_strs) / sizeof(expected_strs[0]));

    size_t count = sizeof(expected_types) / sizeof(expected_types[0]);
    assert_uint32(tokens.size, ==, count);

    for (size_t i = 0; i < count; ++i) {
        assert_uint8(da_at(tokens, i).type, ==, expected_types[i]);

        str_t slice =
            span_to_slice(da_at(tokens, i).span, (str_t){source, source_len});

        fprintf(stderr, "[%zu] %s: '%.*s'\n", i,
                token_to_str(da_at(tokens, i).type), (int)slice.len, slice.ptr);

        uint32_t expected_len = strlen(expected_strs[i]);
        assert_uint32(slice.len, ==, expected_len);
        assert_memory_equal(slice.len, slice.ptr, expected_strs[i]);
    }

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);

    da_free(&tokens);
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
                          "/tokenize/empty",
                          test_tokenize_empty,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tokenize/numbers",
                          test_tokenize_numbers,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
