#include <stdint.h>
#include <stdio.h>

#include "alloc/allocator.h"
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
    assert_string_equal("TT_STR", token_to_str(TT_STR));
    assert_string_equal("TT_IDENT", token_to_str(TT_IDENT));
    assert_string_equal("TT_KW", token_to_str(TT_KW));
    assert_string_equal("TT_DOT", token_to_str(TT_DOT));
    assert_string_equal("TT_COMMA", token_to_str(TT_COMMA));
    assert_string_equal("TT_COLON", token_to_str(TT_COLON));
    assert_string_equal("TT_SEMICOLON", token_to_str(TT_SEMICOLON));
    assert_string_equal("TT_EQUAL", token_to_str(TT_EQUAL));
    assert_string_equal("TT_AMPERSAND", token_to_str(TT_AMPERSAND));
    assert_string_equal("TT_BANG", token_to_str(TT_BANG));
    assert_string_equal("TT_QUESTION", token_to_str(TT_QUESTION));
    assert_string_equal("TT_PLUS", token_to_str(TT_PLUS));
    assert_string_equal("TT_MINUS", token_to_str(TT_MINUS));
    assert_string_equal("TT_STAR", token_to_str(TT_STAR));
    assert_string_equal("TT_SLASH", token_to_str(TT_SLASH));
    assert_string_equal("TT_SMALLER", token_to_str(TT_SMALLER));
    assert_string_equal("TT_LARGER", token_to_str(TT_LARGER));
    assert_string_equal("TT_SMALLER_EQUAL", token_to_str(TT_SMALLER_EQUAL));
    assert_string_equal("TT_LARGER_EQUAL", token_to_str(TT_LARGER_EQUAL));
    assert_string_equal("TT_EQUAL_EQUAL", token_to_str(TT_EQUAL_EQUAL));
    assert_string_equal("TT_BANG_EQUAL", token_to_str(TT_BANG_EQUAL));
    assert_string_equal("TT_LPAREN", token_to_str(TT_LPAREN));
    assert_string_equal("TT_RPAREN", token_to_str(TT_RPAREN));
    assert_string_equal("TT_LBRACKET", token_to_str(TT_LBRACKET));
    assert_string_equal("TT_RBRACKET", token_to_str(TT_RBRACKET));
    assert_string_equal("TT_LBRACE", token_to_str(TT_LBRACE));
    assert_string_equal("TT_RBRACE", token_to_str(TT_RBRACE));
    assert_string_equal("TT_DOT_LPAREN", token_to_str(TT_DOT_LPAREN));
    assert_string_equal("TT_DOT_LBRACE", token_to_str(TT_DOT_LBRACE));
    assert_string_equal("TT_DOT_STAR", token_to_str(TT_DOT_STAR));
    assert_string_equal("TT_DOT_DOT", token_to_str(TT_DOT_DOT));
    assert_string_equal("TT_DOT_DOT_DOT", token_to_str(TT_DOT_DOT_DOT));
    assert_string_equal("TT_MINUS_MINUS", token_to_str(TT_MINUS_MINUS));
    assert_string_equal("TT_3MINUS", token_to_str(TT_3MINUS));
    assert_string_equal("TT_ARROW", token_to_str(TT_ARROW));
    assert_string_equal("TT_CONST", token_to_str(TT_CONST));
    assert_string_equal("TT_RETURN", token_to_str(TT_RETURN));
    assert_string_equal("TT_BREAK", token_to_str(TT_BREAK));
    assert_string_equal("TT_EXTERN", token_to_str(TT_EXTERN));
    assert_string_equal("TT_EXPORT", token_to_str(TT_EXPORT));
    assert_string_equal("TT_IF", token_to_str(TT_IF));
    assert_string_equal("TT_ELSE", token_to_str(TT_ELSE));
    assert_string_equal("TT_AND", token_to_str(TT_AND));
    assert_string_equal("TT_OR", token_to_str(TT_OR));
    assert_string_equal("TT_WHILE", token_to_str(TT_WHILE));
    assert_string_equal("TT_AS", token_to_str(TT_AS));
    assert_string_equal("TT_RECORD", token_to_str(TT_RECORD));
    assert_string_equal("TT_OPAQUE", token_to_str(TT_OPAQUE));
    assert_string_equal("TT_DEFER", token_to_str(TT_DEFER));
    assert_string_equal("TT_EOF", token_to_str(TT_EOF));

    return MUNIT_OK;
}

static MunitResult test_tokenize_empty(MunitParameter const params[],
                                       void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    char const source[] = "";
    uint32_t   source_len = sizeof(source) - 1;

    allocator_t alloc = c_allocator();

    FILE*            errstream = tmpfile();
    error_reporter_t er = {
        .stream = errstream, .source = {source, source_len}
    };

    slice_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.ptr);

    assert_uint32(tokens.len, ==, 1);
    assert_uint8(slice_at(tokens, 0).type, ==, TT_EOF);
    assert_uint32(slice_at(tokens, 0).span.start, ==, 0);
    assert_uint32(slice_at(tokens, 0).span.end, ==, 0);

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    slice_free(alloc, &tokens);
    fclose(errstream);

    return MUNIT_OK;
}

static MunitResult test_tokenize_numbers(MunitParameter const params[],
                                         void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    char const source[] = "12 091 12.0 420.69";
    uint32_t   source_len = sizeof(source) - 1;

    allocator_t alloc = c_allocator();

    FILE*            errstream = tmpfile();
    error_reporter_t er = {
        .stream = errstream, .source = {source, source_len}
    };

    slice_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.ptr);

    token_type_t expected_types[] = {TT_INT, TT_INT, TT_FLOAT, TT_FLOAT,
                                     TT_EOF};
    char const*  expected_strs[] = {"12", "091", "12.0", "420.69", ""};
    assert_size(sizeof(expected_types) / sizeof(expected_types[0]), ==,
                sizeof(expected_strs) / sizeof(expected_strs[0]));

    size_t count = sizeof(expected_types) / sizeof(expected_types[0]);
    assert_uint32(tokens.len, ==, count);

    for (size_t i = 0; i < count; ++i) {
        assert_uint8(slice_at(tokens, i).type, ==, expected_types[i]);

        str_t slice = span_to_slice(slice_at(tokens, i).span,
                                    (str_t){source, source_len});

        fprintf(stderr, "[%zu] %s: '%.*s'\n", i,
                token_to_str(slice_at(tokens, i).type), (int)slice.len,
                slice.ptr);

        uint32_t expected_len = strlen(expected_strs[i]);
        assert_uint32(slice.len, ==, expected_len);
        assert_memory_equal(slice.len, slice.ptr, expected_strs[i]);
    }

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    slice_free(alloc, &tokens);
    fclose(errstream);

    return MUNIT_OK;
}

static MunitResult test_tokenize_ident(MunitParameter const params[],
                                       void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    char const source[] = "test _123 a_b_c Yay";
    uint32_t   source_len = sizeof(source) - 1;

    allocator_t alloc = c_allocator();

    FILE*            errstream = tmpfile();
    error_reporter_t er = {
        .stream = errstream, .source = {source, source_len}
    };

    slice_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.ptr);

    token_type_t expected_types[] = {TT_IDENT, TT_IDENT, TT_IDENT, TT_IDENT,
                                     TT_EOF};
    char const*  expected_strs[] = {"test", "_123", "a_b_c", "Yay", ""};
    assert_size(sizeof(expected_types) / sizeof(expected_types[0]), ==,
                sizeof(expected_strs) / sizeof(expected_strs[0]));

    size_t count = sizeof(expected_types) / sizeof(expected_types[0]);
    assert_uint32(tokens.len, ==, count);

    for (size_t i = 0; i < count; ++i) {
        assert_uint8(slice_at(tokens, i).type, ==, expected_types[i]);

        str_t slice = span_to_slice(slice_at(tokens, i).span,
                                    (str_t){source, source_len});

        fprintf(stderr, "[%zu] %s: '%.*s'\n", i,
                token_to_str(slice_at(tokens, i).type), (int)slice.len,
                slice.ptr);

        uint32_t expected_len = strlen(expected_strs[i]);
        assert_uint32(slice.len, ==, expected_len);
        assert_memory_equal(slice.len, slice.ptr, expected_strs[i]);
    }

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    slice_free(alloc, &tokens);
    fclose(errstream);

    return MUNIT_OK;
}

static MunitResult test_tokenize_symbols(MunitParameter const params[],
                                         void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    char const source[] =
        ". , : ; = & ! ? + - * / < > <= >= == != () [] {} .( .{ .* .. ... -- "
        "--- ->";
    uint32_t source_len = sizeof(source) - 1;

    allocator_t alloc = c_allocator();

    FILE*            errstream = tmpfile();
    error_reporter_t er = {
        .stream = errstream, .source = {source, source_len}
    };

    slice_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.ptr);

    token_type_t expected_types[] = {
        TT_DOT,         TT_COMMA,       TT_COLON,         TT_SEMICOLON,
        TT_EQUAL,       TT_AMPERSAND,   TT_BANG,          TT_QUESTION,
        TT_PLUS,        TT_MINUS,       TT_STAR,          TT_SLASH,
        TT_SMALLER,     TT_LARGER,      TT_SMALLER_EQUAL, TT_LARGER_EQUAL,
        TT_EQUAL_EQUAL, TT_BANG_EQUAL,  TT_LPAREN,        TT_RPAREN,
        TT_LBRACKET,    TT_RBRACKET,    TT_LBRACE,        TT_RBRACE,
        TT_DOT_LPAREN,  TT_DOT_LBRACE,  TT_DOT_STAR,      TT_DOT_DOT,
        TT_DOT_DOT_DOT, TT_MINUS_MINUS, TT_3MINUS,        TT_ARROW,
        TT_EOF};
    char const* expected_strs[] = {
        ".", ",", ":",  ";",  "=",  "&",  "!",   "?",  "+",   "-",  "*",
        "/", "<", ">",  "<=", ">=", "==", "!=",  "(",  ")",   "[",  "]",
        "{", "}", ".(", ".{", ".*", "..", "...", "--", "---", "->", ""};
    assert_size(sizeof(expected_types) / sizeof(expected_types[0]), ==,
                sizeof(expected_strs) / sizeof(expected_strs[0]));

    size_t count = sizeof(expected_types) / sizeof(expected_types[0]);
    assert_uint32(tokens.len, ==, count);

    for (size_t i = 0; i < count; ++i) {
        assert_uint8(slice_at(tokens, i).type, ==, expected_types[i]);

        str_t slice = span_to_slice(slice_at(tokens, i).span,
                                    (str_t){source, source_len});

        fprintf(stderr, "[%zu] %s: '%.*s'\n", i,
                token_to_str(slice_at(tokens, i).type), (int)slice.len,
                slice.ptr);

        uint32_t expected_len = strlen(expected_strs[i]);
        assert_uint32(slice.len, ==, expected_len);
        assert_memory_equal(slice.len, slice.ptr, expected_strs[i]);
    }

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    slice_free(alloc, &tokens);
    fclose(errstream);

    return MUNIT_OK;
}

static MunitResult test_tokenize_keywords(MunitParameter const params[],
                                          void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    char const source[] =
        "return const break extern export if else and or while as record "
        "opaque defer";
    uint32_t source_len = sizeof(source) - 1;

    allocator_t alloc = c_allocator();

    FILE*            errstream = tmpfile();
    error_reporter_t er = {
        .stream = errstream, .source = {source, source_len}
    };

    slice_token_t tokens = tokenize(&er, alloc, (str_t){source, source_len});
    assert_not_null(tokens.ptr);

    token_type_t expected_types[] = {TT_RETURN, TT_CONST, TT_BREAK, TT_EXTERN,
                                     TT_EXPORT, TT_IF,    TT_ELSE,  TT_AND,
                                     TT_OR,     TT_WHILE, TT_AS,    TT_RECORD,
                                     TT_OPAQUE, TT_DEFER, TT_EOF};
    char const*  expected_strs[] = {
        "return", "const", "break", "extern", "export", "if",    "else", "and",
        "or",     "while", "as",    "record", "opaque", "defer", ""};
    assert_size(sizeof(expected_types) / sizeof(expected_types[0]), ==,
                sizeof(expected_strs) / sizeof(expected_strs[0]));

    size_t count = sizeof(expected_types) / sizeof(expected_types[0]);
    assert_uint32(tokens.len, ==, count);

    for (size_t i = 0; i < count; ++i) {
        assert_uint8(slice_at(tokens, i).type, ==, expected_types[i]);

        str_t slice = span_to_slice(slice_at(tokens, i).span,
                                    (str_t){source, source_len});

        fprintf(stderr, "[%zu] %s: '%.*s'\n", i,
                token_to_str(slice_at(tokens, i).type), (int)slice.len,
                slice.ptr);

        uint32_t expected_len = strlen(expected_strs[i]);
        assert_uint32(slice.len, ==, expected_len);
        assert_memory_equal(slice.len, slice.ptr, expected_strs[i]);
    }

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    slice_free(alloc, &tokens);
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

    ctx_add_test(ctx, &(MunitTest){
                          "/tokenize/ident",
                          test_tokenize_ident,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tokenize/symbols",
                          test_tokenize_symbols,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tokenize/keywords",
                          test_tokenize_keywords,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
