#include "tokenizer.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "allocator.h"
#include "da.h"
#include "span.h"

/// Hold state relative to parsing the source code.
typedef struct tokenizer {
    /// Null terminated string of the source code.
    char const* source;
    /// Tokenizing head, points to the current character.
    char const* head;
    /// Start of the current token.
    char const* start;

    // allocator for the tokens array
    allocator_t alloc;

    // array of the tokens
    token_t* tokens;
} tokenizer_t;

static inline void tokenizer_init(tokenizer_t* t, char const* source,
                                  allocator_t alloc) {
    *t = (tokenizer_t){
        .source = source,
        .head = source,
        .alloc = alloc,
        .tokens = da_init(token_t, alloc),
    };
}

static inline void tokenizer_deinit(tokenizer_t* t) {
    da_free(t->tokens, t->alloc);
}

static inline char peek(tokenizer_t const* t) { return *t->head; }
static inline void advance(tokenizer_t* t) { t->head++; }
static inline char next(tokenizer_t* t) {
    char c = peek(t);
    advance(t);

    return c;
}

static inline bool is_at_end(tokenizer_t const* t) { return peek(t) == 0; }

static inline char peek_next(tokenizer_t const* t) {
    return is_at_end(t) ? 0 : t->head[1];
}

static void append_token(tokenizer_t* t, token_type_t ty) {
    uint32_t start = t->start - t->source;
    uint32_t end = t->head - t->source;

    token_t tok = {
        .type = ty, .span = {.start = start, .end = end}
    };

    t->tokens = da_append(t->tokens, t->alloc, &tok);
}

static void skip_whitespace(tokenizer_t* t) {
    while (true) switch (peek(t)) {
            case ' ':
            case '\t':
            case '\r':
            case '\n': advance(t); break;
            default: return;
        }
}

static inline bool is_digit(char c) {
    switch (c) {
        case '0' ... '9': return true;
        default: return false;
    }
}

static tokenizer_result_t tokenize_int(tokenizer_t* t) {
    while (is_digit(peek(t))) advance(t);

    if (peek(t) == '.' && is_digit(peek_next(t))) {
        advance(t);
        while (is_digit(peek(t))) advance(t);

        append_token(t, TT_FLOAT);

        return TRES_OK;
    }

    append_token(t, TT_INT);

    return TRES_OK;
}

static tokenizer_result_t tokenize_one(tokenizer_t* t) {
    skip_whitespace(t);

    t->start = t->head;

    if (is_at_end(t)) return TRES_OK;

    char c = next(t);
    switch (c) {
        case '+': append_token(t, TT_PLUS); break;
        case '-': append_token(t, TT_MINUS); break;
        case '*': append_token(t, TT_STAR); break;
        case '/': append_token(t, TT_SLASH); break;
        case '0' ... '9': return tokenize_int(t);
        default:
            fprintf(stderr, "error: invalid character: '%c'\n", c);
            return TRES_FAIL;
    }

    return TRES_OK;
}

tokenizer_result_t tokenize(char const* source, uint32_t source_len) {
    allocator_t alloc = {};
    allocator_init_stdc(&alloc);

    tokenizer_t t = {};
    tokenizer_init(&t, source, alloc);

    tokenizer_result_t result = TRES_OK;

    while (!is_at_end(&t)) {
        result = tokenize_one(&t);
        if (result) goto error;
    }

    append_token(&t, TT_EOF);

    for (size_t i = 0; i < da_get_size(t.tokens); ++i) {
        printf("token (%d) [%d, %d]\n", t.tokens[i].type,
               t.tokens[i].span.start, t.tokens[i].span.end);
    }

    tokenizer_deinit(&t);
    return result;

error:
    tokenizer_deinit(&t);
    return result;
}
