#include "tokenizer.h"

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "allocator.h"
#include "common.h"
#include "errors.h"
#include "span.h"

/// Hold state relative to parsing the source code.
typedef struct tokenizer {
    /// Name of the pile beeing parsed
    char const* filename;
    /// Null terminated string of the source code.
    char const* source;
    uint32_t    source_len;
    /// Tokenizing head, points to the current character.
    char const* head;
    /// Start of the current token.
    char const* start;

    /// Report errors
    error_reporter_t* er;

    /// allocator for the tokens array
    allocator_t alloc;

    /// array of the tokens
    token_t* tokens;
} tokenizer_t;

static inline void tokenizer_init(tokenizer_t* t, error_reporter_t* er,
                                  char const* filename, char const* source,
                                  uint32_t source_len, allocator_t alloc) {
    *t = (tokenizer_t){
        .filename = filename,
        .source = source,
        .source_len = source_len,
        .head = source,
        .start = source,
        .er = er,
        .alloc = alloc,
        .tokens = da_init_token(alloc),
    };
}

static inline char peek(tokenizer_t const* t) { return *t->head; }
static inline bool is_at_end(tokenizer_t const* t) { return peek(t) == 0; }

static inline void advance(tokenizer_t* t) {
    if (!is_at_end(t)) t->head++;
}

static inline char next(tokenizer_t* t) {
    char c = peek(t);
    advance(t);

    return c;
}

static inline bool match(tokenizer_t* t, char test) {
    if (peek(t) != test) return false;

    advance(t);
    return true;
}

static inline char peek_next(tokenizer_t const* t) {
    return is_at_end(t) ? 0 : t->head[1];
}

static inline span_t mkspan(tokenizer_t const* t) {
    uint32_t start = t->start - t->source;
    uint32_t end = t->head - t->source;

    munit_assert_uint32(start, <=, end);
    return (span_t){.start = start, .end = end};
}

static void append_token(tokenizer_t* t, token_type_t ty) {
    token_t tok = {.type = ty, .span = mkspan(t)};
    t->tokens = da_append_token(t->tokens, t->alloc, &tok);
}

static void skip_whitespace(tokenizer_t* t) {
    while (true) {
        switch (peek(t)) {
            case ' ':
            case '\t':
            case '\r':
            case '\n': advance(t); break;
            case '/':
                if (peek_next(t) == '/') {
                    while (!is_at_end(t) && peek(t) != '\n') advance(t);
                    break;
                }
            default: return;
        }
    }
}

static inline bool is_digit(char c) {
    switch (c) {
        case '0' ... '9': return true;
        default: return false;
    }
}

static inline bool is_alpha(char c) {
    switch (c) {
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '_': return true;
        default: return false;
    }
}

static void tokenize_int(tokenizer_t* t) {
    while (is_digit(peek(t))) advance(t);

    if (peek(t) == '.' && is_digit(peek_next(t))) {
        advance(t);
        while (is_digit(peek(t))) advance(t);

        append_token(t, TT_FLOAT);
        return;
    }

    append_token(t, TT_INT);
}

static void tokenize_str(tokenizer_t* t) {
    while (!is_at_end(t) && peek(t) != '"') {
        if (peek(t) == '\\' && peek_next(t) == '"') advance(t);
        advance(t);
    }

    if (is_at_end(t)) {
        report_error(t->er, t->filename, t->source, mkspan(t),
                     "unterminated string literal");
        append_token(t, TT_ERR);
        return;
    }

    advance(t);
    append_token(t, TT_STR);
}

static void tokenize_ident(tokenizer_t* t) {
    while (is_digit(peek(t)) || is_alpha(peek(t))) advance(t);

    uint32_t    len;
    char const* ident =
        span_str_parts(mkspan(t), t->source, t->source_len, &len);
    if (strncmp(ident, "return", min(len, (uint32_t)6)) == 0) {
        append_token(t, TT_RETURN);
    } else if (strncmp(ident, "extern", min(len, (uint32_t)6)) == 0) {
        append_token(t, TT_EXTERN);
    } else {
        append_token(t, TT_IDENT);
    }
}

static void tokenize_one(tokenizer_t* t) {
    skip_whitespace(t);

    t->start = t->head;

    if (is_at_end(t)) return;

    char c = next(t);
    switch (c) {
        case '+': append_token(t, TT_PLUS); break;
        case '-':
            if (match(t, '>'))
                append_token(t, TT_ARROW);
            else
                append_token(t, TT_MINUS);
            break;
        case '*': append_token(t, TT_STAR); break;
        case '/': append_token(t, TT_SLASH); break;
        case '.':
            if (match(t, '('))
                append_token(t, TT_DOT_LPAREN);
            else if (match(t, '*'))
                append_token(t, TT_DOT_STAR);
            else
                append_token(t, TT_DOT);
            break;
        case ',': append_token(t, TT_COMMA); break;
        case ':': append_token(t, TT_COLON); break;
        case ';': append_token(t, TT_SEMICOLON); break;
        case '=': append_token(t, TT_EQUAL); break;
        case '&': append_token(t, TT_AMPERSAND); break;
        case '(': append_token(t, TT_LPAREN); break;
        case ')': append_token(t, TT_RPAREN); break;
        case '[': append_token(t, TT_LBRACKET); break;
        case ']': append_token(t, TT_RBRACKET); break;
        case '{': append_token(t, TT_LBRACE); break;
        case '}': append_token(t, TT_RBRACE); break;
        case '"': return tokenize_str(t);
        case '0' ... '9': return tokenize_int(t);
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '_': return tokenize_ident(t);
        default:
            report_error(t->er, t->filename, t->source, mkspan(t),
                         "invalid character: '%c'", c);
            append_token(t, TT_ERR);
            break;
    }
}

token_t* tokenize(error_reporter_t* er, allocator_t alloc, char const* filename,
                  char const* source, uint32_t source_len) {
    tokenizer_t t = {};
    tokenizer_init(&t, er, filename, source, source_len, alloc);

    while (!is_at_end(&t)) {
        tokenize_one(&t);
    }

    t.start = t.head;
    append_token(&t, TT_EOF);

    return t.tokens;
}
