#include "tokenizer.h"

#include <stdbool.h>

#include "allocator.h"
#include "da.h"
#include "errors.h"

/// Hold state relative to parsing the source code.
typedef struct tokenizer {
    /// Name of the pile beeing parsed
    char const* filename;
    /// Null terminated string of the source code.
    char const* source;
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
                                  allocator_t alloc) {
    *t = (tokenizer_t){
        .filename = filename,
        .source = source,
        .head = source,
        .start = source,
        .er = er,
        .alloc = alloc,
        .tokens = da_init(token_t, alloc),
    };
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

static inline span_t mkspan(tokenizer_t const* t) {
    return (span_t){.start = t->start - t->source, .end = t->head - t->source};
}

static void append_token(tokenizer_t* t, token_type_t ty) {
    token_t tok = {.type = ty, .span = mkspan(t)};
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

static void tokenize_ident(tokenizer_t* t) {
    while (is_digit(peek(t)) || is_alpha(peek(t))) advance(t);

    append_token(t, TT_IDENT);
}

static void tokenize_one(tokenizer_t* t) {
    skip_whitespace(t);

    t->start = t->head;

    if (is_at_end(t)) return;

    char c = next(t);
    switch (c) {
        case '+': append_token(t, TT_PLUS); break;
        case '-': append_token(t, TT_MINUS); break;
        case '*': append_token(t, TT_STAR); break;
        case '/': append_token(t, TT_SLASH); break;
        case ':': append_token(t, TT_COLON); break;
        case ';': append_token(t, TT_SEMICOLON); break;
        case '(': append_token(t, TT_LPAREN); break;
        case ')': append_token(t, TT_RPAREN); break;
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

token_t* tokenize(error_reporter_t* er, char const* filename,
                  char const* source) {
    allocator_t alloc = {};
    allocator_init_stdc(&alloc);

    tokenizer_t t = {};
    tokenizer_init(&t, er, filename, source, alloc);

    while (!is_at_end(&t)) {
        tokenize_one(&t);
    }

    append_token(&t, TT_EOF);

    return t.tokens;
}
