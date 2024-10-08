#include "tokenizer.h"

#include <stdbool.h>
#include <stdint.h>

#include "da/da.h"
#include "errors.h"
#include "slice/slice.h"
#include "span.h"

/// Hold state relative to parsing the source code.
typedef struct tokenizer {
    /// Null terminated string of the source code.
    str_t source;
    /// Tokenizing head, points to the current character.
    char const* head;
    /// Start of the current token.
    char const* start;

    /// Report errors
    error_reporter_t* er;

    /// allocator for the tokens array
    allocator_t alloc;

    /// array of the tokens
    da_token_t tokens;
} tokenizer_t;

static inline void tokenizer_init(tokenizer_t* t, error_reporter_t* er,
                                  str_t source, allocator_t alloc) {
    *t = (tokenizer_t){
        .source = source,
        .head = source.ptr,
        .start = source.ptr,
        .er = er,
        .alloc = alloc,
        .tokens.alloc = alloc,
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
    uint32_t start = t->start - t->source.ptr;
    uint32_t end = t->head - t->source.ptr;

    assert_uint32(start, <=, end);
    return (span_t){.start = start, .end = end};
}

static void append_token(tokenizer_t* t, token_type_t ty) {
    da_push_back(&t->tokens, (token_t){.type = ty, .span = mkspan(t)});
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
        report_error(t->er, mkspan(t), "unterminated string literal");
        append_token(t, TT_ERR);
        return;
    }

    advance(t);
    append_token(t, TT_STR);
}

static void tokenize_ident(tokenizer_t* t) {
    while (is_digit(peek(t)) || is_alpha(peek(t))) advance(t);

    str_t ident = span_to_slice(mkspan(t), t->source);

    if (slice_eq(ident, str_from_lit("return"))) {
        append_token(t, TT_RETURN);
    } else if (slice_eq(ident, str_from_lit("const"))) {
        append_token(t, TT_CONST);
    } else if (slice_eq(ident, str_from_lit("break"))) {
        append_token(t, TT_BREAK);
    } else if (slice_eq(ident, str_from_lit("extern"))) {
        append_token(t, TT_EXTERN);
    } else if (slice_eq(ident, str_from_lit("export"))) {
        append_token(t, TT_EXPORT);
    } else if (slice_eq(ident, str_from_lit("if"))) {
        append_token(t, TT_IF);
    } else if (slice_eq(ident, str_from_lit("else"))) {
        append_token(t, TT_ELSE);
    } else if (slice_eq(ident, str_from_lit("and"))) {
        append_token(t, TT_AND);
    } else if (slice_eq(ident, str_from_lit("or"))) {
        append_token(t, TT_OR);
    } else if (slice_eq(ident, str_from_lit("while"))) {
        append_token(t, TT_WHILE);
    } else if (slice_eq(ident, str_from_lit("as"))) {
        append_token(t, TT_AS);
    } else if (slice_eq(ident, str_from_lit("record"))) {
        append_token(t, TT_RECORD);
    } else if (slice_eq(ident, str_from_lit("opaque"))) {
        append_token(t, TT_OPAQUE);
    } else if (slice_eq(ident, str_from_lit("defer"))) {
        append_token(t, TT_DEFER);
    } else {
        append_token(t, TT_IDENT);
    }
}

static void tokenize_kw(tokenizer_t* t) {
    while (is_digit(peek(t)) || is_alpha(peek(t))) advance(t);

    append_token(t, TT_KW);
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
            else if (match(t, '{'))
                append_token(t, TT_DOT_LBRACE);
            else if (match(t, '*'))
                append_token(t, TT_DOT_STAR);
            else if (match(t, '.')) {
                if (match(t, '.'))
                    append_token(t, TT_DOT_DOT_DOT);
                else
                    append_token(t, TT_DOT_DOT);
            } else if (is_alpha(peek(t)))
                tokenize_kw(t);
            else
                append_token(t, TT_DOT);
            break;
        case ',': append_token(t, TT_COMMA); break;
        case ':': append_token(t, TT_COLON); break;
        case ';': append_token(t, TT_SEMICOLON); break;
        case '=':
            if (match(t, '='))
                append_token(t, TT_EQUAL_EQUAL);
            else
                append_token(t, TT_EQUAL);
            break;
        case '<':
            if (match(t, '='))
                append_token(t, TT_SMALLER_EQUAL);
            else
                append_token(t, TT_SMALLER);
            break;
        case '>':
            if (match(t, '='))
                append_token(t, TT_LARGER_EQUAL);
            else
                append_token(t, TT_LARGER);
            break;
        case '&': append_token(t, TT_AMPERSAND); break;
        case '!':
            if (match(t, '='))
                append_token(t, TT_BANG_EQUAL);
            else
                append_token(t, TT_BANG);
            break;
        case '?': append_token(t, TT_QUESTION); break;
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
            report_error(t->er, mkspan(t), "invalid character: '%c'", c);
            append_token(t, TT_ERR);
            break;
    }
}

slice_token_t tokenize(error_reporter_t* er, allocator_t alloc, str_t source) {
    tokenizer_t t = {};
    tokenizer_init(&t, er, source, alloc);

    while (!is_at_end(&t)) {
        tokenize_one(&t);
    }

    t.start = t.head;
    append_token(&t, TT_EOF);

    return da_to_slice_of(t.tokens, slice_token_t);
}
