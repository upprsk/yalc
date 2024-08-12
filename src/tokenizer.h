#pragma once

#include <stdint.h>

#include "errors.h"
#include "span.h"

typedef enum token_type : uint8_t {
    TT_ERR,

    TT_INT,
    TT_FLOAT,
    TT_IDENT,

    TT_PLUS,   // +
    TT_MINUS,  // -
    TT_STAR,   // *
    TT_SLASH,  // /

    TT_COLON,      // :
    TT_SEMICOLON,  // ;

    TT_LPAREN,  // (
    TT_RPAREN,  // )

    TT_EOF = 0xFF,
} token_type_t;

static inline char const* token_to_str(token_type_t tt) {
    switch (tt) {
        case TT_ERR: return "TT_ERR";
        case TT_INT: return "TT_INT";
        case TT_FLOAT: return "TT_FLOAT";
        case TT_IDENT: return "TT_IDENT";
        case TT_PLUS: return "TT_PLUS";
        case TT_MINUS: return "TT_MINUS";
        case TT_STAR: return "TT_STAR";
        case TT_SLASH: return "TT_SLASH";
        case TT_COLON: return "TT_COLON";
        case TT_SEMICOLON: return "TT_SEMICOLON";
        case TT_LPAREN: return "TT_LPAREN";
        case TT_RPAREN: return "TT_RPAREN";
        case TT_EOF: return "TT_EOF";
    }

    return "?";
}

typedef struct token {
    token_type_t type;
    span_t       span;
} token_t;

token_t* tokenize(error_reporter_t* er, char const* filename,
                  char const* source);
