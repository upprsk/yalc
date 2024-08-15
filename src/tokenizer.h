#pragma once

#include <stdint.h>

#include "allocator.h"
#include "da.h"
#include "errors.h"
#include "span.h"

typedef enum token_type : uint8_t {
    TT_ERR,

    TT_INT,
    TT_FLOAT,
    TT_STR,
    TT_IDENT,

    TT_PLUS,   // +
    TT_MINUS,  // -
    TT_STAR,   // *
    TT_SLASH,  // /

    TT_DOT,        // .
    TT_COMMA,      // ,
    TT_COLON,      // :
    TT_SEMICOLON,  // ;
    TT_EQUAL,      // =
    TT_AMPERSAND,  // &

    TT_LPAREN,    // (
    TT_RPAREN,    // )
    TT_LBRACKET,  // [
    TT_RBRACKET,  // [
    TT_LBRACE,    // {
    TT_RBRACE,    // {

    TT_DOT_LPAREN,  // .(
    TT_DOT_STAR,    // .*
    TT_ARROW,       // ->

    TT_RETURN,  // return
    TT_EXTERN,  // extern

    TT_EOF = 0xFF,
} token_type_t;

static inline char const* token_to_str(token_type_t tt) {
    switch (tt) {
        case TT_ERR: return "TT_ERR";
        case TT_INT: return "TT_INT";
        case TT_FLOAT: return "TT_FLOAT";
        case TT_STR: return "TT_STR";
        case TT_IDENT: return "TT_IDENT";
        case TT_PLUS: return "TT_PLUS";
        case TT_MINUS: return "TT_MINUS";
        case TT_STAR: return "TT_STAR";
        case TT_SLASH: return "TT_SLASH";
        case TT_DOT: return "TT_DOT";
        case TT_COMMA: return "TT_COMMA";
        case TT_COLON: return "TT_COLON";
        case TT_SEMICOLON: return "TT_SEMICOLON";
        case TT_EQUAL: return "TT_EQUAL";
        case TT_AMPERSAND: return "TT_AMPERSAND";
        case TT_LPAREN: return "TT_LPAREN";
        case TT_RPAREN: return "TT_RPAREN";
        case TT_LBRACKET: return "TT_LBRACKET";
        case TT_RBRACKET: return "TT_RBRACKET";
        case TT_LBRACE: return "TT_LBRACE";
        case TT_RBRACE: return "TT_RBRACE";
        case TT_DOT_LPAREN: return "TT_DOT_LPAREN";
        case TT_DOT_STAR: return "TT_DOT_STAR";
        case TT_ARROW: return "TT_ARROW";
        case TT_RETURN: return "TT_RETURN";
        case TT_EXTERN: return "TT_EXTERN";
        case TT_EOF: return "TT_EOF";
    }

    return "?";
}

typedef struct token {
    token_type_t type;
    span_t       span;
} token_t;

da_declare(token_t, token);

token_t* tokenize(error_reporter_t* er, allocator_t alloc, char const* filename,
                  char const* source, uint32_t source_len);
