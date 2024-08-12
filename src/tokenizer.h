#pragma once

#include <stdint.h>

#include "span.h"

typedef enum token_type : uint8_t {
    TT_ERR,

    TT_INT,
    TT_FLOAT,

    TT_PLUS,   // +
    TT_MINUS,  // -
    TT_STAR,   // *
    TT_SLASH,  // /

    TT_EOF = 0xFF,
} token_type_t;

typedef struct token {
    token_type_t type;
    span_t       span;
} token_t;

typedef struct token_stream {
} token_stream_t;

typedef enum tokenizer_result {
    TRES_OK = 0,
    TRES_FAIL,
} tokenizer_result_t;

tokenizer_result_t tokenize(char const* source, uint32_t source_len);
