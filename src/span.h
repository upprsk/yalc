#pragma once

#include <stddef.h>
#include <stdint.h>

#include "munit.h"

typedef struct span {
    uint32_t start, end;
} span_t;

/// Get a pointer to the start of string and it's length from a span.
static inline char const* span_str_parts(span_t const* s, char const* source,
                                         uint32_t  source_len,
                                         uint32_t* str_len) {
    munit_assert_uint32(s->start, <, source_len);
    munit_assert_uint32(s->end, <, source_len);
    munit_assert_uint32(s->end, >=, s->start);

    char const* start = &source[s->start];
    uint32_t    len = s->end - s->start;

    *str_len = len;
    return start;
}
