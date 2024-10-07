#pragma once

#include <stddef.h>
#include <stdint.h>

#include "munit.h"
#include "slice/slice.h"

typedef struct span {
    uint32_t start, end;
} span_t;

static inline str_t span_to_slice(span_t s, str_t source) {
    return slice_s(source, s.start, s.end);
}

static inline uint32_t span_len(span_t s) { return s.end - s.start; }

static inline span_t span_between(span_t start, span_t end) {
    return (span_t){.start = start.start, .end = end.end};
}
