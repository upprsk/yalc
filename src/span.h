#pragma once

#include <stddef.h>
#include <stdint.h>

#include "munit.h"
#include "slice/slice.h"

typedef struct span {
    uint32_t start, len;
} span_t;

static inline str_t span_to_slice(span_t s, str_t source) {
    return slice_s(source, s.start, s.start + s.len);
}
