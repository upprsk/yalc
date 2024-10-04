#pragma once

#include <stdint.h>

#include "slice/slice.h"

static inline uint64_t parse_uint64(str_t s) {
    uint64_t value = 0;

    for (size_t i = 0; i < s.len; ++i) {
        value = value * 10 + s.ptr[i] - '0';
    }

    return value;
}
