#pragma once

#include <stdbool.h>
#include <string.h>

#define max(a, b)               \
    ({                          \
        __typeof__(a) _a = (a); \
        __typeof__(b) _b = (b); \
        _a > _b ? _a : _b;      \
    })

#define min(a, b)               \
    ({                          \
        __typeof__(a) _a = (a); \
        __typeof__(b) _b = (b); \
        _a < _b ? _a : _b;      \
    })

static inline bool streq(char const* a, char const* b) {
    if (a && b) return strcmp(a, b) == 0;
    if (!a && !b) return true;
    return false;
}

static inline bool strnneq(char const* a, size_t alen, char const* b,
                           size_t blen) {
    return alen == blen && memcmp(a, b, alen) == 0;
}
