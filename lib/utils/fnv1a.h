#pragma once

#include <stddef.h>
#include <stdint.h>

static inline uint32_t fnv1a32(char const* s, size_t length) {
    uint32_t hash = 2166136261u;

    for (size_t i = 0; i < length; i++) {
        hash ^= (uint8_t)s[i];
        hash *= 16777619;
    }

    return hash;
}
