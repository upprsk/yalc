#pragma once

#include <stdbool.h>
#include <string.h>

/// Compare two null terminated c-strings.
static inline bool streq(char const* a, char const* b) {
    return strcmp(a, b) == 0;
}
