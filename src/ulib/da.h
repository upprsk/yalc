#pragma once

#include <assert.h>
#include <stddef.h>
#include <stdlib.h> // IWYU pragma: keep

#define DA_INITIAL_LEN  8
#define DA_ASSERT(...)  assert(__VA_ARGS__)
#define DA_REALLOC(...) realloc(__VA_ARGS__)
#define DA_FREE(...)    free(__VA_ARGS__)

#define da_append(_da, ...)                                                    \
    do {                                                                       \
        if ((_da)->len >= (_da)->cap) {                                        \
            /* not enough space, allocate more */                              \
            (_da)->cap = (_da)->cap > 0 ? (_da)->cap * 2 : DA_INITIAL_LEN;     \
            (_da)->items =                                                     \
                DA_REALLOC((_da)->items, (_da)->cap * sizeof(*(_da)->items));  \
            DA_ASSERT((_da)->items != nullptr);                                \
        }                                                                      \
        (_da)->items[(_da)->len++] = __VA_ARGS__;                              \
    } while (0)

#define da_pop(_da)                                                            \
    do {                                                                       \
        DA_ASSERT((_da)->len > 0);                                             \
        (_da)->len--;                                                          \
    } while (0)

#define da_last(_da) (_da)->items[(_da)->len - 1]

#define da_clear(_da)                                                          \
    do {                                                                       \
        (_da)->len = 0;                                                        \
    } while (0)

#define da_destroy(_da)                                                        \
    do {                                                                       \
        DA_FREE((_da)->items);                                                 \
        (_da)->items = nullptr;                                                \
        (_da)->len   = 0;                                                      \
        (_da)->cap   = 0;                                                      \
    } while (0)
