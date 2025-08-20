#pragma once

// Dynamic arrays should have the following shape:
//
//     #define Array(T) struct {
//         T     *items;
//         size_t len;
//         size_t cap;
//     }

#include <assert.h>
#include <stddef.h>
#include <stdlib.h> // IWYU pragma: keep

// Initial length of a dynamic array.
#define DA_INITIAL_LEN 8

#define DA_ASSERT(...)  assert(__VA_ARGS__)
#define DA_REALLOC(...) realloc(__VA_ARGS__)
#define DA_FREE(...)    free(__VA_ARGS__)

// Append a value to a dynamic array.
//
//     Array(int) arr = {};
//     da_append(&arr, 1);
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

// Reserve space for at least some ammount of elements. The new capacity is a
// multiple of the current capacity. Does not change len.
//
//     Array(int) arr = {};
//     da_reserve(&arr, 1); // cap is now DA_INITIAL_LEN
//     da_append(&arr, 1); // no allocation
//     da_append(&arr, 2); // no allocation
#define da_reserve(_da, desired_cap)                                           \
    do {                                                                       \
        if ((_da)->cap == 0) (_da)->cap = DA_INITIAL_LEN;                      \
        while ((_da)->cap < (desired_cap))                                     \
            (_da)->cap *= 2;                                                   \
        (_da)->items =                                                         \
            DA_REALLOC((_da)->items, (_da)->cap * sizeof(*(_da)->items));      \
        DA_ASSERT((_da)->items != nullptr);                                    \
    } while (0)

// Reserve space for exactly an ammount of elements. Does not change len.
//
//     Array(int) arr = {};
//     da_reserve_exact(&arr, 2); // cap is now 2
//     da_append(&arr, 1); // no allocation
//     da_append(&arr, 2); // no allocation
#define da_reserve_exact(_da, desired_cap)                                     \
    do {                                                                       \
        if ((_da)->len >= (desired_cap)) break;                                \
        (_da)->cap = (desired_cap);                                            \
        (_da)->items =                                                         \
            DA_REALLOC((_da)->items, (_da)->cap * sizeof(*(_da)->items));      \
        DA_ASSERT((_da)->items != nullptr);                                    \
    } while (0)

// Pop the last element of the array.
//
// - Asserts that the array is not empty.
//
//     Array(int) arr = {};
//     da_append(&arr, 1);
//     da_pop(&arr);
#define da_pop(_da)                                                            \
    do {                                                                       \
        DA_ASSERT((_da)->len > 0);                                             \
        (_da)->len--;                                                          \
    } while (0)

// Get the last element of the array.
//
// NOTE: Performs no checks.
//
//     Array(int) arr = {}
//     da_append(&arr, 1);
//     assert(da_last(&arr) == 1):
#define da_last(_da) (_da)->items[(_da)->len - 1]

// Clear the array, without freeing memory.
//
//     Array(int) arr = {};
//     da_append(&arr, 1);
//     assert(arr.len == 1);
//     da_clear(&arr);
//     assert(arr.len == 0);
#define da_clear(_da)                                                          \
    do {                                                                       \
        (_da)->len = 0;                                                        \
    } while (0)

// Free all memory used by the array and reset fields. The array is in a good
// state after, ready for appending again.
//
//     Array(int) arr = {};
//     defer { da_destroy(&arr); }
#define da_destroy(_da)                                                        \
    do {                                                                       \
        DA_FREE((_da)->items);                                                 \
        (_da)->items = nullptr;                                                \
        (_da)->len   = 0;                                                      \
        (_da)->cap   = 0;                                                      \
    } while (0)
