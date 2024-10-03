#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "utils/minmax.h"

/// Define the type of a dynamic array.
///
/// For each new array type you want to add, write:
///
/// ```c
/// typedef da_t(int) da_int_t;
/// ```
///
/// > Some types are already pre-defined, and some (like string_t) have
/// > additional functionality.
#define da_t(T)          \
    struct {             \
        T*     items;    \
        size_t size;     \
        size_t capacity; \
    }

/// This maps exactly to any `da_t` structure, except that the type of `items`
/// is fixed to `uint8_t` so that we can treat it as a raw view of bytes.
typedef struct {
    uint8_t* items;
    size_t   size;
    size_t   capacity;
} da_opaque_t;

#define da_push_back(_da, ...) da_push_back_opaque((da_opaque_t*)(_da), )

/// Adjust the size of the array, so that it will have space for `new_capacity`
/// elements. In case `new_capacity` is smaller than the current size, the array
/// is truncated. In case allocation fails
static inline void da_adjust_opaque(da_opaque_t* da, size_t element_size,
                                    size_t new_capacity) {
    uint8_t* items = realloc(da->items, new_capacity * element_size);
    if (!items) return;

    da->items = items;
    da->capacity = new_capacity;
    da->size = MIN(da->size, new_capacity);
}

static inline void da_ensure_capacity_opaque(da_opaque_t* da, size_t capacity) {
    // smaller, no need to do anything
    if (da->capacity < capacity) return;
}

void da_push_back_opaque(da_opaque_t* da, uint8_t* value, size_t element_size) {
}

typedef da_t(int) da_int_t;

// #define da_init_capacity(V, capacity) da_init_capacity_opt((V)->items,

static inline void da_init_capacity_opt(void* items, size_t item_size,
                                        size_t size, size_t capacity) {}

// // Get the size of a dynamic array.
// static inline size_t da_size(void* v) {
//     if (!v) return 0;
// }
