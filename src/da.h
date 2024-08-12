#pragma once

#include <stddef.h>
#include <stdint.h>

#include "allocator.h"

#define DA_DEFAULT_CAPACITY 8

typedef struct da_header {
    uint32_t size;
    uint32_t capacity;
    uint32_t element_size;
    uint32_t _padding;
    uint8_t  data[];
} da_header_t;

/// get the header in front of the dynamic array. If `arr` is null, returns
/// null.
static inline da_header_t* da_get_header(void* arr) {
    if (!arr) return NULL;

    da_header_t* da = arr;
    return &da[-1];
}

static inline uint32_t da_get_size(void* arr) {
    if (!arr) return 0;

    return da_get_header(arr)->size;
}

static inline uint32_t da_get_element_size(void* arr) {
    if (!arr) return 0;

    return da_get_header(arr)->element_size;
}

/// Initialize a dynamic array with a given capacity.
void* da_init_capacity(allocator_t alloc, uint32_t elem_size,
                       uint32_t capacity);

/// Initialize a dynamic array with the default capacity.
static inline void* da_init_default(allocator_t alloc, uint32_t elem_size) {
    return da_init_capacity(alloc, elem_size, DA_DEFAULT_CAPACITY);
}

static inline void* da_free(void* arr, allocator_t alloc) {
    allocator_free(alloc, da_get_header(arr));

    return NULL;
}

/// Append to a dynamic array.
void* da_append_opt(void* arr, allocator_t alloc, void* value);

// T* da_init(T* ptr, allocator_t alloc)
#define da_init(T, alloc_) (T*)da_init_default(alloc_, sizeof(T))

// T* da_append(T* ptr, allocator_t alloc, T const* value)
#define da_append(ptr_, alloc_, value_) \
    (typeof(ptr_))da_append_opt(ptr_, alloc_, (typeof(ptr_))(value_))
