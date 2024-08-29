#pragma once

#include <stdbool.h>
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
void* da_append_opt(void* arr, allocator_t alloc, void const* value);

void* da_extend_opt(void* arr, allocator_t alloc, void const* value,
                    size_t count);

bool da_pop_opt(void* arr, void* value);

#define da_declare(T, name)                                                \
    static inline T* da_init_##name(allocator_t alloc) {                   \
        return da_init_default(alloc, sizeof(T));                          \
    }                                                                      \
    static inline T* da_append_##name(T* arr, allocator_t alloc,           \
                                      T const* elem) {                     \
        return da_append_opt(arr, alloc, elem);                            \
    }                                                                      \
    static inline T* da_extend_##name(T* arr, allocator_t alloc,           \
                                      T const* elems, size_t elem_count) { \
        return da_extend_opt(arr, alloc, elems, elem_count);               \
    }                                                                      \
    static inline bool da_pop_##name(T* arr, T* elem) {                    \
        return da_pop_opt(arr, elem);                                      \
    }

da_declare(char, char);
da_declare(size_t, size);

#if 0

// T* da_init(T* ptr, allocator_t alloc)
#define da_init(T, alloc_) (T*)da_init_default(alloc_, sizeof(T))

// T* da_append(T* ptr, allocator_t alloc, T const* value)
#define da_append(ptr_, alloc_, ...) \
    (typeof(ptr_))da_append_opt(ptr_, alloc_, (typeof(ptr_))(__VA_ARGS__))

#endif
