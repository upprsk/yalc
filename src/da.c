#include "da.h"

#include <stdint.h>
#include <string.h>

#include "allocator.h"
#include "span.h"

static da_header_t* allocate_header_with_capacity(allocator_t alloc,
                                                  uint32_t    elem_size,
                                                  uint32_t    capacity) {
    da_header_t* h =
        allocator_alloc(alloc, sizeof(da_header_t) + elem_size * capacity);
    *h = (da_header_t){
        .element_size = elem_size,
        .capacity = capacity,
        .size = 0,
    };

    return h;
}

static da_header_t* reallocate_header_with_capacity(allocator_t  alloc,
                                                    da_header_t* header) {
    munit_assert_uint32(header->capacity, !=, 0);

    uint32_t     new_capacity = header->capacity * 2;
    da_header_t* h = allocator_realloc(
        alloc, header,
        sizeof(da_header_t) + header->element_size * header->capacity,
        sizeof(da_header_t) + header->element_size * new_capacity);
    munit_assert_not_null(h);

    h->capacity = new_capacity;
    return h;
}

static da_header_t* reallocate_header_with_capacity_of_at_least(
    allocator_t alloc, da_header_t* header, size_t desired) {
    munit_assert_uint32(header->capacity, !=, 0);

    uint32_t new_capacity = header->capacity * 2;
    while (new_capacity < desired) new_capacity *= 2;

    da_header_t* h = allocator_realloc(
        alloc, header,
        sizeof(da_header_t) + header->element_size * header->capacity,
        sizeof(da_header_t) + header->element_size * new_capacity);
    munit_assert_not_null(h);

    h->capacity = new_capacity;
    return h;
}

void* da_init_capacity(allocator_t alloc, uint32_t elem_size,
                       uint32_t capacity) {
    // This is an init method, so the array is empty, initialize it with the
    // given capacity
    da_header_t* header =
        allocate_header_with_capacity(alloc, elem_size, capacity);

    return header->data;
}

void* da_append_opt(void* arr, allocator_t alloc, void const* value) {
    da_header_t* header = da_get_header(arr);
    munit_assert_not_null(header);

    if (header->size >= header->capacity) {
        header = reallocate_header_with_capacity(alloc, header);
    }

    size_t off = header->element_size * header->size;

    memcpy(&header->data[off], value, header->element_size);

    header->size += 1;

    return header->data;
}

void* da_extend_opt(void* arr, allocator_t alloc, void const* value,
                    size_t count) {
    da_header_t* header = da_get_header(arr);
    munit_assert_not_null(header);

    if (header->size + count >= header->capacity) {
        header = reallocate_header_with_capacity_of_at_least(
            alloc, header, header->size + count);
    }

    size_t off = header->element_size * header->size;
    memcpy(&header->data[off], value, header->element_size * count);

    header->size += count;

    return header->data;
}
