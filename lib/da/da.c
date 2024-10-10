#include "da.h"

#include <stdbool.h>
#include <stdio.h>

#include "alloc/allocator.h"
#include "munit.h"
#include "utils/minmax.h"

da_sts_t da_ensure_push(uint8_t** items, uint32_t* size, uint32_t* capacity,
                        allocator_t alloc, size_t item_size) {
    // it fits, increment size and return
    if (*size < *capacity) {
        *size += 1;
        return DA_OK;
    }

    // double capacity each time
    uint32_t new_capacity = *capacity ? (*capacity << 1) : DA_INITIAL_CAPACITY;

    // expand the memory
    uint8_t* new_items =
        allocator_realloc(alloc, *items, new_capacity * item_size);
    if (!new_items) return DA_OOM;

    // update parameters
    *items = new_items;
    *size += 1;
    *capacity = new_capacity;

    return DA_OK;
}

da_sts_t da_ensure_capacity(uint8_t** items, uint32_t* size, uint32_t* capacity,
                            allocator_t alloc, size_t item_size,
                            uint32_t desired_capacity) {
    (void)size;

    // it fits, nothing to do
    if (desired_capacity <= *capacity) return DA_OK;

    // double capacity until it fits
    uint32_t new_capacity = *capacity ? (*capacity << 1) : DA_INITIAL_CAPACITY;
    while (new_capacity < desired_capacity) new_capacity <<= 1;

    // expand the memory
    uint8_t* new_items =
        allocator_realloc(alloc, *items, new_capacity * item_size);
    if (!new_items) return DA_OOM;

    // update parameters (size does not change)
    *items = new_items;
    *capacity = new_capacity;

    return DA_OK;
}

da_sts_t da_set_capacity(uint8_t** items, uint32_t* size, uint32_t* capacity,
                         allocator_t alloc, size_t item_size,
                         uint32_t desired_capacity) {
    // expand or shrink the memory
    uint8_t* new_items =
        allocator_realloc(alloc, *items, desired_capacity * item_size);
    if (!new_items) return DA_OOM;

    // update parameters
    *items = new_items;
    *capacity = desired_capacity;
    *size = MIN(*size, desired_capacity);

    return DA_OK;
}

da_sts_t da_resize_to(uint8_t** items, uint32_t* size, uint32_t* capacity,
                      allocator_t alloc, size_t item_size,
                      uint32_t desired_size) {
    // Smaller than capacity, adjust and return
    if (desired_size < *capacity) {
        *size = desired_size;
        return DA_OK;
    }

    // expand or shrink the memory
    uint8_t* new_items =
        allocator_realloc(alloc, *items, desired_size * item_size);
    if (!new_items) return DA_OOM;

    // update parameters
    *items = new_items;
    *capacity = desired_size;
    *size = desired_size;

    return DA_OK;
}

string_t da_sprintf(allocator_t alloc, char const* fmt, ...) {
    va_list args;
    va_start(args, fmt);

    string_t s = da_vsprintf(alloc, fmt, args);

    va_end(args);

    return s;
}

string_t da_vsprintf(allocator_t alloc, char const* fmt, va_list args) {
    va_list cpy;
    va_copy(cpy, args);

    int len = vsnprintf(NULL, 0, fmt, args);

    string_t s = da_init(alloc);
    da_resize(&s, len + 1);

    vsnprintf(s.items, s.size, fmt, cpy);

    // remove the null terminator from the size
    da_pop(&s);

    va_end(cpy);

    return s;
}

__attribute__((format(printf, 2, 3))) void da_catfmt(string_t*   s,
                                                         char const* fmt, ...) {
    va_list args;
    va_start(args, fmt);

    da_vcatfmt(s, fmt, args);

    va_end(args);
}

void da_vcatfmt(string_t* s, char const* fmt, va_list args) {
    va_list cpy;
    va_copy(cpy, args);

    int len = vsnprintf(NULL, 0, fmt, args);
    da_reserve(s, s->size + len + 1);

    vsnprintf(s->items + s->size, len + 1, fmt, cpy);
    s->size += len;  // manually increment the size

    va_end(cpy);
}

void da_strcat(string_t* lhs, string_t const* rhs) {
    da_append(lhs, rhs->size, rhs->items);

    // add null terminator, but not include it in the size
    da_push_back(lhs, 0);
    da_pop(lhs);
}

void da_strcat_and_free(string_t* lhs, string_t* rhs) {
    da_strcat(lhs, rhs);
    da_free(rhs);
}

void da_strjoin(string_t* lhs, string_t const* rhs, string_t const* sep) {
    da_strcat(lhs, sep);
    da_strcat(lhs, rhs);
}

void da_strjoin_and_free(string_t* lhs, string_t* rhs, string_t* sep) {
    da_strjoin(lhs, rhs, sep);
    da_free(rhs);
    da_free(sep);
}
