#pragma once

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "alloc/allocator.h"

/// The initial capacity of an empty dynamic array.
#ifndef DA_INITIAL_CAPACITY
#define DA_INITIAL_CAPACITY 8
#endif  // !DA_INITIAL_CAPACITY

/// Define the type of a dynamic array.
///
/// For each new array type you want to add, write:
///
/// ```c
/// typedef da_t(int) da_int_t;
/// ```
///
/// The structure stores a pointer to the items, size, capacity and an
/// allocator. The allocator adds an overhead of 8 bytes to the array, but that
/// should be fine.
///
/// > Some types are already pre-defined, and some (like string_t) have
/// > additional functionality.
#define da_t(T)               \
    struct {                  \
        T*          items;    \
        uint32_t    size;     \
        uint32_t    capacity; \
        allocator_t alloc;    \
    }

/// Represent possible results of modifying a dynamic array. Note that these are
/// only use when a reallocation might happen.
typedef enum da_sts {
    DA_OK,
    DA_OOM,
    DA_EMPTY,
} da_sts_t;

/// Initialize a dynamic array with the given allocator, it is just sugar for:
///
/// ```c
/// da_int_t arr = { .alloc = alloc };
/// ```
#define da_init(_alloc) \
    { .alloc = _alloc }

/// Index into the dynamic array, but first do a bounds check.
///
/// `_idx` is only used once in the macro, so it may include side effects.
#define da_at(_da, _idx)                   \
    ({                                     \
        typeof((_da).size) idx = _idx;     \
        assert_not_null((_da).items);      \
        assert_uint32(idx, <, (_da).size); \
        (_da).items[idx];                  \
    })

/// Index into the dynamic array and get a poiter to the element, but first do a
/// bounds check.
///
/// `_idx` is only used once in the macro, so it may include side effects.
#define da_ptr_at(_da, _idx)               \
    ({                                     \
        typeof((_da).size) idx = _idx;     \
        assert_not_null((_da).items);      \
        assert_uint32(idx, <, (_da).size); \
        &(_da).items[idx];                 \
    })

/// Same as `da_reserve_opt`, but asserts that the allocation was succesful.
#define da_reserve(_da, _cap) assert_int(da_reserve_opt(_da, _cap), ==, DA_OK)

/// Reserve capacity for further operations. Note that this not set capacity
/// exactly to _cap, but instead grows the current capacity by doubling it until
/// it reaches the desired ammount. This also means that the capacity will not
/// be reduces if a smaller capacity is given.
///
/// This returns DA_OOM if the allocation fails.
#define da_reserve_opt(_da, _cap) \
    da_ensure_capacity(da_spread(_da), sizeof(*(_da)->items), _cap)

/// Reserve an exact ammount of capacity for the dynamic array. If the new
/// capacity is smaller than the current size, the array is truncated. Assert
/// that allocation does not fail.
#define da_reserve_exact(_da, _cap) \
    assert_int(da_reserve_exact_opt(_da, _cap), ==, DA_OK)

/// Reserve an exact ammount of capacity for the dynamic array. If the new
/// capacity is smaller than the current size, the array is truncated.
///
/// This returns DA_OOM if the allocation fails.
#define da_reserve_exact_opt(_da, _cap) \
    da_set_capacity(da_spread(_da), sizeof(*(_da)->items), _cap)

/// Resize the dynamic array to have `_size` elements. If `_size` is larger than
/// the current capacity, then capacity is expanded to exactly `_size`. If
/// `_size` is smaller than the current capacity, no allocation is performed.
/// Asserts that the allocation was succesfull.
#define da_resize(_da, _size) assert_int(da_resize_opt(_da, _size), ==, DA_OK)

/// Resize the dynamic array to have `_size` elements. If `_size` is larger than
/// the current capacity, then capacity is expanded to exactly `_size`. If
/// `_size` is smaller than the current capacity, no allocation is performed. In
/// case allocation fails returns DA_OOM.
#define da_resize_opt(_da, _size) \
    da_resize_to(da_spread(_da), sizeof(*(_da)->items), _size)

/// Push a value to the end of the dynamic array. Asserts that the allocation
/// does not fail.
#define da_push_back(_da, ...) \
    assert_int(da_push_back_opt(_da, __VA_ARGS__), ==, DA_OK)

/// Push a value to the end of the dynamic array and return if it was succesful.
/// In case allocation fails, returns DA_OOM.
#define da_push_back_opt(_da, ...)                                            \
    ({                                                                        \
        typeof((_da)->size) idx = (_da)->size;                                \
        da_sts_t sts = da_ensure_push(da_spread(_da), sizeof(*(_da)->items)); \
        if (!sts) {                                                           \
            assert_not_null((_da)->items);                                    \
            (_da)->items[idx] = __VA_ARGS__;                                  \
        }                                                                     \
        sts;                                                                  \
    })

/// Append `_count` elements to the end of the dynamic array. Asserts that the
/// allocation does not fail.
#define da_append(_da, _count, ...) \
    assert_int(da_append_opt(_da, _count, __VA_ARGS__), ==, DA_OK)

/// Append `_count` elements to the end of the dynamic array and return if it
/// was succesful. In case allocation fails, returns DA_OOM.
#define da_append_opt(_da, _count, ...)                           \
    ({                                                            \
        da_sts_t sts = da_reserve_opt(_da, (_da)->size + _count); \
        if (!sts) {                                               \
            memcpy(&(_da)->items[(_da)->size], __VA_ARGS__,       \
                   _count * sizeof(*(_da)->items));               \
            (_da)->size += _count;                                \
        }                                                         \
        sts;                                                      \
    })

/// Pop from the end of the array. If there was no item to pop, returns
/// DA_EMPTY.
#define da_pop_opt(_da, _recv)                      \
    ({                                              \
        da_sts_t sts = DA_EMPTY;                    \
        if ((_da)->size) {                          \
            *(_recv) = (_da)->items[--(_da)->size]; \
            sts = DA_OK;                            \
        }                                           \
        sts;                                        \
    })

/// Pop from the end of the array. Asserts that the array is not empty.
#define da_pop(_da)                       \
    ({                                    \
        assert_uint32((_da)->size, >, 0); \
        (_da)->items[--(_da)->size];      \
    })

/// Set the size of the array to zero, but does not deallocate used memory.
#define da_clear(_da)    \
    do {                 \
        (_da)->size = 0; \
    } while (0)

/// free all of the memory used by the array and reset it.
#define da_free(_da)                                \
    do {                                            \
        if ((_da)->alloc.alloc) allocator_free((_da)->alloc, (_da)->items); \
        (_da)->size = 0;                            \
        (_da)->capacity = 0;                        \
        (_da)->items = NULL;                        \
    } while (0)

/// Helper to iterate an array.
#define da_foreach(_da, _i) for (uint32_t _i = 0; _i < (_da)->size; (_i)++)

/// Convert the dynamic array to a slice. Note that this does not reduce the
/// capacity to size (that would be a potential allocation) or create a copy.
/// The returned slice is a view over the array.
///
/// ```c
/// str_t s = da_to_slice(da);
/// ```
#define da_to_slice(_da) \
    { .ptr = (_da).items, .len = (_da).size }

/// Same as `da_to_slice` but with an explicit type.
#define da_to_slice_of(_da, T) (T) da_to_slice(_da)

/// Spread all of the fields of the dynamic array to be used by implementation
/// functions.
#define da_spread(_da) \
    (uint8_t**)&(_da)->items, &(_da)->size, &(_da)->capacity, (_da)->alloc

/// Ensure that the dynamic array has space for at least one more item. In case
/// it does not, allocate more space (doubling capacity each time). In case
/// allocation fails, returns DA_OOM.
da_sts_t da_ensure_push(uint8_t** items, uint32_t* size, uint32_t* capacity,
                        allocator_t alloc, size_t item_size);

/// Ensure that the dynamic array has space for at least extra_capacity items.
/// In case it does not, allocate more space (doubling capacity each time). In
/// case allocation fails, returns DA_OOM.
da_sts_t da_ensure_capacity(uint8_t** items, uint32_t* size, uint32_t* capacity,
                            allocator_t alloc, size_t item_size,
                            uint32_t desired_capacity);

/// Set the capacity to the wanted ammount, exactly. In case allocation fails,
/// returns DA_OOM.
da_sts_t da_set_capacity(uint8_t** items, uint32_t* size, uint32_t* capacity,
                         allocator_t alloc, size_t item_size,
                         uint32_t desired_capacity);

/// Resize to the wanted ammount. If the new size is larger than the capacity,
/// it is expanded to exactly the desired size. In case the new size is smaller
/// than the current capacity, no allocation is performed. When allocation
/// fails, returns DA_OOM.
da_sts_t da_resize_to(uint8_t** items, uint32_t* size, uint32_t* capacity,
                      allocator_t alloc, size_t item_size,
                      uint32_t desired_size);

typedef da_t(int) da_int_t;
typedef da_t(size_t) da_size_t;
typedef da_t(uint8_t) da_u8_t;
typedef da_t(uint16_t) da_u16_t;
typedef da_t(uint32_t) da_u32_t;
typedef da_t(uint64_t) da_u64_t;

typedef da_t(char) string_t;

/// Print into a dynamically allocated string. The string will be allocated with
/// exactly the needed size. The string needs to be deallocated with either
/// `da_free` or `string_free` In case allocation fails, an empty string is
/// returned. There is a null-terminator at the end of the string, and it **is
/// included** in it's size.
__attribute__((format(printf, 2, 3))) string_t da_sprintf(allocator_t alloc,
                                                          char const* fmt, ...);
string_t da_vsprintf(allocator_t alloc, char const* fmt, va_list args);
