#pragma once

#include <stddef.h>

/// Define the slice type.
///
/// ```c
/// typedef slice_t(char) slice_char_t;
/// typedef slice_t(char const) str_t;
/// ```
#define slice_t(T)  \
    struct {        \
        T*     ptr; \
        size_t len; \
    }

/// Get the item at the specified index, but with bounds checking.
#define slice_at(_s, _at)             \
    ({                                \
        typeof((_s).len) at = _at;    \
        assert_not_null((_s).ptr);    \
        assert_size(at, <, (_s).len); \
        (_s).ptr[at];                 \
    })

/// Slice the slice from [_begin, _end). Does bounds checking.
#define slice_s(_s, _begin, _end)                                  \
    ({                                                             \
        typeof((_s).len) begin = _begin;                           \
        typeof((_s).len) end = _end;                               \
        assert_size(begin, <=, end);                               \
        assert_size(begin, <=, (_s).len);                          \
        assert_size(end, <=, (_s).len);                            \
        assert_not_null((_s).ptr);                                 \
        (typeof(_s)){.ptr = (_s).ptr + begin, .len = end - begin}; \
    })

/// Check if two slices are the same. Note that this uses memcmp and as such may
/// fail in some scenarios (like when using unions).
#define slice_eq(_lhs, ...)               \
    (((_lhs).len == (__VA_ARGS__).len) && \
     (memcmp((_lhs).ptr, (__VA_ARGS__).ptr, (_lhs).len) == 0))

/// In case the slice was dinamically allocated, use this to free it. The fields
/// will also be reset to an empty slice.
#define slice_free(_alloc, _s)                   \
    do {                                         \
        allocator_free(_alloc, (void*)(_s).ptr); \
        (_s).ptr = NULL;                         \
        (_s).len = 0;                            \
    } while (0)

/// Create a slice from a string literal:
///
/// ```c
/// str_t s = str_from_lit("hello!");
/// ```
#define str_from_lit(_s) \
    (str_t) { _s, sizeof(_s) - 1 }

typedef slice_t(char) slice_char_t;
typedef slice_t(char const) str_t;
