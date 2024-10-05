#pragma once

#include <stddef.h>

#include "alloc/allocator.h"
#include "munit.h"

// clang-format off

#if defined(__clang__)
#define disable_warnings                                            \
    _Pragma("GCC diagnostic push");                                 \
    _Pragma("GCC diagnostic ignored \"-Wincompatible-pointer-types-discards-qualifiers\"");

#define enable_warnings _Pragma("GCC diagnostic pop");

#elif defined(__GNUC__) || defined(__GNUG__)
#define disable_warnings                                            \
    _Pragma("GCC diagnostic push");                                 \
    _Pragma("GCC diagnostic ignored \"-Wdiscarded-qualifiers\"");

#define enable_warnings _Pragma("GCC diagnostic pop");

#endif

// clang-format on

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

/// Helper for iterating slices
#define slice_foreach(_s, _i) \
    for (typeof((_s).len) _i = 0; _i < (_s).len; (_i)++)

/// In case the slice was dinamically allocated, use this to free it. The fields
/// will also be reset to an empty slice.
#define slice_free(_alloc, _s)                    \
    do {                                          \
        allocator_free(_alloc, (void*)(_s)->ptr); \
        (_s)->ptr = NULL;                         \
        (_s)->len = 0;                            \
    } while (0)

/// Move the pointer one element forward and reduce the length by one. Asserst
/// that the slice is not empty. Returns the removed element.
#define slice_shift(_s)                         \
    ({                                          \
        assert_not_null((_s)->ptr);             \
        assert_size((_s)->len, >, 0);           \
        typeof(*(_s)->ptr) item = (_s)->ptr[0]; \
        (_s)->ptr++;                            \
        (_s)->len--;                            \
        item;                                   \
    })

#define slice_dupe(_alloc, _s)                                     \
    ({                                                             \
        typeof((_s).ptr) duped =                                   \
            allocator_alloc(_alloc, sizeof(*(_s).ptr) * (_s).len); \
        assert_not_null(duped);                                    \
        disable_warnings;                                          \
        memcpy(duped, (_s).ptr, (_s).len * sizeof(*(_s).ptr));     \
        enable_warnings;                                           \
        (typeof(_s)){duped, (_s).len};                             \
    })

/// Create a slice from a string literal:
///
/// ```c
/// str_t s = str_from_lit("hello!");
/// ```
#define str_from_lit(_s) \
    (str_t) { _s, sizeof(_s) - 1 }

typedef slice_t(char) slice_char_t;
typedef slice_t(int) slice_int_t;
typedef slice_t(char const) str_t;

static inline str_t str_dupe(allocator_t alloc, str_t s) {
    char* duped = allocator_alloc(alloc, sizeof(*s.ptr) * (s.len + 1));
    assert_not_null(duped);

    memcpy(duped, s.ptr, s.len * sizeof(*s.ptr));
    duped[s.len] = 0;

    return (typeof(s)){duped, s.len};
}
