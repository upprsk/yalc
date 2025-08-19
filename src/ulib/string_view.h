#pragma once

#include <assert.h>
#include <stddef.h>
#include <string.h>

typedef struct {
    char const *ptr;
    size_t      len;
} string_view;

static inline string_view string_view_from_cstr(char const *ptr) {
    return (string_view){.ptr = ptr, .len = strlen(ptr)};
}

static inline string_view substr(string_view s, size_t start, size_t end) {
    assert(start < end);
    assert(end <= s.len);

    return (string_view){.ptr = s.ptr + start, .len = end - start};
}

static inline bool string_view_eq(string_view a, string_view b) {
    return a.len == b.len && strncmp(a.ptr, b.ptr, a.len) == 0;
}

static inline bool string_view_starts_with(string_view s, string_view prefix) {
    if (s.len < prefix.len) return false;

    return string_view_eq(substr(s, 0, prefix.len), prefix);
}
