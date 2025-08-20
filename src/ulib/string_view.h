#pragma once

#include <assert.h>
#include <stddef.h>
#include <string.h>

// A string view. This provides a constant view over a string as a fat pointer.
//
// In order to use this with traditional printf, use the following syntax:
//
//     string_view s = string_view_from_cstr("test");
//     printf("%.*s", (int)s.len, s.ptr);
typedef struct {
    char const *ptr;
    size_t      len;
} string_view;

// Create a string_view from a null-terminated c-string.
static inline string_view string_view_from_cstr(char const *ptr) {
    return (string_view){.ptr = ptr, .len = strlen(ptr)};
}

// Create a substring, from `[start, end[`.
//
// - Asserts that `start <= end`
// - Asserts that `end <= s.len`
static inline string_view substr(string_view s, size_t start, size_t end) {
    assert(start < end);
    assert(end <= s.len);

    return (string_view){.ptr = s.ptr + start, .len = end - start};
}

// Checks that two strings are equal.
static inline bool string_view_eq(string_view a, string_view b) {
    return a.len == b.len && strncmp(a.ptr, b.ptr, a.len) == 0;
}

// Checks that a string starts with a given prefix.
static inline bool string_view_starts_with(string_view s, string_view prefix) {
    if (s.len < prefix.len) return false;

    return string_view_eq(substr(s, 0, prefix.len), prefix);
}

// Checks that a string ends with a given prefix.
static inline bool string_view_ends_with(string_view s, string_view suffix) {
    if (s.len < suffix.len) return false;

    return string_view_eq(substr(s, s.len - suffix.len, s.len), suffix);
}
