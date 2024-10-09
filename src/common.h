#pragma once

#include <stdbool.h>

typedef enum node_ptr_flags {
    PTR_MULTI = 1 << 0,
    PTR_CONST = 1 << 1,
    PTR_SLICE = 1 << 2,
    PTR_TERM = 1 << 3,
} node_ptr_flags_t;

static inline bool ptr_is_multi(node_ptr_flags_t f) { return f & PTR_MULTI; }
static inline bool ptr_is_const(node_ptr_flags_t f) { return f & PTR_CONST; }
static inline bool ptr_is_slice(node_ptr_flags_t f) { return f & PTR_SLICE; }

// this is only used for types, ast nodes use null nodes to mark this
static inline bool ptr_has_term(node_ptr_flags_t f) { return f & PTR_TERM; }
