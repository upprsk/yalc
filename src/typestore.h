#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "allocator.h"
#include "da.h"

typedef struct type_id {
    uint32_t id;
} type_id_t;

typedef enum type_tag {
    TYPE_ERR,
    TYPE_VOID,
    TYPE_TYPE,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_PTR,
    TYPE_MPTR,
    TYPE_PROC,
} type_tag_t;

static inline char const* type_tag_to_str(type_tag_t tag) {
    switch (tag) {
        case TYPE_ERR: return "TYPE_ERR";
        case TYPE_VOID: return "TYPE_VOID";
        case TYPE_TYPE: return "TYPE_TYPE";
        case TYPE_INT: return "TYPE_INT";
        case TYPE_FLOAT: return "TYPE_FLOAT";
        case TYPE_PTR: return "TYPE_PTR";
        case TYPE_MPTR: return "TYPE_MPTR";
        case TYPE_PROC: return "TYPE_PROC";
    }

    return "?";
}

typedef struct type_int {
    uint16_t bits;
    bool     signed_;
} type_int_t;

typedef struct type_float {
    uint16_t bits;
} type_float_t;

typedef struct type_ptr {
    type_id_t inner;
} type_ptr_t;

// TODO: Add support for terminated multi pointers
typedef struct type_mptr {
    type_id_t inner;
} type_mptr_t;

typedef struct type_proc {
    type_id_t return_type;

    // array of argument types
    type_id_t* args;
} type_proc_t;

da_declare(type_id_t, type_id);

typedef struct type {
    type_tag_t tag;
    union {
        type_int_t   int_;
        type_float_t float_;
        type_ptr_t   ptr;
        type_mptr_t  mptr;
        type_proc_t  proc;
    } as;
} type_t;

typedef struct typestore_entry {
    type_id_t id;
    type_t    type;
} typestore_entry_t;

da_declare(typestore_entry_t, typestore_entry);

typedef struct typestore_primitives {
    type_id_t err;
    type_id_t void_;
    type_id_t type;
    type_id_t i32;
    type_id_t f32;
    type_id_t f64;
} typestore_primitives_t;

typedef struct typestore {
    // array of all types
    typestore_entry_t* entries;

    allocator_t            alloc;
    typestore_primitives_t primitives;
} typestore_t;

#define INVALID_TYPEID \
    (type_id_t) { 0 }

static inline bool type_id_is_valid(type_id_t id) { return id.id != 0; }

static inline bool type_id_eq(type_id_t lhs, type_id_t rhs) {
    return lhs.id == rhs.id;
}

void typestore_init(typestore_t* ts, allocator_t alloc);
void typestore_deinit(typestore_t* ts);

type_id_t     typestore_add_type(typestore_t* ts, type_t const* t);
type_t const* typestore_find_type(typestore_t* ts, type_id_t id);

char const* typestore_type_to_str(typestore_t* ts, allocator_t alloc,
                                  type_t const* type);

static inline char const* typestore_type_id_to_str(typestore_t* ts,
                                                   allocator_t  alloc,
                                                   type_id_t    id) {
    type_t const* type = typestore_find_type(ts, id);
    if (!type) return allocator_sprintf(alloc, "<not found>");

    return typestore_type_to_str(ts, alloc, type);
}
