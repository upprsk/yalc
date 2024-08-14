#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "allocator.h"

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

typedef struct type {
    type_tag_t tag;
    union {
        type_int_t   int_;
        type_float_t float_;
        type_ptr_t   ptr;
        type_mptr_t  mptr;
        type_proc_t  rett;
    } as;
} type_t;

typedef struct typestore_entry {
    type_id_t id;
    type_t    type;
} typestore_entry_t;

typedef struct typestore_primitives {
    type_id_t void_;
    type_id_t type;
    type_id_t i32;
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

void typestore_init(typestore_t* ts, allocator_t alloc);
void typestore_deinit(typestore_t* ts);

type_id_t typestore_add_type(typestore_t* ts, type_t const* t);
