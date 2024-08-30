#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "allocator.h"
#include "common.h"
#include "da.h"

typedef struct type_id {
    uint32_t id;
} type_id_t;

#define INVALID_TYPEID \
    (type_id_t) { 0 }

static inline bool type_id_is_valid(type_id_t id) {
    return id.id != 0 && id.id != 0xFF;
}

static inline bool type_id_eq(type_id_t lhs, type_id_t rhs) {
    return lhs.id == rhs.id;
}

typedef enum type_tag : uint8_t {
    TYPE_ERR,
    TYPE_VOID,
    TYPE_TYPE,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
    TYPE_ARRAY,
    TYPE_PTR,
    TYPE_MPTR,
    TYPE_PROC,
    TYPE_KW,
    TYPE_RECORD,
    TYPE_PLACEHOLDER,
} type_tag_t;

static inline char const* type_tag_to_str(type_tag_t tag) {
    switch (tag) {
        case TYPE_ERR: return "TYPE_ERR";
        case TYPE_VOID: return "TYPE_VOID";
        case TYPE_TYPE: return "TYPE_TYPE";
        case TYPE_INT: return "TYPE_INT";
        case TYPE_FLOAT: return "TYPE_FLOAT";
        case TYPE_BOOL: return "TYPE_BOOL";
        case TYPE_ARRAY: return "TYPE_ARRAY";
        case TYPE_PTR: return "TYPE_PTR";
        case TYPE_MPTR: return "TYPE_MPTR";
        case TYPE_PROC: return "TYPE_PROC";
        case TYPE_KW: return "TYPE_KW";
        case TYPE_RECORD: return "TYPE_RECORD";
        case TYPE_PLACEHOLDER: return "TYPE_PLACEHOLDER";
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

typedef struct type_array {
    type_id_t inner;
    uint64_t  len;
} type_array_t;

typedef struct type_ptr {
    type_id_t inner;
} type_ptr_t;

typedef struct type_mptr {
    type_id_t inner;
    uint64_t  term;
    bool      has_term;
} type_mptr_t;

typedef struct type_proc {
    type_id_t return_type;
    uint32_t  generic_count;

    // array of argument types
    type_id_t* args;
    type_id_t* generic_args;

    bool is_variadic;
} type_proc_t;

typedef struct record_field {
    char const* name;
    type_id_t   type;
} record_field_t;

da_declare(record_field_t, record_field);

typedef struct type_record {
    char const*     extern_name;
    char const*     inferred_name;
    record_field_t* fields;
} type_record_t;

static inline record_field_t const* type_record_find_field(
    type_record_t const* rec, char const* name, size_t* idx) {
    size_t count = da_get_size(rec->fields);
    for (size_t i = 0; i < count; ++i) {
        if (streq(rec->fields[i].name, name)) {
            if (idx) *idx = i;
            return &rec->fields[i];
        }
    }

    return NULL;
}

typedef struct type_kw {
    char const* ident;
} type_kw_t;

da_declare(type_id_t, type_id);

typedef struct type {
    type_tag_t tag;
    union {
        type_int_t    int_;
        type_float_t  float_;
        type_array_t  array;
        type_ptr_t    ptr;
        type_mptr_t   mptr;
        type_proc_t   proc;
        type_record_t record;
        type_kw_t     kw;
    } as;
} type_t;

static inline bool type_eq(type_t const* lhs, type_t const* rhs) {
    if (lhs->tag != rhs->tag) return false;

    switch (lhs->tag) {
        case TYPE_ERR:
        case TYPE_VOID:
        case TYPE_BOOL:
        case TYPE_TYPE: return true;
        case TYPE_INT:
            return lhs->as.int_.bits == rhs->as.int_.bits &&
                   lhs->as.int_.signed_ == rhs->as.int_.signed_;
        case TYPE_FLOAT: return lhs->as.float_.bits == rhs->as.float_.bits;
        case TYPE_ARRAY:
            return lhs->as.array.len == rhs->as.array.len &&
                   type_id_eq(lhs->as.array.inner, rhs->as.array.inner);
        case TYPE_PTR: return type_id_eq(lhs->as.ptr.inner, rhs->as.ptr.inner);
        case TYPE_MPTR:
            return type_id_eq(lhs->as.mptr.inner, rhs->as.mptr.inner) &&
                   lhs->as.mptr.has_term == rhs->as.mptr.has_term &&
                   lhs->as.mptr.term == rhs->as.mptr.term;
        case TYPE_PROC: {
            if (!type_id_eq(lhs->as.proc.return_type, rhs->as.proc.return_type))
                return false;

            size_t size = da_get_size(lhs->as.proc.args);
            if (size != da_get_size(rhs->as.proc.args)) return false;

            for (size_t i = 0; i < size; ++i) {
                if (!type_id_eq(lhs->as.proc.args[i], rhs->as.proc.args[i]))
                    return false;
            }

            return true;
        } break;
        case TYPE_RECORD: {
            // WARN: For now, record types are always unique.
            return false;
        } break;
        case TYPE_KW: {
            return streq(lhs->as.kw.ident, rhs->as.kw.ident);
        } break;
        case TYPE_PLACEHOLDER: {
            // WARN: For now, placeholder types are always unique.
            return false;
        } break;
    }

    __builtin_unreachable();
}

typedef struct typestore_entry {
    type_id_t id;
    type_t    type;
} typestore_entry_t;

da_declare(typestore_entry_t, typestore_entry);

typedef struct typestore_primitives {
    type_id_t err;
    type_id_t void_;
    type_id_t type;
    type_id_t bool_;
    type_id_t i8;
    type_id_t i32;
    type_id_t f32;
    type_id_t f64;
    type_id_t str;
} typestore_primitives_t;

typedef struct typestore {
    // array of all types
    typestore_entry_t* entries;

    allocator_t            alloc;
    typestore_primitives_t primitives;
} typestore_t;

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
