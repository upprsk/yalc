#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "alloc/allocator.h"
#include "common.h"
#include "da/da.h"
#include "slice/slice.h"

typedef struct type_ref {
    uint32_t id;
} type_ref_t;

#define type_ref_valid(_ref)    (!!(_ref).id)
#define type_ref_eq(_lhs, _rhs) ((_lhs).id == (_rhs).id)

typedef enum type_kind {
    TYPE_INVAL,

    TYPE_TYPE,
    TYPE_VOID,
    TYPE_INT,
    TYPE_BOOL,
    TYPE_PTR,
    TYPE_PROC,
} type_kind_t;

static inline char const* type_kind_str(type_kind_t tk) {
    switch (tk) {
        case TYPE_INVAL: return "TYPE_INVAL";
        case TYPE_TYPE: return "TYPE_TYPE";
        case TYPE_VOID: return "TYPE_VOID";
        case TYPE_INT: return "TYPE_INT";
        case TYPE_BOOL: return "TYPE_BOOL";
        case TYPE_PTR: return "TYPE_PTR";
        case TYPE_PROC: return "TYPE_PROC";
    }

    return "?";
}

typedef struct type_int {
    uint32_t bits;
    bool     is_signed;
} type_int_t;

typedef struct type_ptr {
    uint64_t         term;
    type_ref_t       child;
    node_ptr_flags_t flags;
} type_ptr_t;

typedef struct args_arr {
    uint32_t start;
    uint32_t len;
} args_arr_t;

typedef struct type_proc {
    args_arr_t args;
    type_ref_t ret;
    bool       is_varargs;
} type_proc_t;

typedef struct type_proc_arg {
    type_ref_t type;
} type_proc_arg_t;

typedef struct type {
    type_kind_t kind;
    union {
        type_int_t  int_;
        type_ptr_t  ptr;
        type_proc_t proc;
    } as;
} type_t;

typedef da_t(type_t) da_type_t;
typedef da_t(type_proc_arg_t) da_args_t;
typedef slice_t(type_proc_arg_t) slice_args_t;

typedef struct tstore_builtins {
    type_ref_t type;
    type_ref_t void_;
    type_ref_t i8;
    type_ref_t u8;
    type_ref_t i16;
    type_ref_t u16;
    type_ref_t i32;
    type_ref_t u32;
    type_ref_t i64;
    type_ref_t u64;
    type_ref_t bool_;
} tstore_builtins_t;

typedef struct tstore {
    da_type_t         types;
    da_args_t         args;
    tstore_builtins_t builtins;
} tstore_t;

void tstore_init(tstore_t* ts, allocator_t alloc);

static inline void tstore_deinit(tstore_t* ts) {
    da_free(&ts->types);
    da_free(&ts->args);
}

type_ref_t tstore_find(tstore_t const* ts, type_t const* t);

type_ref_t tstore_add(tstore_t* ts, type_t const* t);

static inline type_ref_t tstore_add_int(tstore_t* ts, bool is_signed,
                                        uint32_t bits) {
    return tstore_add(ts,
                      &(type_t){
                          .kind = TYPE_INT,
                          .as.int_ = {.is_signed = is_signed, .bits = bits}
    });
}

static inline type_ref_t tstore_add_ptr(tstore_t* ts, node_ptr_flags_t flags,
                                        type_ref_t child, uint64_t term) {
    return tstore_add(
        ts, &(type_t){
                .kind = TYPE_PTR,
                .as.ptr = {.child = child, .flags = flags, .term = term}
    });
}

static inline type_ref_t tstore_add_proc(tstore_t* ts, args_arr_t args,
                                         type_ref_t ret, bool is_varars) {
    return tstore_add(
        ts,
        &(type_t){
            .kind = TYPE_PROC,
            .as.proc = {.args = args, .ret = ret, .is_varargs = is_varars}
    });
}

static inline args_arr_t tstore_add_args(tstore_t* ts, da_args_t args) {
    uint32_t idx = ts->args.size;
    da_append(&ts->args, args.size, args.items);

    return (args_arr_t){.start = idx, .len = args.size};
}

static inline slice_args_t tstore_get_args(tstore_t const* ts,
                                           args_arr_t      args) {
    slice_args_t s = da_to_slice(ts->args);
    return slice_s(s, args.start, args.start + args.len);
}

static inline type_t const* tstore_get(tstore_t const* ts, type_ref_t ref) {
    assert_uint32(ref.id, >, 0);
    assert_uint32(ref.id, <, ts->types.size);

    return &ts->types.items[ref.id];
}

static inline type_t const* tstore_maybe_get(tstore_t const* ts,
                                             type_ref_t      ref) {
    assert_uint32(ref.id, <, ts->types.size);

    return &ts->types.items[ref.id];
}

bool tstore_type_eq(tstore_t const* t, type_t const* lhs, type_t const* rhs);

static inline bool tstore_type_is_void(tstore_t const* t, type_ref_t type) {
    return type_ref_eq(t->builtins.void_, type);
}

static inline bool tstore_type_is_type(tstore_t const* t, type_ref_t type) {
    return type_ref_eq(t->builtins.type, type);
}

static inline bool tstore_type_is_bool(tstore_t const* t, type_ref_t type) {
    return type_ref_eq(t->builtins.bool_, type);
}

static inline bool tstore_type_is_proc(tstore_t const* t, type_ref_t type) {
    return type_ref_valid(type) && tstore_get(t, type)->kind == TYPE_PROC;
}

static inline type_ref_t tstore_get_ret(tstore_t const* ts, type_ref_t type) {
    type_t const* ty = tstore_get(ts, type);
    assert_uint(ty->kind, ==, TYPE_PROC);

    return ty->as.proc.ret;
}

static inline type_proc_t const* type_as_proc(type_t const* t) {
    assert_uint(t->kind, ==, TYPE_PROC);
    return &t->as.proc;
}

string_t tstore_type_str(tstore_t const* t, type_t const* type,
                         allocator_t alloc);
string_t tstore_type_ref_str(tstore_t const* t, type_ref_t type,
                             allocator_t alloc);
