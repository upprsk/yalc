#include "tstore.h"

#include <stdbool.h>

#include "alloc/allocator.h"
#include "common.h"
#include "da/da.h"
#include "slice/slice.h"

static bool proc_args_eq(tstore_t const* t, type_proc_t const* lhs,
                         type_proc_t const* rhs) {
    if (lhs->args.len != rhs->args.len) return false;

    slice_args_t lhs_args = tstore_get_args(t, lhs->args);
    slice_args_t rhs_args = tstore_get_args(t, rhs->args);
    assert_size(lhs_args.len, ==, rhs_args.len);

    for (size_t i = 0; i < lhs_args.len; i++) {
        if (!type_ref_eq(lhs_args.ptr[i].type, rhs_args.ptr[i].type))
            return false;
    }

    return true;
}

void tstore_init(tstore_t* ts, allocator_t alloc) {
    assert_not_null(ts);

    *ts = (tstore_t){.types = da_init(alloc), .args = da_init(alloc)};

    // push a dummy node to occupy index zero
    da_push_back(&ts->types, (type_t){});

    ts->builtins.type = tstore_add(ts, &(type_t){.kind = TYPE_TYPE});
    ts->builtins.void_ = tstore_add(ts, &(type_t){.kind = TYPE_VOID});
    ts->builtins.i8 = tstore_add_int(ts, true, 8);
    ts->builtins.u8 = tstore_add_int(ts, false, 8);
    ts->builtins.i16 = tstore_add_int(ts, true, 16);
    ts->builtins.u16 = tstore_add_int(ts, false, 16);
    ts->builtins.i32 = tstore_add_int(ts, true, 32);
    ts->builtins.u32 = tstore_add_int(ts, false, 32);
    ts->builtins.i64 = tstore_add_int(ts, true, 64);
    ts->builtins.u64 = tstore_add_int(ts, false, 64);
}

type_ref_t tstore_find(tstore_t const* ts, type_t const* t) {
    da_foreach(&ts->types, i) {
        if (tstore_type_eq(ts, da_ptr_at(ts->types, i), t))
            return (type_ref_t){i};
    }

    return (type_ref_t){};
}

type_ref_t tstore_add(tstore_t* ts, type_t const* t) {
    type_ref_t p = tstore_find(ts, t);
    if (type_ref_valid(p)) return p;

    uint32_t idx = ts->types.size;
    da_push_back(&ts->types, *t);

    return (type_ref_t){idx};
}

bool tstore_type_eq(tstore_t const* t, type_t const* lhs, type_t const* rhs) {
    assert_not_null(t);
    assert_not_null(lhs);
    assert_not_null(rhs);

    if (lhs->kind != rhs->kind) return false;

    switch (lhs->kind) {
        case TYPE_INVAL: return true;
        case TYPE_TYPE:
        case TYPE_VOID: return true;
        case TYPE_INT:
            return lhs->as.int_.bits == rhs->as.int_.bits &&
                   rhs->as.int_.is_signed == lhs->as.int_.is_signed;
        case TYPE_PTR:
            return lhs->as.ptr.flags == rhs->as.ptr.flags &&
                   type_ref_eq(lhs->as.ptr.child, rhs->as.ptr.child);
        case TYPE_PROC:
            return lhs->as.proc.is_varargs == rhs->as.proc.is_varargs &&
                   type_ref_eq(lhs->as.proc.ret, rhs->as.proc.ret) &&
                   proc_args_eq(t, &lhs->as.proc, &rhs->as.proc);
    }

    // this should never be reached
    assert(false);
}

string_t tstore_type_ref_str(tstore_t const* t, type_ref_t type,
                             allocator_t alloc) {
    return tstore_type_str(t, tstore_maybe_get(t, type), alloc);
}

string_t tstore_type_str(tstore_t const* ts, type_t const* type,
                         allocator_t alloc) {
    switch (type->kind) {
        case TYPE_INVAL: return da_sprintf(alloc, "<inval>");
        case TYPE_TYPE: return da_sprintf(alloc, "type");
        case TYPE_VOID: return da_sprintf(alloc, "void");
        case TYPE_INT:
            return da_sprintf(alloc, "%c%d",
                              type->as.int_.is_signed ? 'i' : 'u',
                              type->as.int_.bits);
        case TYPE_PTR: {
            string_t c = tstore_type_ref_str(ts, type->as.ptr.child, alloc);

            char const* const_ =
                ptr_is_const(type->as.ptr.flags) ? "const " : "";

            string_t s;
            if (ptr_is_multi(type->as.ptr.flags)) {
                if (ptr_has_term(type->as.ptr.flags)) {
                    s = da_sprintf(alloc, "[*:%ld]%s%s", type->as.ptr.term,
                                   const_, c.items);
                } else {
                    s = da_sprintf(alloc, "[*]%s%s", const_, c.items);
                }
            } else if (ptr_is_slice(type->as.ptr.flags)) {
                if (ptr_has_term(type->as.ptr.flags)) {
                    s = da_sprintf(alloc, "[:%ld]%s%s", type->as.ptr.term,
                                   const_, c.items);
                } else {
                    s = da_sprintf(alloc, "[]%s%s", const_, c.items);
                }
            } else {
                s = da_sprintf(alloc, "*%s%s", const_, c.items);
            }

            da_free(&c);
            return s;
        }
        case TYPE_PROC: {
            slice_args_t args = tstore_get_args(ts, type->as.proc.args);

            string_t a = (string_t){.items = ""};
            slice_foreach(args, i) {
                string_t c =
                    tstore_type_ref_str(ts, slice_at(args, i).type, alloc);
                string_t tmp = da_sprintf(alloc, "%s%zu: %s, ", a.items, i, c.items);

                da_free(&a);
                da_free(&c);

                a = tmp;
            }

            string_t r = tstore_type_ref_str(ts, type->as.proc.ret, alloc);
            string_t s = da_sprintf(alloc, ".(%s) %s ...", a.items, r.items);

            da_free(&a);
            da_free(&r);
            return s;
        }
    }

    assert(false);
}
