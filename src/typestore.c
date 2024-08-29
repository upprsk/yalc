#include "typestore.h"

#include <stdio.h>
#include <string.h>

#include "allocator.h"
#include "da.h"
#include "span.h"

void typestore_init(typestore_t* ts, allocator_t alloc) {
    *ts = (typestore_t){
        .entries = da_init_typestore_entry(alloc),
        .alloc = alloc,
    };

    typestore_add_type(ts, &(type_t){.tag = 0xFF});
    ts->primitives.err = typestore_add_type(ts, &(type_t){.tag = TYPE_ERR});
    ts->primitives.void_ = typestore_add_type(ts, &(type_t){.tag = TYPE_VOID});
    ts->primitives.type = typestore_add_type(ts, &(type_t){.tag = TYPE_TYPE});
    ts->primitives.bool_ = typestore_add_type(ts, &(type_t){.tag = TYPE_BOOL});
    ts->primitives.i8 = typestore_add_type(
        ts, &(type_t){
                .tag = TYPE_INT, .as.int_ = {.bits = 8, .signed_ = true}
    });
    ts->primitives.i32 = typestore_add_type(
        ts, &(type_t){
                .tag = TYPE_INT, .as.int_ = {.bits = 32, .signed_ = true}
    });
    ts->primitives.f32 = typestore_add_type(
        ts, &(type_t){.tag = TYPE_FLOAT, .as.float_ = {.bits = 32}});
    ts->primitives.f64 = typestore_add_type(
        ts, &(type_t){.tag = TYPE_FLOAT, .as.float_ = {.bits = 64}});

    // TODO: Make an actual string type
    ts->primitives.str = typestore_add_type(
        ts,
        &(type_t){.tag = TYPE_MPTR, .as.mptr = {.inner = ts->primitives.i8}});
}

void typestore_deinit(typestore_t* ts) {
    munit_assert_not_null(ts);

    ts->entries = da_free(ts->entries, ts->alloc);
}

type_id_t typestore_add_type(typestore_t* ts, type_t const* t) {
    munit_assert_not_null(ts);

    size_t size = da_get_size(ts->entries);
    for (size_t i = 0; i < size; ++i) {
        if (type_eq(&ts->entries[i].type, t)) return ts->entries[i].id;
    }

    type_id_t id = {size};
    ts->entries = da_append_typestore_entry(
        ts->entries, ts->alloc, &(typestore_entry_t){.type = *t, .id = id});

    return id;
}

type_t const* typestore_find_type(typestore_t* ts, type_id_t id) {
    munit_assert_not_null(ts);

    size_t size = da_get_size(ts->entries);
    for (size_t i = 0; i < size; ++i) {
        if (type_id_eq(ts->entries[i].id, id)) return &ts->entries[i].type;
    }

    return NULL;
}

char const* typestore_type_to_str(typestore_t* ts, allocator_t alloc,
                                  type_t const* type) {
    munit_assert_not_null(ts);
    munit_assert_not_null(type);

    switch (type->tag) {
        case TYPE_ERR: return allocator_sprintf(alloc, "ERR");
        case TYPE_VOID: return allocator_sprintf(alloc, "void");
        case TYPE_TYPE: return allocator_sprintf(alloc, "type");
        case TYPE_INT: {
            int bits = type->as.int_.bits;
            return allocator_sprintf(alloc, "%c%d",
                                     type->as.int_.signed_ ? 'i' : 'u', bits);
        }
        case TYPE_FLOAT:
            return allocator_sprintf(alloc, "f%d", type->as.float_.bits);
        case TYPE_BOOL: return allocator_sprintf(alloc, "bool");
        case TYPE_ARRAY: {
            char const* inner =
                typestore_type_id_to_str(ts, alloc, type->as.array.inner);

            char const* s =
                allocator_sprintf(alloc, "[%u]%s", type->as.array.len, inner);
            allocator_free(alloc, (char*)inner);

            return s;
        }
        case TYPE_PTR: {
            char const* inner =
                typestore_type_id_to_str(ts, alloc, type->as.ptr.inner);

            char const* s = allocator_sprintf(alloc, "*%s", inner);
            allocator_free(alloc, (char*)inner);

            return s;
        }
        case TYPE_MPTR: {
            char const* inner =
                typestore_type_id_to_str(ts, alloc, type->as.ptr.inner);

            char const* s = allocator_sprintf(alloc, "[*]%s", inner);
            allocator_free(alloc, (char*)inner);

            return s;
        }
        case TYPE_PROC: {
            char* b = da_init_char(alloc);
            char  c = '(';
            b = da_append_char(b, alloc, &c);

            size_t size = da_get_size(type->as.proc.args);
            munit_assert_size(size, <, 255);  // arbitrary limit
            for (size_t i = 0; i < size; ++i) {
                char const* argtype =
                    typestore_type_id_to_str(ts, alloc, type->as.proc.args[i]);
                b = da_extend_char(b, alloc, argtype, strlen(argtype));
                allocator_free(alloc, argtype);

                b = da_extend_char(b, alloc, ", ", 2);
                if (i > 10) {
                    b = da_extend_char(b, alloc, "\n", 1);
                    printf("\n%s\n", b);
                    munit_assert(false);
                }
            }

            b = da_extend_char(b, alloc, ") -> ", 5);

            char const* rettype =
                typestore_type_id_to_str(ts, alloc, type->as.proc.return_type);
            b = da_extend_char(b, alloc, rettype, strlen(rettype));
            allocator_free(alloc, rettype);

            char zero = 0;
            b = da_append_char(b, alloc, &zero);

            size = da_get_size(b);
            char* buf = allocator_alloc(alloc, size);
            memcpy(buf, b, size);

            da_free(b, alloc);

            return buf;
        }
        case TYPE_RECORD: {
            char* b = da_init_char(alloc);
            b = da_extend_char(b, alloc, "record {", 8);

            size_t count = da_get_size(type->as.record.fields);
            munit_assert_size(count, <, 255);  // arbitrary limit
            for (size_t i = 0; i < count; i++) {
                b = da_extend_char(b, alloc, type->as.record.fields[i].name,
                                   strlen(type->as.record.fields[i].name));
                b = da_extend_char(b, alloc, ": ", 2);

                char const* fieldtype = typestore_type_id_to_str(
                    ts, alloc, type->as.record.fields[i].type);
                b = da_extend_char(b, alloc, fieldtype, strlen(fieldtype));
                allocator_free(alloc, fieldtype);

                b = da_extend_char(b, alloc, ", ", 2);
                if (i > 10) {
                    b = da_extend_char(b, alloc, "\n", 1);
                    printf("\n%s\n", b);
                    munit_assert(false);
                }
            }

            b = da_extend_char(b, alloc, "}", 1);

            char zero = 0;
            b = da_append_char(b, alloc, &zero);

            size_t size = da_get_size(b);
            char*  buf = allocator_alloc(alloc, size);
            memcpy(buf, b, size);

            da_free(b, alloc);

            return buf;
        }
        case TYPE_KW: {
            return allocator_sprintf(alloc, ".%s", type->as.kw.ident);
        }
    }

    return allocator_sprintf(alloc, "<invalid %d>", type->tag);
}
