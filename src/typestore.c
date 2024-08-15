#include "typestore.h"

#include <string.h>

#include "allocator.h"
#include "da.h"

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
    ts->primitives.i32 = typestore_add_type(
        ts, &(type_t){
                .tag = TYPE_INT, .as.int_ = {.bits = 32, .signed_ = true}
    });
    ts->primitives.f32 = typestore_add_type(
        ts, &(type_t){.tag = TYPE_FLOAT, .as.float_ = {.bits = 32}});
    ts->primitives.f64 = typestore_add_type(
        ts, &(type_t){.tag = TYPE_FLOAT, .as.float_ = {.bits = 64}});
}

void typestore_deinit(typestore_t* ts) {
    ts->entries = da_free(ts->entries, ts->alloc);
}

type_id_t typestore_add_type(typestore_t* ts, type_t const* t) {
    // TODO: De-dupe types
    size_t size = da_get_size(ts->entries);
    for (size_t i = 0; i < size; ++i) {
        if (type_eq(&ts->entries[i].type, t)) return ts->entries[i].id;
    }

    type_id_t         id = {size};
    typestore_entry_t entry = {.type = *t, .id = id};
    ts->entries = da_append_typestore_entry(ts->entries, ts->alloc, &entry);

    return id;
}

type_t const* typestore_find_type(typestore_t* ts, type_id_t id) {
    size_t size = da_get_size(ts->entries);
    for (size_t i = 0; i < size; ++i) {
        if (type_id_eq(ts->entries[i].id, id)) return &ts->entries[i].type;
    }

    return NULL;
}

char const* typestore_type_to_str(typestore_t* ts, allocator_t alloc,
                                  type_t const* type) {
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
            for (size_t i = 0; i < size; ++i) {
                char const* argtype =
                    typestore_type_id_to_str(ts, alloc, type->as.proc.args[i]);
                b = da_extend_char(b, alloc, argtype, strlen(argtype));
                allocator_free(alloc, argtype);

                b = da_extend_char(b, alloc, ", ", 2);
            }

            b = da_extend_char(b, alloc, ") -> ", 5);

            char const* rettype =
                typestore_type_id_to_str(ts, alloc, type->as.proc.return_type);
            b = da_extend_char(b, alloc, rettype, strlen(rettype));
            allocator_free(alloc, rettype);

            size = da_get_size(b);
            char* buf = allocator_alloc(alloc, size);
            memcpy(buf, b, size);

            da_free(b, alloc);

            return buf;
        }
    }

    return allocator_sprintf(alloc, "<invalid %d>", type->tag);
}
