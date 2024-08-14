#include "typestore.h"

#include "allocator.h"
#include "da.h"

void typestore_init(typestore_t* ts, allocator_t alloc) {
    *ts = (typestore_t){
        .entries = da_init(typestore_entry_t, alloc),
        .alloc = alloc,
    };

    typestore_entry_t inval_entry = {.id = {0}, .type = {.tag = TYPE_ERR}};
    ts->entries = da_append(ts->entries, alloc, &inval_entry);

    ts->primitives.err = typestore_add_type(ts, &(type_t){.tag = TYPE_ERR});
    ts->primitives.void_ = typestore_add_type(ts, &(type_t){.tag = TYPE_VOID});
    ts->primitives.type = typestore_add_type(ts, &(type_t){.tag = TYPE_TYPE});
    ts->primitives.i32 = typestore_add_type(
        ts, &(type_t){
                .tag = TYPE_INT, .as.int_ = {.bits = 32, .signed_ = true}
    });
}

void typestore_deinit(typestore_t* ts) {
    ts->entries = da_free(ts->entries, ts->alloc);
}

type_id_t typestore_add_type(typestore_t* ts, type_t const* t) {
    // TODO: De-dupe types

    type_id_t         id = {da_get_size(ts->entries)};
    typestore_entry_t entry = {.type = *t, .id = id};
    ts->entries = da_append(ts->entries, ts->alloc, &entry);

    return id;
}

type_t const* typestore_find_type(typestore_t* ts, type_id_t id) {
    size_t size = da_get_size(ts->entries);
    for (size_t i = 0; i < size; ++i) {
        if (type_id_eq(ts->entries[i].id, id)) return &ts->entries[i].type;
    }

    return NULL;
}
