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
