#include "allocator.h"

#include <stdlib.h>

#include "span.h"

static void* stdc_alloc(void* ctx, size_t size, size_t align) {
    (void)ctx;
    (void)align;

    return malloc(size);
}

static void stdc_free(void* ctx, void* ptr) {
    (void)ctx;

    free(ptr);
}

static void* stdc_realloc(void* ctx, void* ptr, size_t new_size) {
    (void)ctx;

    return realloc(ptr, new_size);
}

static allocator_vtable_t const stdc_vtable = {
    .alloc = stdc_alloc,
    .free = stdc_free,
    .realloc = stdc_realloc,
};

void allocator_init_stdc(allocator_t* a) {
    *a = (allocator_t){.vtable = &stdc_vtable};
}

void* allocator_alloc(allocator_t a, size_t size) {
    return allocator_alloc_opt(a, size, _Alignof(void*));
}

void* allocator_alloc_opt(allocator_t a, size_t size, size_t align) {
    munit_assert_not_null(a.vtable);
    munit_assert_not_null(a.vtable->alloc);

    return a.vtable->alloc(a.ctx, size, align);
}

void allocator_free(allocator_t a, void* ptr) {
    munit_assert_not_null(a.vtable);
    if (!a.vtable->free) return;

    return a.vtable->free(a.ctx, ptr);
}

void* allocator_realloc(allocator_t a, void* ptr, size_t new_size) {
    munit_assert_not_null(a.vtable);
    munit_assert_not_null(a.vtable->realloc);

    return a.vtable->realloc(a.ctx, ptr, new_size);
}

static void* my_arena_alloc(void* ctx, size_t size, size_t align) {
    (void)align;

    Arena* arena = ctx;
    return arena_alloc(arena, size);
}

// static void* my_arena_realloc(void* ctx, void* ptr, size_t new_size) {
//     return arena_realloc(arena, ptr, size_t oldsz, size_t newsz)
// }

static allocator_vtable_t const arena_vtable = {
    .alloc = my_arena_alloc,
};

void allocator_init_arena(allocator_t* a, Arena* arena) {
    *a = (allocator_t){.vtable = &arena_vtable, .ctx = arena};
}
