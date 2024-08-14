#pragma once

#include <stdio.h>

#include "arena/arena.h"

typedef void* (*allocator_alloc_fn)(void* ctx, size_t size, size_t align);
typedef void (*allocator_free_fn)(void* ctx, void const* ptr);
typedef void* (*allocator_realloc_fn)(void* ctx, void* ptr, size_t old_size,
                                      size_t new_size);

typedef struct allocator_vtable {
    allocator_alloc_fn   alloc;
    allocator_free_fn    free;
    allocator_realloc_fn realloc;
} allocator_vtable_t;

typedef struct allocator {
    allocator_vtable_t const* vtable;
    void*                     ctx;
} allocator_t;

// ============================================================================

void allocator_init_stdc(allocator_t* a);
void allocator_init_arena(allocator_t* a, Arena* arena);

// ============================================================================

void* allocator_alloc(allocator_t a, size_t size);
void* allocator_alloc_opt(allocator_t a, size_t size, size_t align);
void  allocator_free(allocator_t a, void const* ptr);
void* allocator_realloc(allocator_t a, void* ptr, size_t old_size,
                        size_t new_size);

char* allocator_vsprintf(allocator_t a, char const* format, va_list va);
char* allocator_sprintf(allocator_t a, char const* format, ...);
