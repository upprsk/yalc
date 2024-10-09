#pragma once

#include <stddef.h>
#include <stdint.h>

/// The type of the allocation function.
///
/// When `ptr` is NULL and `size` is not 0, then it should allocate (`malloc`).
///
/// When `ptr` is non-NULL and `size` is not 0, then it should free the pointer
/// (`free`).
///
/// When `ptr` ir non-NULL and `size` is 0, then it should reallocate the
/// pointer with the new size (`realloc`).
///
/// When `ptr` is NULL and `size` is 0, the operation is a no-op.
typedef void* (*allocator_allocate_fn_t)(void* ctx, void* ptr, size_t size);

/// The v-table of an allocator, contais a single function pointer for an
/// allocate function that is used for all operations of malloc, free and
/// realloc.
typedef struct allocator {
    allocator_allocate_fn_t alloc;
    void*                   ctx;
} allocator_t;

/// Perform an allocation of the given size.
void* allocator_alloc(allocator_t a, size_t size);

/// Free an allocation, does nothing if `ptr` is null.
void allocator_free(allocator_t a, void* ptr);

/// Realloc an existing allocation.
void* allocator_realloc(allocator_t a, void* ptr, size_t size);

// ----------------------------------------------------------------------------

/// Wrapper for the standard c-allocator.
allocator_t c_allocator(void);

// ----------------------------------------------------------------------------

/// An allocation block for the arena allocator.
typedef struct arena_block arena_block_t;
struct arena_block {
    arena_block_t* next;

    uint8_t* end;
    uint8_t* head;
    uint8_t  start[];
};

/// An arena allocator, free is a no-op.
typedef struct arena_allocator {
    /// Store what was the last allocation in case a realloc for it is
    /// requested. This way we avoid having to allocate a whole new block of
    /// memory for it.
    uint8_t* last_alloc;

    /// The allocated memory block.
    arena_block_t* head;
} arena_allocator_t;

/// Get an allocator interface struct from the arena.
///
/// The given `arena_allocator` **MUST** longer than the allocator interface.
///
/// NOTE: This uses memmap to allocate the blocks (full pages), this means that
/// the implementation is linux only for now.
allocator_t arena_allocator(arena_allocator_t* a);

/// Clear the arena, keeping one memory block.
void arena_clear(arena_allocator_t* a);

/// Clear the arena, freeing all data.
void arena_clear_all(arena_allocator_t* a);
