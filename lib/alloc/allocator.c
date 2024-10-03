#include "allocator.h"

#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "munit.h"
#include "utils/minmax.h"

void* allocator_alloc(allocator_t a, size_t size) {
    assert_not_null(a.alloc);

    return a.alloc(a.ctx, NULL, size);
}

void allocator_free(allocator_t a, void* ptr) {
    assert_not_null(a.alloc);

    // could return here, but we specified that null, 0 is a no-op.
    // if (!ptr) return;

    a.alloc(a.ctx, ptr, 0);
}

void* allocator_realloc(allocator_t a, void* ptr, size_t size) {
    assert_not_null(a.alloc);

    return a.alloc(a.ctx, ptr, size);
}

// ----------------------------------------------------------------------------

static void* c_allocator_fn(void* ctx, void* ptr, size_t size) {
    // handle no-op case
    if (!ptr && !size) return NULL;

    // allocation request
    if (!ptr && size) return calloc(size, sizeof(uint8_t));

    // free request
    if (ptr && !size) {
        free(ptr);
        return NULL;
    }

    // realloc request
    return realloc(ptr, size);
}

allocator_t c_allocator(void) { return (allocator_t){.alloc = c_allocator_fn}; }

// ----------------------------------------------------------------------------

struct arena_block {
    arena_block_t* next;

    uint8_t* end;
    uint8_t* head;
    uint8_t  start[];
};

static arena_block_t* arena_allocate_new_block(arena_block_t* next,
                                               size_t         size) {
    assert_size(size, !=, 0);

    long page_size = sysconf(_SC_PAGESIZE);
    while (page_size < size) page_size <<= 1;  // multiply by 2

    arena_block_t* blk = mmap(NULL, page_size, PROT_READ | PROT_WRITE,
                              MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (!blk) return NULL;

    // the blocks form a linked list, save the next in the chain
    blk->next = next;

    // save the end of the block (we need it for both detecting when we are out
    // of space and when unmapping the block)
    blk->end = (uint8_t*)blk + page_size;

    // the head is at the start
    blk->head = blk->start;

    return blk;
}

static void arena_block_free(arena_block_t* blk) {
    assert_not_null(blk);

    munmap(blk, blk->end - (uint8_t*)blk);
}

static inline size_t arena_block_available(arena_block_t const* blk) {
    assert_not_null(blk);

    return blk->end - blk->head;
}

static bool arena_block_fits(arena_block_t const* blk, size_t size) {
    return arena_block_available(blk) >= size;
}

static void* arena_block_allocate(arena_block_t* blk, size_t size) {
    assert_not_null(blk);
    assert_size(size, !=, 0);
    assert_size(arena_block_available(blk), >=, size);

    void* ptr = blk->head;

    // advance past the allocated region
    blk->head += size;

    // align the final address up (so that the next allocation is pointer
    // aligned)
    uintptr_t addr =
        ((uintptr_t)blk->head + (sizeof(uintptr_t) - 1)) & -sizeof(uintptr_t);

    // make shure that the head is before the end after alignment
    blk->head = MIN(blk->end, (uint8_t*)addr);

    return ptr;
}

static arena_block_t* arena_append_block(arena_allocator_t* a, size_t size) {
    assert_not_null(a);

    arena_block_t* blk = arena_allocate_new_block(a->head, size);
    if (!blk) return NULL;

    // add the new block to the list
    a->head = blk;
    return blk;
}

static arena_block_t* arena_get_available_block(arena_allocator_t* a,
                                                size_t             size) {
    assert_not_null(a);

    if (a->head && arena_block_fits(a->head, size)) return a->head;
    return arena_append_block(a, size);
}

static arena_block_t* arena_find_block(arena_allocator_t const* a, void* ptr) {
    assert_not_null(a);
    uint8_t* addr = ptr;

    for (arena_block_t* b = a->head; b; b = b->next) {
        if (addr >= b->start && addr < b->end) return b;
    }

    return NULL;
}

static void* arena_allocator_fn(void* ctx, void* ptr, size_t size) {
    arena_allocator_t* a = ctx;

    // no-op and we don't do frees
    if ((!ptr && !size) || (ptr && !size)) return NULL;

    size_t original_size = size;

    // allocate
    if (!ptr && size) {
        // we store the size of the allocation, so we actually need to allocate
        // 8 bytes more (so that the returned pointer is still pointer aligned)
        size += sizeof(size_t);

        arena_block_t* blk = arena_get_available_block(a, size);
        if (!blk) return NULL;

        size_t* raw = arena_block_allocate(blk, size);
        if (!raw) return NULL;

        // save allocation size
        // FIXME: adjust the size to include the aligment
        *raw = original_size;

        // return to the user the data right after the stored size.
        uint8_t* ptr = (uint8_t*)&raw[1];
        a->last_alloc = ptr;

        return ptr;
    }

    // realloc
    // We need the block from where this was allocated.
    arena_block_t* blk = arena_find_block(a, ptr);
    assert_not_null(blk);

    // the size of the allocation is stored right before the bytes we return
    // to the user.
    size_t* size_ptr = &((size_t*)ptr)[-1];
    size_t  prev_size = *size_ptr;

    // same size, ignore.
    if (prev_size == size) return ptr;

    uint8_t* alloc = ptr;
    if (alloc == a->last_alloc) {
        // we are growing the last allocation, so we have the opportunity to
        // simply grow it instead of allocating a new region and copying over.

        if (alloc + size <= blk->end) {
            // the new size still fits inside of the block, so we update the
            // size and head to it.
            blk->head = alloc + size;

            // align the final address up (so that the next allocation is
            // pointer aligned)
            uintptr_t addr = ((uintptr_t)blk->head + (sizeof(uintptr_t) - 1)) &
                             -sizeof(uintptr_t);

            // make shure that the head is before the end after alignment
            blk->head = MIN(blk->end, (uint8_t*)addr);

            // update the size of the allocation
            // FIXME: adjust the size to include the aligment
            *size_ptr = size;

            // return the same pointer
            return ptr;
        }

        // the new size does not fit in the current block, so we need to
        // allocate a new one
    } else {
        // it is not the most recent allocation, but if the new size is smaller,
        // the we don't need to allocate anything new, so we check for that.
        if (size < prev_size) {
            blk->head = alloc + size;

            // align the final address up (so that the next allocation is
            // pointer aligned)
            uintptr_t addr = ((uintptr_t)blk->head + (sizeof(uintptr_t) - 1)) &
                             -sizeof(uintptr_t);

            // make shure that the head is before the end after alignment
            blk->head = MIN(blk->end, (uint8_t*)addr);

            // update the size of the allocation
            // FIXME: adjust the size to include the aligment
            *size_ptr = size;

            // return the same pointer
            return ptr;
        }

        // the new size is larger, then we need to allocate.
    }

    // we store the size of the allocation, so we actually need to allocate
    // 8 bytes more (so that the returned pointer is still pointer aligned)
    size += sizeof(size_t);

    arena_block_t* new_blk = arena_append_block(a, size);
    if (!new_blk) return NULL;

    size_t* raw = arena_block_allocate(new_blk, size);
    if (!raw) return NULL;
    *raw = original_size;

    // return to the user the data right after the stored size.
    uint8_t* new_ptr = (uint8_t*)&raw[1];
    a->last_alloc = new_ptr;

    // copy the old data into the new allocation.
    memcpy(new_ptr, ptr, prev_size);

    return new_ptr;
}

allocator_t arena_allocator(arena_allocator_t* a) {
    return (allocator_t){.ctx = a, .alloc = arena_allocator_fn};
}

void arena_clear(arena_allocator_t* a) {
    assert_not_null(a);

    if (!a->head) return;

    arena_block_t* b = a->head->next;
    while (b) {
        arena_block_t* blk = b;
        b = b->next;

        arena_block_free(blk);
    }

    // reset the head of the remaining block
    a->head->head = a->head->start;
    a->head->next = NULL;

    // don't actually need to reset this, but let's do it anyway
    a->last_alloc = NULL;
}

void arena_clear_all(arena_allocator_t* a) {
    assert_not_null(a);

    arena_block_t* b = a->head;
    while (b) {
        arena_block_t* blk = b;
        b = b->next;

        arena_block_free(blk);
    }

    a->head = NULL;

    // don't actually need to reset this, but let's do it anyway
    a->last_alloc = NULL;
}
