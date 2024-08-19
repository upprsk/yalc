#include "yair.h"

#include "allocator.h"

typedef struct state {
    allocator_t tempalloc;
} state_t;

void pass_to_ir(pass_to_ir_params_t const* params) {
    Arena       arena = {};
    allocator_t tempalloc = {};
    allocator_init_arena(&tempalloc, &arena);

    arena_free(&arena);
}
