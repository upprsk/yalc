#include <stdint.h>
#include <unistd.h>

#include "alloc/allocator.h"
#include "ast.h"
#include "test.h"

static MunitResult test_c_allocator_basic(MunitParameter const params[],
                                          void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    int* p = allocator_alloc(alloc, sizeof(*p));
    assert_not_null(p);

    *p = 42;

    allocator_free(alloc, p);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_create(MunitParameter const params[],
                                               void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);
    (void)alloc;

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_basic(MunitParameter const params[],
                                              void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p1 = allocator_alloc(alloc, sizeof(*p1));
    assert_not_null(p1);
    assert_size((uintptr_t)p1 % 8, ==, 0);

    *p1 = 42;

    // this should be a nop
    allocator_free(alloc, p1);

    int* p2 = allocator_alloc(alloc, sizeof(*p2));
    assert_not_null(p2);
    assert_size((uintptr_t)p2 % 8, ==, 0);

    *p2 = 42;

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_clear(MunitParameter const params[],
                                              void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p1 = allocator_alloc(alloc, sizeof(*p1));
    assert_not_null(p1);
    assert_size((uintptr_t)p1 % 8, ==, 0);

    *p1 = 42;

    arena_clear(&arena);

    int* p2 = allocator_alloc(alloc, sizeof(*p2));
    assert_not_null(p2);
    assert_size((uintptr_t)p2 % 8, ==, 0);

    // the two should be the same, as the block will be reused
    assert_ptr_equal(p1, p2);

    *p2 = 42;

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_realloc(MunitParameter const params[],
                                                void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p1 = allocator_alloc(alloc, 4 * sizeof(*p1));
    assert_not_null(p1);

    // put an allocation in between
    int* p2 = allocator_alloc(alloc, 4 * sizeof(*p2));
    assert_not_null(p2);

    int* p3 = allocator_realloc(alloc, p1, 12 * sizeof(*p3));
    assert_not_null(p3);

    // should still be the same allocation
    assert_ptr_not_equal(p1, p3);

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_realloc_latest(
    MunitParameter const params[], void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p1 = allocator_alloc(alloc, 4 * sizeof(*p1));
    assert_not_null(p1);
    memset(p1, 0xCC, 4 * sizeof(*p1));

    int* p2 = allocator_realloc(alloc, p1, 12 * sizeof(*p2));
    assert_not_null(p2);

    {
        int expected[4];
        memset(expected, 0xCC, sizeof(expected));
        assert_memory_equal(4 * sizeof(*p2), p2, expected);
    }

    // should still be the same allocation
    assert_ptr_equal(p1, p2);

    int* p3 = allocator_alloc(alloc, 32 * sizeof(*p3));
    assert_not_null(p3);
    memset(p1, 0xCC, 32 * sizeof(*p1));

    int* p4 = allocator_alloc(alloc, 32 * sizeof(*p4));
    assert_not_null(p4);

    arena_block_t* blk = arena.head;

    int* p5 = allocator_realloc(alloc, p3, 8 * sizeof(*p5));
    assert_not_null(p5);
    assert_ptr_equal(p3, p5);
    assert_ptr_equal(blk->head, arena.head->head);

    {
        int expected[8];
        memset(expected, 0xCC, sizeof(expected));
        assert_memory_equal(8 * sizeof(*p5), p5, expected);
    }

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_realloc_endpage(
    MunitParameter const params[], void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    // get the page size (*NIX) minus 16 ints
    long very_large_boy = sysconf(_SC_PAGESIZE) - 16 * sizeof(int);

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p = allocator_alloc(alloc, very_large_boy);
    assert_not_null(p);

    arena_block_t* blk1 = arena.head;

    int* p1 = allocator_alloc(alloc, 1 * sizeof(*p1));
    assert_not_null(p1);
    assert_ptr_equal(blk1, arena.head);

    p1[0] = 0x32;

    int* p2 = allocator_realloc(alloc, p1, 12 * sizeof(*p2));
    assert_not_null(p2);
    assert_ptr_not_equal(blk1, arena.head);

    // should not be the same location
    assert_ptr_not_equal(p1, p2);
    // but should have the same contents
    assert_int(p1[0], ==, p2[0]);

    arena_block_t* blk2 = arena.head;

    int* p3 = allocator_realloc(alloc, p, very_large_boy / 2);
    assert_not_null(p3);
    assert_ptr_equal(blk2, arena.head);
    assert_ptr_equal(blk2->head, arena.head->head);

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_fill_page(MunitParameter const params[],
                                                  void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    // get the page size (*NIX) minus 8 ints
    long very_large_boy = sysconf(_SC_PAGESIZE) - 8 * sizeof(int);

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p1 = allocator_alloc(alloc, very_large_boy);
    assert_not_null(p1);

    arena_block_t* blk1 = arena.head;

    // allocate 24 ints, this should trigger allocating a new block
    int* p2 = allocator_alloc(alloc, 24 * sizeof(*p2));
    assert_not_null(p2);

    arena_block_t* blk2 = arena.head;
    assert_ptr_not_equal(blk1, blk2);

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_arena_allocator_clear_page(
    MunitParameter const params[], void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    // get the page size (*NIX) minus 8 ints
    long very_large_boy = sysconf(_SC_PAGESIZE) - 8 * sizeof(int);

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    int* p1 = allocator_alloc(alloc, very_large_boy);
    assert_not_null(p1);

    arena_block_t* blk1 = arena.head;

    // allocate 24 ints, this should trigger allocating a new block
    int* p2 = allocator_alloc(alloc, 24 * sizeof(*p2));
    assert_not_null(p2);

    arena_block_t* blk2 = arena.head;
    assert_ptr_not_equal(blk1, blk2);

    arena_clear(&arena);

    int* p3 = allocator_alloc(alloc, 24 * sizeof(*p3));
    assert_not_null(p3);

    arena_block_t* blk3 = arena.head;
    assert_ptr_equal(blk2, blk3);
    assert_ptr_equal(p2, p3);

    arena_clear_all(&arena);

    return MUNIT_OK;
}

void test_lib_allocator_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/c_allocator/basic",
                          test_c_allocator_basic,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/create",
                          test_arena_allocator_create,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/basic",
                          test_arena_allocator_basic,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/clear",
                          test_arena_allocator_clear,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/realloc",
                          test_arena_allocator_realloc,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/realloc_latest",
                          test_arena_allocator_realloc_latest,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/realloc_endpage",
                          test_arena_allocator_realloc_endpage,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/fill_page",
                          test_arena_allocator_fill_page,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/allocator/arena/clear_page",
                          test_arena_allocator_clear_page,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
