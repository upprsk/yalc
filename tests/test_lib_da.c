#include "alloc/allocator.h"
#include "ast.h"
#include "da/da.h"
#include "slice/slice.h"
#include "test.h"

static MunitResult test_da_create(MunitParameter const params[],
                                  void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    // with explicit initialzer
    da_int_t arr = {.alloc = alloc};
    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    // with helper
    da_int_t arr2 = da_init(alloc);
    da_free(&arr2);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_reserve(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};
    da_reserve(&arr, 30);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 32);

    da_reserve(&arr, 10);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 32);

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_reserve_exact(MunitParameter const params[],
                                         void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};
    da_reserve_exact(&arr, 31);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 31);

    da_reserve_exact(&arr, 10);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 10);

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_resize(MunitParameter const params[],
                                  void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};
    da_resize(&arr, 31);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 31);
    assert_uint32(arr.capacity, ==, 31);

    da_resize(&arr, 10);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 10);
    assert_uint32(arr.capacity, ==, 31);

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_push(MunitParameter const params[],
                                void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};

    da_push_back(&arr, 32);
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 1);
    assert_uint32(arr.capacity, ==, DA_INITIAL_CAPACITY);
    assert_int(arr.items[0], ==, 32);
    assert_int(da_at(arr, 0), ==, 32);

    da_push_back(&arr, 12);
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 2);
    assert_uint32(arr.capacity, ==, DA_INITIAL_CAPACITY);
    assert_int(arr.items[0], ==, 32);
    assert_int(arr.items[1], ==, 12);
    assert_int(da_at(arr, 0), ==, 32);
    assert_int(da_at(arr, 1), ==, 12);

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_pop(MunitParameter const params[],
                               void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};

    da_append(&arr, 4, (int[]){1, 2, 3, 4});
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 4);
    assert_uint32(arr.capacity, ==, DA_INITIAL_CAPACITY);
    assert_int(da_at(arr, 0), ==, 1);
    assert_int(da_at(arr, 1), ==, 2);
    assert_int(da_at(arr, 2), ==, 3);
    assert_int(da_at(arr, 3), ==, 4);

    assert_int(da_pop(&arr), ==, 4);
    assert_int(da_pop(&arr), ==, 3);
    assert_int(da_pop(&arr), ==, 2);

    int r = 42;
    assert_int(da_pop_opt(&arr, &r), ==, DA_OK);
    assert_int(r, ==, 1);

    r = 42;
    assert_int(da_pop_opt(&arr, &r), ==, DA_EMPTY);
    assert_int(r, ==, 42);

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_append(MunitParameter const params[],
                                  void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};

    da_append(&arr, 4, (int[]){1, 2, 3, 4});
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 4);
    assert_uint32(arr.capacity, ==, DA_INITIAL_CAPACITY);
    assert_int(da_at(arr, 0), ==, 1);
    assert_int(da_at(arr, 1), ==, 2);
    assert_int(da_at(arr, 2), ==, 3);
    assert_int(da_at(arr, 3), ==, 4);

    da_append(&arr, 8, (int[]){5, 6, 7, 8, 9, 10, 11, 12});
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 12);
    assert_uint32(arr.capacity, ==, DA_INITIAL_CAPACITY * 2);

    for (int i = 0; i < (int)arr.size; i++) {
        assert_int(da_at(arr, i), ==, i + 1);
    }

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_clear(MunitParameter const params[],
                                 void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    static int items[] = {1, 2,  3,  4,  5,  6,  7,  8,
                          9, 10, 11, 12, 13, 14, 15, 16};

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};

    da_append(&arr, sizeof(items) / sizeof(items[0]), items);
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 16);
    assert_uint32(arr.capacity, ==, 16);

    for (int i = 0; i < (int)arr.size; i++) {
        assert_int(da_at(arr, i), ==, i + 1);
    }

    da_clear(&arr);

    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 16);

    int* ptr = arr.items;

    da_append(&arr, sizeof(items) / sizeof(items[0]), items);
    assert_ptr_equal(arr.items, ptr);
    assert_uint32(arr.size, ==, 16);
    assert_uint32(arr.capacity, ==, 16);

    for (int i = 0; i < (int)arr.size; i++) {
        assert_int(da_at(arr, i), ==, i + 1);
    }

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_slice(MunitParameter const params[],
                                 void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    static int items[] = {1, 2,  3,  4,  5,  6,  7,  8,
                          9, 10, 11, 12, 13, 14, 15, 16};

    allocator_t alloc = c_allocator();
    da_int_t    arr = {.alloc = alloc};

    da_append(&arr, sizeof(items) / sizeof(items[0]), items);
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 16);
    assert_uint32(arr.capacity, ==, 16);

    slice_int_t s = da_to_slice(arr);
    assert_not_null(s.ptr);
    assert_uint32(s.len, ==, 16);

    da_free(&arr);

    assert_null(arr.items);
    assert_uint32(arr.size, ==, 0);
    assert_uint32(arr.capacity, ==, 0);

    da_append(&arr, sizeof(items) / sizeof(items[0]), items);
    assert_not_null(arr.items);
    assert_uint32(arr.size, ==, 16);
    assert_uint32(arr.capacity, ==, 16);

    s = da_to_slice_of(arr, slice_int_t);
    assert_not_null(s.ptr);
    assert_uint32(s.len, ==, 16);

    slice_int_t ss = slice_s(s, 2, 5);
    assert_not_null(ss.ptr);
    assert_ptr_equal(ss.ptr, arr.items + 2);
    assert_size(ss.len, ==, 5 - 2);

    slice_free(alloc, &s);
    assert_null(s.ptr);
    assert_uint32(s.len, ==, 0);

    return MUNIT_OK;
}

static MunitResult test_da_sprintf(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    string_t s = da_sprintf(alloc, "Hello %s!", "world");

    assert_string_equal(s.items, "Hello world!");
    assert_uint32(s.size, ==, 13);
    assert_uint32(s.capacity, ==, 13);

    da_free(&s);

    assert_null(s.items);
    assert_uint32(s.size, ==, 0);
    assert_uint32(s.capacity, ==, 0);

    return MUNIT_OK;
}

void test_lib_da_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/create",
                          test_da_create,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/reserve",
                          test_da_reserve,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/reserve_exact",
                          test_da_reserve_exact,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/resize",
                          test_da_resize,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/push",
                          test_da_push,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/pop",
                          test_da_pop,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/append",
                          test_da_append,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/clear",
                          test_da_clear,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/slice",
                          test_da_slice,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/da/sprintf",
                          test_da_sprintf,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
