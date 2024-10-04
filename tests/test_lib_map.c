#include <stdbool.h>

#include "alloc/allocator.h"
#include "map/map.h"
#include "test.h"

static MunitResult test_map_create(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    map_int_int_t* map = NULL;
    assert_size(map_len(map), ==, 0);

    map_free(alloc, map);

    return MUNIT_OK;
}

static MunitResult test_map_put(MunitParameter const params[],
                                void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    map_int_int_t* map = NULL;
    assert_size(map_len(map), ==, 0);

    map = map_put(alloc, map, 12, 33);
    assert_size(map_len(map), ==, 1);
    map = map_put(alloc, map, 14, 69);
    assert_size(map_len(map), ==, 2);

    assert_true(map_contains(map, 12, map_cmp_int));
    assert_false(map_contains(map, 13, map_cmp_int));

    map_free(alloc, map);
    assert_size(map_len(map), ==, 0);

    return MUNIT_OK;
}

static MunitResult test_map_get(MunitParameter const params[],
                                void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    map_int_int_t* map = NULL;
    assert_size(map_len(map), ==, 0);

    map = map_put(alloc, map, 12, 33);
    assert_size(map_len(map), ==, 1);
    map = map_put(alloc, map, 11, 1);
    assert_size(map_len(map), ==, 2);
    map = map_put(alloc, map, 14, 69);
    assert_size(map_len(map), ==, 3);

    assert_true(map_contains_kint(map, 12));
    assert_false(map_contains_kint(map, 13));

    map_int_int_t* one = map_get(map, 11, map_cmp_int);
    assert_not_null(one);
    assert_int(one->key, ==, 11);
    assert_int(one->value, ==, 1);

    map_int_int_t* noice = map_get_kint(map, 14);
    assert_not_null(noice);
    assert_int(noice->key, ==, 14);
    assert_int(noice->value, ==, 69);

    assert_null(map_get_kint(map, 2));

    map_free(alloc, map);
    assert_size(map_len(map), ==, 0);

    return MUNIT_OK;
}

static MunitResult test_map_del(MunitParameter const params[],
                                void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    map_int_int_t* map = NULL;
    assert_size(map_len(map), ==, 0);

    map = map_put(alloc, map, 12, 33);
    assert_size(map_len(map), ==, 1);
    map = map_put(alloc, map, 11, 1);
    assert_size(map_len(map), ==, 2);
    map = map_put(alloc, map, 14, 69);
    assert_size(map_len(map), ==, 3);

    map_int_int_t* one = map_get(map, 11, map_cmp_int);
    assert_not_null(one);
    assert_int(one->key, ==, 11);
    assert_int(one->value, ==, 1);

    map_int_int_t* deleted = NULL;
    map_del(map, 12, &deleted, map_cmp_int);
    assert_size(map_len(map), ==, 2);
    assert_not_null(deleted);
    assert_int(deleted->key, ==, 12);
    assert_int(deleted->value, ==, 33);

    map_free(alloc, deleted);
    map_free(alloc, map);

    assert_size(map_len(map), ==, 0);

    return MUNIT_OK;
}

static MunitResult test_map_cstr(MunitParameter const params[],
                                 void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    map_cstr_int_t* map = NULL;
    assert_size(map_len(map), ==, 0);

    map = map_put(alloc, map, "one", 1);
    map = map_put(alloc, map, "two", 2);
    map = map_put(alloc, map, "three", 3);
    map = map_put(alloc, map, "four", 4);
    map = map_put(alloc, map, "five", 5);

    assert_size(map_len(map), ==, 5);

    map_cstr_int_t* one = map_get_kcstr(map, "one");
    assert_not_null(one);
    assert_int(one->value, ==, 1);

    map_cstr_int_t* four = map_get_kcstr(map, "four");
    assert_not_null(four);
    assert_int(four->value, ==, 4);

    map_free(alloc, map);
    assert_size(map_len(map), ==, 0);

    return MUNIT_OK;
}

static MunitResult test_map_string(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    map_str_int_t* map = NULL;
    assert_size(map_len(map), ==, 0);

    map = map_put(alloc, map, mkstrkey("one"), 1);
    map = map_put(alloc, map, mkstrkey("two"), 2);
    map = map_put(alloc, map, mkstrkey("three"), 3);
    map = map_put(alloc, map, mkstrkey("four"), 4);
    map = map_put(alloc, map, mkstrkey("five"), 5);

    assert_size(map_len(map), ==, 5);

    map_str_int_t* one = map_get_kstr(map, mkstrkey("one"));
    assert_not_null(one);
    assert_int(one->value, ==, 1);

    map_str_int_t* four = map_get_kstr(map, mkstrkey("four"));
    assert_not_null(four);
    assert_int(four->value, ==, 4);

    map_free(alloc, map);
    assert_size(map_len(map), ==, 0);

    return MUNIT_OK;
}

void test_lib_map_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/lib/map/create",
                          test_map_create,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/map/put",
                          test_map_put,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/map/get",
                          test_map_get,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/map/del",
                          test_map_del,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/map/cstr",
                          test_map_cstr,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/map/string",
                          test_map_string,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
