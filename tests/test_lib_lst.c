#include "alloc/allocator.h"
#include "lst/lst.h"
#include "test.h"

static MunitResult test_lst_create(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    lst_int_t* lst = NULL;
    assert_size(lst_len(lst), ==, 0);

    lst_free(alloc, lst);
    assert_size(lst_len(lst), ==, 0);

    return MUNIT_OK;
}

static MunitResult test_lst_prepend(MunitParameter const params[],
                                    void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    lst_int_t* lst = NULL;
    assert_size(lst_len(lst), ==, 0);

    lst = lst_prepend(alloc, lst, 1);
    assert_size(lst_len(lst), ==, 1);

    lst = lst_prepend(alloc, lst, 2);
    assert_size(lst_len(lst), ==, 2);

    lst_free(alloc, lst);
    assert_size(lst_len(lst), ==, 0);

    return MUNIT_OK;
}

static MunitResult test_lst_pop(MunitParameter const params[],
                                void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    lst_int_t* lst = NULL;
    assert_size(lst_len(lst), ==, 0);

    lst = lst_prepend(alloc, lst, 1);
    assert_size(lst_len(lst), ==, 1);

    lst = lst_prepend(alloc, lst, 2);
    assert_size(lst_len(lst), ==, 2);

    lst_int_t* front = lst;
    lst = lst_pop_front(lst);

    assert_size(lst_len(lst), ==, 1);
    assert_size(lst_len(front), ==, 1);

    assert_int(lst->data, ==, 1);
    assert_int(front->data, ==, 2);

    lst = lst_pop_free_front(alloc, lst);
    assert_null(lst);

    lst_free(alloc, lst);
    lst_free(alloc, front);

    assert_size(lst_len(lst), ==, 0);

    return MUNIT_OK;
}

void test_lib_lst_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/lib/lst/create",
                          test_lst_create,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/lst/prepend",
                          test_lst_prepend,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/lib/lst/pop",
                          test_lst_pop,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
