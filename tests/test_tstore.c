#include <stdio.h>

#include "alloc/allocator.h"
#include "common.h"
#include "da/da.h"
#include "slice/slice.h"
#include "test.h"
#include "tstore.h"

static MunitResult test_type_kind_to_str(MunitParameter const params[],
                                         void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    assert_string_equal("TYPE_INVAL", type_kind_str(TYPE_INVAL));
    assert_string_equal("TYPE_INT", type_kind_str(TYPE_INT));
    assert_string_equal("TYPE_PROC", type_kind_str(TYPE_PROC));

    return MUNIT_OK;
}

static MunitResult test_tstore_create(MunitParameter const params[],
                                      void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    tstore_t ts;
    tstore_init(&ts, alloc);

    tstore_deinit(&ts);

    return MUNIT_OK;
}

static MunitResult test_tstore_ints(MunitParameter const params[],
                                    void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    allocator_t alloc = c_allocator();

    tstore_t ts;
    tstore_init(&ts, alloc);

    assert_uint32(tstore_add_int(&ts, true, 8).id, ==, 2);
    assert_uint32(tstore_add_int(&ts, false, 8).id, ==, 3);
    assert_uint32(tstore_add_int(&ts, true, 16).id, ==, 4);
    assert_uint32(tstore_add_int(&ts, false, 16).id, ==, 5);
    assert_uint32(tstore_add_int(&ts, true, 32).id, ==, 6);
    assert_uint32(tstore_add_int(&ts, false, 32).id, ==, 7);
    assert_uint32(tstore_add_int(&ts, true, 64).id, ==, 8);
    assert_uint32(tstore_add_int(&ts, false, 64).id, ==, 9);

    tstore_deinit(&ts);

    return MUNIT_OK;
}

static MunitResult test_tstore_ptr(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    tstore_t ts;
    tstore_init(&ts, alloc);

    type_ref_t u8 = ts.builtins.u8;
    type_ref_t i32 = ts.builtins.i32;

    type_ref_t ptr1 = tstore_add_ptr(&ts, 0, i32, 0);
    assert_uint32(ptr1.id, ==, 10);
    type_ref_t ptr2 = tstore_add_ptr(&ts, PTR_CONST, i32, 0);
    assert_uint32(ptr2.id, ==, 11);
    type_ref_t ptr3 = tstore_add_ptr(&ts, PTR_CONST | PTR_MULTI, u8, 0);
    assert_uint32(ptr3.id, ==, 12);
    type_ref_t ptr4 = tstore_add_ptr(&ts, 0, ptr1, 0);
    assert_uint32(ptr4.id, ==, 13);

    assert_uint32(tstore_add_ptr(&ts, PTR_CONST, i32, 0).id, ==, 11);
    assert_uint32(tstore_add_ptr(&ts, 0, ptr1, 0).id, ==, 13);

    da_args_t proc1_args = da_init(alloc);
    da_push_back(&proc1_args, (type_proc_arg_t){.type = i32});
    type_ref_t proc1 =
        tstore_add_proc(&ts, tstore_add_args(&ts, proc1_args), i32, false);

    type_ref_t ptr5 = tstore_add_ptr(&ts, PTR_CONST, proc1, 0);
    assert_uint32(ptr5.id, ==, 15);

    tstore_deinit(&ts);
    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_tstore_proc(MunitParameter const params[],
                                    void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    tstore_t ts;
    tstore_init(&ts, alloc);

    type_ref_t i32 = ts.builtins.i32;
    type_ref_t u32 = ts.builtins.u32;

    da_args_t proc1_args = da_init(alloc);
    da_push_back(&proc1_args, (type_proc_arg_t){.type = i32});

    type_ref_t proc1 =
        tstore_add_proc(&ts, tstore_add_args(&ts, proc1_args), i32, false);
    assert_uint32(proc1.id, ==, 10);

    da_args_t proc2_args = da_init(alloc);
    da_push_back(&proc2_args, (type_proc_arg_t){.type = u32});
    da_push_back(&proc2_args, (type_proc_arg_t){.type = u32});

    type_ref_t proc2 =
        tstore_add_proc(&ts, tstore_add_args(&ts, proc2_args), u32, false);
    assert_uint32(proc2.id, ==, 11);

    da_args_t proc3_args = da_init(alloc);
    da_push_back(&proc3_args, (type_proc_arg_t){.type = i32});

    type_ref_t proc3 =
        tstore_add_proc(&ts, tstore_add_args(&ts, proc3_args), i32, false);
    assert_uint32(proc3.id, ==, 10);

    da_args_t proc4_args = da_init(alloc);
    da_push_back(&proc4_args, (type_proc_arg_t){.type = u32});
    da_push_back(&proc4_args, (type_proc_arg_t){.type = u32});

    type_ref_t proc4 =
        tstore_add_proc(&ts, tstore_add_args(&ts, proc4_args), u32, false);
    assert_uint32(proc4.id, ==, 11);

    tstore_deinit(&ts);
    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_tstore_str(MunitParameter const params[],
                                   void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    tstore_t ts;
    tstore_init(&ts, alloc);

    type_ref_t ty;

    ty = tstore_add_int(&ts, true, 8);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "i8");

    ty = tstore_add_int(&ts, false, 8);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "u8");

    ty = tstore_add_int(&ts, true, 16);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "i16");

    ty = tstore_add_int(&ts, false, 16);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "u16");

    ty = tstore_add_int(&ts, true, 32);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "i32");

    ty = tstore_add_int(&ts, false, 32);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "u32");

    ty = tstore_add_int(&ts, true, 64);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "i64");

    ty = tstore_add_int(&ts, false, 64);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "u64");

    ty = tstore_add_ptr(&ts, 0, ts.builtins.i32, 0);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "*i32");

    ty = tstore_add_ptr(&ts, PTR_MULTI, ts.builtins.i32, 0);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "[*]i32");

    ty = tstore_add_ptr(&ts, PTR_SLICE, ts.builtins.i32, 0);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items, "[]i32");

    ty = tstore_add_ptr(&ts, PTR_CONST, ts.builtins.i32, 0);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items,
                        "*const i32");

    ty = tstore_add_ptr(&ts, PTR_CONST | PTR_MULTI | PTR_TERM, ts.builtins.i32,
                        0);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items,
                        "[*:0]const i32");

    ty = tstore_add_ptr(&ts, PTR_CONST | PTR_SLICE | PTR_TERM, ts.builtins.i32,
                        0);
    assert_string_equal(tstore_type_ref_str(&ts, ty, alloc).items,
                        "[:0]const i32");

    tstore_deinit(&ts);
    arena_clear_all(&arena);

    return MUNIT_OK;
}

void test_tstore_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/tstore/type_kind_str",
                          test_type_kind_to_str,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tstore/create",
                          test_tstore_create,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tstore/ints",
                          test_tstore_ints,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tstore/ptr",
                          test_tstore_ptr,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tstore/proc",
                          test_tstore_proc,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/tstore/str",
                          test_tstore_str,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
