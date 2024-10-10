#include <stdio.h>

#include "alloc/allocator.h"
#include "da/da.h"
#include "ir.h"
#include "slice/slice.h"
#include "test.h"

static MunitResult test_irop_str(MunitParameter const params[],
                                 void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    assert_string_equal("IR_INVAL", irop_str(IR_INVAL));
    // FIXME: add the other things

    return MUNIT_OK;
}

static MunitResult test_inst_str(MunitParameter const params[],
                                 void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    arena_allocator_t arena = {};
    allocator_t       alloc = arena_allocator(&arena);

    inst_t inst_arr[] = {
        inst_copy(IRT_I32, 2, 123), inst_copy(IRT_U8, 5, 0),
        inst_move(IRT_U8, 2, 0),    inst_move(IRT_I32, 6, 2),
        inst_add(IRT_I32, 3, 0, 2), inst_sub(IRT_U64, 5, 0, 2),
        inst_mul(IRT_I32, 3, 0, 2), inst_div(IRT_U64, 5, 0, 2),
        inst_slt(IRT_I32, 3, 0, 2), inst_sle(IRT_I32, 5, 0, 2),
        inst_sgt(IRT_I32, 3, 0, 2), inst_sge(IRT_I32, 5, 0, 2),
        inst_seq(IRT_I32, 3, 0, 2), inst_sne(IRT_I32, 5, 0, 2),
        inst_bz(IRT_I32, 2, 1),     inst_bnz(IRT_I32, 2, 2),
        inst_b(IRT_U32, 4),         inst_call(IRT_U32, 4, 0, 2, 2),
        inst_ret(IRT_I32, 23),
    };
    size_t inst_count = (sizeof inst_arr / sizeof inst_arr[0]);

    char const* expected[] = {
        "$2 = copy<i32> 123",
        "$5 = copy<u8> 0",
        "$2 = move<u8> $0",
        "$6 = move<i32> $2",
        "$3 = add<i32> $0, $2",
        "$5 = sub<u64> $0, $2",
        "$3 = mul<i32> $0, $2",
        "$5 = div<u64> $0, $2",
        "$3 = slt<i32> $0, $2",
        "$5 = sle<i32> $0, $2",
        "$3 = sgt<i32> $0, $2",
        "$5 = sge<i32> $0, $2",
        "$3 = seq<i32> $0, $2",
        "$5 = sne<i32> $0, $2",
        "bz $2, @1",
        "bnz $2, @2",
        "b @4",
        "$4 = call<u32> 0[2], $2",
        "ret $23",
    };
    assert_size(inst_count, ==, (sizeof expected / sizeof expected[0]));

    slice_inst_t insts = {inst_arr, inst_count};

    slice_foreach(insts, i) {
        string_t s = inst_str(slice_at(insts, i), alloc);
        assert_string_equal(expected[i], s.items);
    }

    arena_clear_all(&arena);

    return MUNIT_OK;
}

void test_ir_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/ir/irop_str",
                          test_irop_str,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/ir/inst_str",
                          test_inst_str,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
