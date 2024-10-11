#include <stdio.h>

#include "alloc/allocator.h"
#include "ast.h"
#include "errors.h"
#include "parser.h"
#include "slice/slice.h"
#include "test.h"
#include "tokenizer.h"

static MunitResult test_ast_kind_to_str(MunitParameter const params[],
                                        void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    // FIXME: add the missing nodes
    assert_string_equal(node_kind_str(NODE_INVAL), "NODE_INVAL");
    assert_string_equal(node_kind_str(NODE_MOD), "NODE_MOD");
    assert_string_equal(node_kind_str(NODE_DECL), "NODE_DECL");
    assert_string_equal(node_kind_str(NODE_PROC), "NODE_PROC");
    assert_string_equal(node_kind_str(NODE_CALL), "NODE_CALL");
    assert_string_equal(node_kind_str(NODE_BLK), "NODE_BLK");
    assert_string_equal(node_kind_str(NODE_ARG), "NODE_ARG");
    assert_string_equal(node_kind_str(NODE_ADD), "NODE_ADD");
    assert_string_equal(node_kind_str(NODE_SUB), "NODE_SUB");
    assert_string_equal(node_kind_str(NODE_MUL), "NODE_MUL");
    assert_string_equal(node_kind_str(NODE_DIV), "NODE_DIV");
    assert_string_equal(node_kind_str(NODE_NEG), "NODE_NEG");
    assert_string_equal(node_kind_str(NODE_NOT), "NODE_NOT");
    assert_string_equal(node_kind_str(NODE_RET), "NODE_RET");
    assert_string_equal(node_kind_str(NODE_IDENT), "NODE_IDENT");
    assert_string_equal(node_kind_str(NODE_INT), "NODE_INT");

    return MUNIT_OK;
}

static node_ref_t tokenize_and_parse(allocator_t alloc, ast_t* ast,
                                     str_t source) {
    FILE* errstream = tmpfile();
    assert_not_null(errstream);

    error_reporter_t er = {.stream = errstream, .source = source};

    slice_token_t tokens = tokenize(&er, alloc, source);
    assert_not_null(tokens.ptr);

    long errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    node_ref_t ast_root = parse(&(parser_desc_t){.alloc = alloc,
                                                 .temp_alloc = alloc,
                                                 .source = source,
                                                 .tokens = tokens,
                                                 .er = &er},
                                ast);
    assert_uint32(ast_root.id, !=, 0);

    errstream_size = ftell(errstream);
    assert_long(errstream_size, ==, 0);
    assert_size(er.error_count, ==, 0);

    return ast_root;
}

static MunitResult test_parser_empty(MunitParameter const params[],
                                     void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] = "";
    uint32_t   source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items, "(mod)");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_basic_decls(MunitParameter const params[],
                                           void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "// full\n"
        "a: i32 : 10;\n"
        "b: u8 : 5321;\n"
        "// no type\n"
        "c:  : 5321;\n"
        "yo :: 1234;\n"
        "// no init\n"
        "   abc: i32;\n"
        "// extern\n"
        "extern thing: i32;\n"
        "extern \"yo\" test: u64;\n"
        "// variable\n"
        "extern \"var_test\" var_test: u64 = undefined;\n"
        "extern var_test_2: u64 = undefined;\n"
        "g_thing: i32 = 0;\n"
        "g_stuff := 0; // this is i32 as well\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(
    //     s.items,
    //     "(mod"
    //     " (decl \"a\" i32 10)"
    //     " (decl \"b\" u8 5321)"
    //     " (decl \"c\" nil 5321)"
    //     " (decl \"yo\" nil 1234)"
    //     " (decl \"abc\" i32 nil)"
    //     " (decl \"thing\" (extern) i32 nil)"
    //     " (decl \"test\" (extern \"yo\") u64 nil)"
    //     " (decl var \"var_test\" (extern \"var_test\") u64 undefined)"
    //     " (decl var \"var_test_2\" (extern) u64 undefined)"
    //     " (decl var \"g_thing\" i32 0)"
    //     " (decl var \"g_stuff\" nil 0)"
    //     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_min_main(MunitParameter const params[],
                                        void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "main :: .() i32 {\n"
        "    return 0;\n"
        "};\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"main\" nil (proc i32 (blk"
    //                     " (return 0)))"
    //                     ")"
    //                     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_calls(MunitParameter const params[],
                                     void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "f :: .(x: i32) i32 { return x * 2; };\n"
        "\n"
        "main :: .() i32 {\n"
        "    r := f(32);\n"
        "    return 0;\n"
        "};\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"f\" nil (proc (arg x i32) i32 (blk"
    //                     " (return (mul x 2)))))"
    //                     " (decl \"main\" nil (proc i32 (blk"
    //                     " (decl var \"r\" nil (call f 32))"
    //                     " (return 0)))))");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

// same as test_parser_calls but with trailing commas the ast should be the
// exact same.
static MunitResult test_parser_trailing_commas(MunitParameter const params[],
                                               void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "f :: .(x: i32,) i32 { return x * 2; };\n"
        "\n"
        "main :: .() i32 {\n"
        "    r := f(32,);\n"
        "    return 0;\n"
        "};\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"f\" nil (proc (arg x i32) i32 (blk"
    //                     " (return (mul x 2)))))"
    //                     " (decl \"main\" nil (proc i32 (blk"
    //                     " (decl var \"r\" nil (call f 32))"
    //                     " (return 0)))))");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_ptrs(MunitParameter const params[],
                                    void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "a :: *u8;\n"
        "b :: *const u8;\n"
        "c :: []u8;\n"
        "d :: []const u8;\n"
        "e :: [*]u8;\n"
        "f :: [*]const u8;\n"
        "g :: [*:0]u8;\n"
        "h :: [*:0]const u8;\n"
        "i :: [][]u8;\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"a\" nil (ptr u8))"
    //                     " (decl \"b\" nil (ptr const u8))"
    //                     " (decl \"c\" nil (ptr slice u8))"
    //                     " (decl \"d\" nil (ptr slice const u8))"
    //                     " (decl \"e\" nil (ptr multi u8))"
    //                     " (decl \"f\" nil (ptr multi const u8))"
    //                     " (decl \"g\" nil (ptr multi 0 u8))"
    //                     " (decl \"h\" nil (ptr multi const 0 u8))"
    //                     " (decl \"i\" nil (ptr slice (ptr slice u8)))"
    //                     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_extern_printf(MunitParameter const params[],
                                             void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "extern printf: .(format: [*:0]const u8, ...) i32 ...;\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"printf\" (extern) (proc"
    //                     " (arg format (ptr multi const 0 u8)) ... i32 nil)"
    //                     " nil)"
    //                     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_if(MunitParameter const params[],
                                  void*                user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "main :: .() {\n"
        "    if true {\n"
        "    }\n"
        "\n"
        "    if false {\n"
        "    } else {\n"
        "    }\n"
        "};\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"main\" nil (proc nil (blk"
    //                     " (if true (blk))"
    //                     " (if false (blk) (blk))"
    //                     ")))"
    //                     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_while(MunitParameter const params[],
                                     void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "main :: .() {\n"
        "    while true {\n"
        "    }\n"
        "};\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"main\" nil (proc nil (blk"
    //                     " (while true (blk))"
    //                     ")))"
    //                     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

static MunitResult test_parser_assign(MunitParameter const params[],
                                      void* user_data_or_fixture) {
    (void)params;
    (void)user_data_or_fixture;

    return MUNIT_SKIP;

    char const source[] =
        "main :: .() {\n"
        "    i := 0;\n"
        "    while true {\n"
        "        i = i + 1;\n"
        "    }\n"
        "};\n"
        //
        ;
    uint32_t source_len = sizeof(source) - 1;

    arena_allocator_t arena = {0};
    allocator_t       alloc = arena_allocator(&arena);

    ast_t      ast = {0};
    node_ref_t ast_root =
        tokenize_and_parse(alloc, &ast, (str_t){source, source_len});

    // string_t s = ast_dump(&ast, ast_root, alloc);
    // assert_string_equal(s.items,
    //                     "(mod"
    //                     " (decl \"main\" nil (proc nil (blk"
    //                     " (decl var \"i\" nil 0)"
    //                     " (while true (blk"
    //                     " (assign i (add i 1))))"
    //                     ")))"
    //                     ")");

    arena_clear_all(&arena);

    return MUNIT_OK;
}

void test_parser_add_tests(ctx_t* ctx) {
    ctx_add_test(ctx, &(MunitTest){
                          "/ast/ast-kind-to-str",
                          test_ast_kind_to_str,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/empty",
                          test_parser_empty,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/basic_decls",
                          test_parser_basic_decls,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/min-main",
                          test_parser_min_main,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/calls",
                          test_parser_calls,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/trailing_commas",
                          test_parser_trailing_commas,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/ptrs",
                          test_parser_ptrs,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/while",
                          test_parser_while,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/if",
                          test_parser_if,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/assign",
                          test_parser_assign,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });

    ctx_add_test(ctx, &(MunitTest){
                          "/parser/extern_printf",
                          test_parser_extern_printf,
                          NULL,
                          NULL,
                          MUNIT_TEST_OPTION_NONE,
                          NULL,
                      });
}
