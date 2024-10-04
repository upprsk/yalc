#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc/allocator.h"
#include "args.h"
#include "ast.h"
#include "errors.h"
#include "parser.h"
#include "slice/slice.h"
#include "span.h"
#include "tokenizer.h"
#include "utils/fs.h"

static char const* make_sanitized_name(allocator_t alloc, char const* name) {
    ssize_t len = strlen(name);

    size_t file_start = 0;
    size_t file_end = len;

    for (ssize_t i = len - 1; i > 0; i--) {
        if (name[i] == '/') {
            file_start = i + 1;
            break;
        }
    }

    for (ssize_t i = file_start; i < len; i++) {
        if (name[i] == '.') {
            file_end = i;
            break;
        }
    }

    size_t size = file_end - file_start;
    char*  buf = allocator_alloc(alloc, size + 1);
    memcpy(buf, name + file_start, size);
    buf[size] = 0;

    return buf;
}

int main(int argc, char* argv[]) {
    args_t args = {0};
    if (parse_args(argc, argv, &args)) return EXIT_FAILURE;
    if (args.help) return EXIT_SUCCESS;

    if (!args.root_filename) {
        fprintf(stderr, "error: missing root file\n");
        return EXIT_FAILURE;
    }

    allocator_t c_alloc = c_allocator();

    slice_char_t raw_source = {0};
    if (read_entire_file(args.root_filename, c_alloc, &raw_source)) {
        fprintf(stderr, "failed to open %s\n", args.root_filename);
        return EXIT_FAILURE;
    }

    str_t source = {raw_source.ptr, raw_source.len};

    error_reporter_t er = {
        .stream = stderr, .filename = args.root_filename, .source = source};

    slice_token_t tokens = tokenize(&er, c_alloc, source);
    if (args.show_tokens) {
        slice_foreach(tokens, i) {
            token_t tok = slice_at(tokens, i);
            str_t   tok_slice = span_to_slice(tok.span, source);

            printf("[%zu] %s (%d) '%.*s'\n", i, token_to_str(tok.type),
                   tok.type, (int)tok_slice.len, tok_slice.ptr);
        }
    }

    arena_allocator_t main_arena = {0};
    arena_allocator_t temp_arena = {0};

    allocator_t main_alloc = arena_allocator(&main_arena);
    allocator_t temp_alloc = arena_allocator(&temp_arena);

    size_t errs = er.error_count;

    ast_t      ast = {0};
    node_ref_t ast_root = parse(&(parser_desc_t){.alloc = main_alloc,
                                                 .temp_alloc = temp_alloc,
                                                 .source = source,
                                                 .tokens = tokens,
                                                 .er = &er},
                                &ast);
    if (errs != er.error_count) return EXIT_FAILURE;

    string_t s = ast_dump(&ast, ast_root, temp_alloc);
    printf("ast:\n%s\n", s.items);

    arena_clear_all(&main_arena);
    arena_clear_all(&temp_arena);

    slice_free(c_alloc, &tokens);
    slice_free(c_alloc, &source);

    return EXIT_SUCCESS;
}
