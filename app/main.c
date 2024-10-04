#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc/allocator.h"
#include "args.h"
#include "da/da.h"
#include "errors.h"
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

    da_token_t tokens = tokenize(&er, c_alloc, source);
    if (args.show_tokens) {
        da_foreach(&tokens, i) {
            token_t tok = da_at(tokens, i);
            str_t   tok_slice = span_to_slice(tok.span, source);

            printf("[%u] %s (%d) '%.*s'\n", i, token_to_str(tok.type), tok.type,
                   (int)tok_slice.len, tok_slice.ptr);
        }
    }

    da_free(&tokens);
    slice_free(c_alloc, source);

    return EXIT_SUCCESS;
}
