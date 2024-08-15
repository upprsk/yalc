#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "allocator.h"
#include "ast.h"
#include "errors.h"
#include "parser.h"
#include "tokenizer.h"
#include "typecheck.h"
#include "typestore.h"

static char* read_entire_file(char const* filename, allocator_t alloc,
                              size_t* len) {
    FILE* f = fopen(filename, "rb");
    if (!f) return NULL;

    char* buf = NULL;

    fseek(f, 0, SEEK_END);
    long tell_len = ftell(f);
    if (tell_len < 0) goto error;
    fseek(f, 0, SEEK_SET);

    buf = allocator_alloc(alloc, tell_len + 1);
    if (!buf) goto error;

    long read = fread(buf, sizeof(char), tell_len, f);
    if (read != tell_len) goto error;

    *len = tell_len;
    buf[tell_len] = 0;

    return buf;

error:
    if (f) fclose(f);
    if (buf) allocator_free(alloc, buf);

    return NULL;
}

char const* shift_args(int* argc, char*** argv) {
    *argc -= 1;

    char const* data = *argv[0];
    *argv += 1;

    return data;
}

static inline bool streq(char const* a, char const* b) {
    return strcmp(a, b) == 0;
}

int main(int argc, char* argv[]) {
    char const* filename = NULL;

    char const* self_path = shift_args(&argc, &argv);

    for (;;) {
        char const* arg = shift_args(&argc, &argv);
        if (!arg) break;

        if (streq(arg, "-h") || streq(arg, "--help")) {
            fprintf(stderr, "usage: %s <root filename> [options]\n", self_path);
            fprintf(stderr, "      -h, --help      show this message\n");

            return EXIT_SUCCESS;
        }

        if (filename) {
            fprintf(stderr, "error: root filename already provided: %s\n",
                    filename);
            return EXIT_FAILURE;
        }

        filename = arg;
    }

    if (!filename) {
        fprintf(stderr, "error: no input files\n");
        return EXIT_FAILURE;
    }

    allocator_t alloc;
    allocator_init_stdc(&alloc);

    size_t      source_len = 0;
    char const* source = read_entire_file(filename, alloc, &source_len);
    if (!source) {
        fprintf(stderr, "error: failed to open: %s\n", filename);
        return EXIT_FAILURE;
    }

    error_reporter_t er = {.stream = stderr};

    token_t* tokens = tokenize(&er, alloc, filename, source, source_len);
    if (!tokens) {
        fprintf(stderr, "failed to tokenize\n");
        return 1;
    }

    // for (size_t i = 0; i < da_get_size(tokens); ++i) {
    //     printf("token (%d) [%d, %d]\n", tokens[i].type, tokens[i].span.start,
    //            tokens[i].span.end);
    // }

    Arena node_arena = {};

    node_t* ast = parse(&(parse_params_t){
        .tokens = tokens,
        .filename = filename,
        .source = source,
        .source_len = source_len,
        .er = &er,
        .arena = &node_arena,
    });

    // fprintf(stdout, "untyped ast:\n");
    // dump_node(stdout, ast, 0);

    typestore_t ts = {};
    typestore_init(&ts, alloc);

    pass_typecheck(&(typecheck_params_t){
        .ast = ast,
        .ts = &ts,
        .filename = filename,
        .source = source,
        .source_len = source_len,
        .er = &er,
    });

    fprintf(stdout, "typed ast:\n");
    dump_node(stdout, ast, 0);

    // fprintf(stdout, "typestore:\n");
    // size_t size = da_get_size(ts.entries);
    // for (size_t i = 0; i < size; ++i) {
    //     type_t* type = &ts.entries[i].type;
    //
    //     char const* typestr = typestore_type_to_str(&ts, alloc, type);
    //     fprintf(stdout, "[%d] '%s': %s\n", ts.entries[i].id.id,
    //             type_tag_to_str(type->tag), typestr);
    //     allocator_free(alloc, (char*)typestr);
    // }

    // fprintf(stdout, "wasm:\n");
    // codegen_wasm(&(codegen_params_t){
    //     .ast = ast,
    //     .out = stdout,
    //     .ts = &ts,
    // });

    typestore_deinit(&ts);
    arena_free(&node_arena);

    return 0;
}
