#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "ast.h"
#include "codegen/codegen_mips.h"
#include "common.h"
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

typedef enum outopt {
    OUT_NONE,
    OUT_MIPS,
} outopt_t;

int main(int argc, char* argv[]) {
    char const* filename = NULL;
    bool        show_tokens = false;
    bool        show_ast = false;
    bool        show_typed_ast = false;
    bool        show_typestore = false;
    outopt_t    out = 0;

    char const* self_path = shift_args(&argc, &argv);

    for (;;) {
        char const* arg = shift_args(&argc, &argv);
        if (!arg) break;

        if (streq(arg, "-h") || streq(arg, "--help")) {
            // clang-format off
            fprintf(stderr, "usage: %s <root filename> [options]\n", self_path);
            fprintf(stderr, "      -h, --help          show this message\n");
            fprintf(stderr, "      --show-tokens       show all tokens from file\n");
            fprintf(stderr, "      --show-ast          show the parse tree\n");
            fprintf(stderr, "      --show-typed-ast    show the typed AST\n");
            fprintf(stderr, "      --show-typestore    show all of the types defined in the typestore\n");
            fprintf(stderr, "      --output-mips       output MIPS32 assembly\n");
            // clang-format on

            return EXIT_SUCCESS;
        }

        if (streq(arg, "--show-tokens")) {
            show_tokens = true;
        }

        else if (streq(arg, "--show-typed-ast")) {
            show_typed_ast = true;
        }

        else if (streq(arg, "--show-ast")) {
            show_ast = true;
        }

        else if (streq(arg, "--show-typestore")) {
            show_typestore = true;
        }

        else if (streq(arg, "--output-mips")) {
            if (out) {
                fprintf(stderr, "output mode already specified\n");
                return EXIT_FAILURE;
            }

            out = OUT_MIPS;
        }

        else if (filename) {
            fprintf(stderr, "error: root filename already provided: %s\n",
                    filename);
            return EXIT_FAILURE;
        }

        else
            filename = arg;
    }

    if (!filename) {
        fprintf(stderr, "error: no input files\n");
        return EXIT_FAILURE;
    }

    if (!out &&
        !(show_tokens || show_ast || show_typed_ast || show_typestore)) {
        fprintf(stderr, "error: no output selected\n");
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
        return EXIT_FAILURE;
    }

    if (show_tokens) {
        for (size_t i = 0; i < da_get_size(tokens); ++i) {
            printf("token %s (%d) [%d, %d]\n", token_to_str(tokens[i].type),
                   tokens[i].type, tokens[i].span.start, tokens[i].span.end);
        }
    }

    Arena       node_arena = {0};
    allocator_t node_alloc = {};
    allocator_init_arena(&node_alloc, &node_arena);

    node_t* ast = parse(&(parse_params_t){
        .tokens = tokens,
        .filename = filename,
        .source = source,
        .source_len = source_len,
        .node_alloc = node_alloc,
        .er = &er,
    });

    if (show_ast) {
        fprintf(stdout, "untyped ast:\n");
        dump_node(stdout, ast, 0);
    }

    if (er.error_count > 0) return EXIT_FAILURE;

    typestore_t ts = {};
    typestore_init(&ts, alloc);

    pass_typecheck(&(typecheck_params_t){
        .ast = ast,
        .ts = &ts,
        .filename = filename,
        .source = source,
        .source_len = source_len,
        .alloc = node_alloc,
        .er = &er,
    });

    if (show_typed_ast) {
        fprintf(stdout, "typed ast:\n");
        dump_node(stdout, ast, 0);
    }

    if (show_typestore) {
        Arena       arena = {};
        allocator_t temp = {};
        allocator_init_arena(&temp, &arena);

        fprintf(stderr, "typestore:\n");
        size_t size = da_get_size(ts.entries);
        for (size_t i = 0; i < size; ++i) {
            type_t* type = &ts.entries[i].type;

            char const* typestr = typestore_type_to_str(&ts, temp, type);
            fprintf(stdout, "[%d] '%s': %s\n", ts.entries[i].id.id,
                    type_tag_to_str(type->tag), typestr);
        }

        arena_free(&arena);
    }

    if (er.error_count > 0) return EXIT_FAILURE;

    if (out == OUT_MIPS) {
        codegen_mips(&(codegen_mips_params_t){
            .ast = ast,
            .ts = &ts,
            .filename = filename,
            .source = source,
            .source_len = source_len,
            .er = &er,
            .out = stdout,
        });
    }

    typestore_deinit(&ts);
    arena_free(&node_arena);

    return EXIT_SUCCESS;
}
