#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "allocator.h"
#include "da.h"
#include "errors.h"
#include "parser.h"
#include "tokenizer.h"

static char* read_entire_file(char const* filename, allocator_t alloc,
                              size_t* len) {
    FILE* f = fopen(filename, "rb");
    if (!f) return NULL;

    char* buf = NULL;

    fseek(f, 0, SEEK_END);
    long tell_len = ftell(f);
    if (tell_len < 0) goto error;
    fseek(f, 0, SEEK_SET);

    buf = allocator_alloc(alloc, tell_len);
    if (!buf) goto error;

    long read = fread(buf, sizeof(char), tell_len, f);
    if (read != tell_len) goto error;

    *len = tell_len;

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

    token_t* tokens = tokenize(&er, filename, source);
    if (!tokens) {
        fprintf(stderr, "failed to tokenize\n");
        return 1;
    }

    for (size_t i = 0; i < da_get_size(tokens); ++i) {
        printf("token (%d) [%d, %d]\n", tokens[i].type, tokens[i].span.start,
               tokens[i].span.end);
    }

    parse(&er, filename, source, source_len, tokens);

    return 0;
}
