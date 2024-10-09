#pragma once

#include <stdbool.h>

// Store all of the command line arguments that can be given to the program.
typedef struct args {
    // argv[0]
    char const* self;
    // <root filename>
    char const* root_filename;
    // -o <filename>
    char const* output_filename;

    // -h,--help
    bool help;

    // --show-tokens
    bool show_tokens;
    // --show-ast
    bool show_ast;
    // --show-typed-ast
    bool show_typed_ast;
} args_t;

// The result from parsing args.
typedef enum argparse_result {
    ARGPARSE_OK,
    ARGPARSE_ERR,
} argparse_result_t;

// Shift the arguments, effectivelly popping the first value.
static inline char const* shift_args(int* argc, char*** argv) {
    *argc -= 1;

    char const* data = *argv[0];
    *argv += 1;

    return data;
}

// Parse argc and argv into the `args` structure.
argparse_result_t parse_args(int argc, char* argv[], args_t* args);
