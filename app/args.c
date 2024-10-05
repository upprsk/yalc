#include "args.h"

#include <stdio.h>

#include "munit.h"
#include "utils/string.h"

argparse_result_t parse_args(int argc, char* argv[], args_t* args) {
    munit_assert_not_null(args);

    args->self = shift_args(&argc, &argv);

    for (;;) {
        char const* arg = shift_args(&argc, &argv);
        if (!arg) break;

        if (streq(arg, "-h") || streq(arg, "--help")) {
            args->help = true;

            // clang-format off
            fprintf(stderr, "usage: %s <root filename> [options]\n", args->self);
            fprintf(stderr, "      -h, --help          show this message\n");
            fprintf(stderr, "      -o <filename>       set name of output file\n");
            fprintf(stderr, "      --show-tokens       show all tokens from file\n");
            // clang-format on

            // as this is a "stop the world flag, we break here
            break;
        }

        if (streq(arg, "-o")) {
            args->output_filename = shift_args(&argc, &argv);
            if (!args->output_filename) {
                fprintf(stderr, "'-o' requires a filename as argument\n");
                return ARGPARSE_ERR;
            }
        }

        else if (streq(arg, "--show-tokens")) {
            args->show_tokens = true;
        }

        else if (streq(arg, "--show-ast")) {
            args->show_ast = true;
        }

        else {
            if (args->root_filename) {
                fprintf(stderr, "invalid argument: %s\n", arg);
                return ARGPARSE_ERR;
            }

            args->root_filename = arg;
        }
    }

    return ARGPARSE_OK;
}
