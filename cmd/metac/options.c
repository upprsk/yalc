#include "options.h"
#include "da.h"
#include "string_view.h"
#include <stdio.h>

void print_usage(string_view self) {
    fprintf(stderr, "usage: %.*s [options] <input_files...> <output_file>\n",
            (int)self.len, self.ptr);
}

void print_help(void) {
    fprintf(stderr, "options:\n");
    fprintf(stderr, "    -h,--help: show this message\n");
    fprintf(stderr, "    -vo,--verbose-options: print the parsed options\n");
    fprintf(stderr, "    -vp,--verbose-parse: print messages when parsing\n");
    fprintf(stderr, "    -vs,--verbose-search: print message when searching "
                    "for declarations\n");
    fprintf(stderr, "    -vl,--verbose-paths: print the full location paths "
                    "for everything\n");
    fprintf(stderr, "    -vall: Maximum verbosity\n");
    fprintf(stderr,
            "    --no-verbose-paths: Do not print full location of paths.\n");
    fprintf(stderr, "    --clang-args: extra args to be passed to clang (like "
                    "include paths). This should be the last argument, as "
                    "everything after is gobbled up.\n");
    fprintf(stderr, "    --clang-resource-dir: resource dir for clang (use "
                    "clang -print-resource-dir to get it)\n");
}

void options_destroy(Options *opt) {
    da_destroy(&opt->input_files);
    da_destroy(&opt->extra_clang_args);
}

void options_dump(FILE *o, Options const *opt) {
    fprintf(o, "options={\n");
    fprintf(o, "  verbose-options=%s,\n",
            opt->flags & OPTION_VERBOSE_OPTIONS ? "yes" : "no");
    fprintf(o, "  verbose-parse=%s,\n",
            opt->flags & OPTION_VERBOSE_PARSE ? "yes" : "no");
    fprintf(o, "  verbose-search=%s,\n",
            opt->flags & OPTION_VERBOSE_SEARCH ? "yes" : "no");
    fprintf(o, "  clang-resource-dir='%.*s',\n",
            (int)opt->clang_resource_dir.len, opt->clang_resource_dir.ptr);
    fprintf(o, "  input_files = [\n");
    for (size_t i = 0; i < opt->input_files.len; ++i) {
        fprintf(o, "    '%.*s',\n", (int)opt->input_files.items[i].len,
                opt->input_files.items[i].ptr);
    }
    fprintf(o, "  ],\n");
    fprintf(o, "  output_file = '%.*s',\n}\n", (int)opt->output_file.len,
            opt->output_file.ptr);
}

Options parse_options(int argc, char **argv) {
    string_view self = string_view_from_cstr(*argv);

    argc--;
    argv++;

    Options options = {
        .clang_resource_dir = string_view_from_cstr("/usr/bin/../lib/clang/20"),
    };

    while (argc) {
        string_view arg = string_view_from_cstr(*argv);
        // fprintf(stderr, "arg='%.*s'\n", (int)arg.len, arg.ptr);

        if (string_view_eq(arg, string_view_from_cstr("-h")) ||
            string_view_eq(arg, string_view_from_cstr("--help"))) {
            print_usage(self);
            print_help();
            exit(0);
        }

        else if (string_view_eq(arg, string_view_from_cstr("-vo")) ||
                 string_view_eq(arg,
                                string_view_from_cstr("--verbose-options"))) {
            options.flags |= OPTION_VERBOSE_OPTIONS;
        }

        else if (string_view_eq(arg, string_view_from_cstr("-vp")) ||
                 string_view_eq(arg,
                                string_view_from_cstr("--verbose-parse"))) {
            options.flags |= OPTION_VERBOSE_PARSE;
        }

        else if (string_view_eq(arg, string_view_from_cstr("-vs")) ||
                 string_view_eq(arg,
                                string_view_from_cstr("--verbose-search"))) {
            options.flags |= OPTION_VERBOSE_SEARCH;
        }

        else if (string_view_eq(arg, string_view_from_cstr("-vl")) ||
                 string_view_eq(arg,
                                string_view_from_cstr("--verbose-paths"))) {
            options.flags |= OPTION_VERBOSE_SEARCH;
        }

        else if (string_view_eq(arg, string_view_from_cstr("-vall"))) {
            options.flags |= OPTION_VERBOSE_OPTIONS | OPTION_VERBOSE_PARSE |
                             OPTION_VERBOSE_SEARCH | OPTION_VERBOSE_PATHS;
        }

        else if (string_view_eq(arg,
                                string_view_from_cstr("--no-verbose-paths"))) {
            options.flags &= ~OPTION_VERBOSE_PATHS;
        }

        else if (string_view_eq(arg,
                                string_view_from_cstr("--print-all-types"))) {
            options.flags |= OPTION_PRINT_ALL_TYPES;
        }

        else if (string_view_eq(arg, string_view_from_cstr("--clang-args"))) {
            argc--;
            argv++;

            while (argc > 1) {
                arg = string_view_from_cstr(*argv);
                da_append(&options.extra_clang_args, arg.ptr);

                argc--;
                argv++;
            }

            arg = string_view_from_cstr(*argv);
            da_append(&options.extra_clang_args, arg.ptr);
        }

        else if (string_view_eq(
                     arg, string_view_from_cstr("--clang-resource-dir"))) {
            argc--;
            argv++;

            if (argc == 0) {
                fprintf(stderr,
                        "error: missing argument for --clang-resource-dir\n");
                exit(1);
            }

            options.clang_resource_dir = string_view_from_cstr(*argv);
        }

        else {
            da_append(&options.input_files, arg);
        }

        argc--;
        argv++;
    }

    if (options.input_files.len < 1) {
        fprintf(stderr, "error: missing input files\n");
        exit(1);
    }

    if (options.input_files.len < 2) {
        fprintf(stderr, "error: missing output file\n");
        exit(1);
    }

    options.output_file = da_last(&options.input_files);
    da_pop(&options.input_files);

    return options;
}
