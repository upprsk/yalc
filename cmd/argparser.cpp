#include "argparser.hpp"

#include <cstdio>
#include <string_view>

#include "fmt/base.h"
#include "libassert/assert.hpp"
#include "utils.hpp"

namespace yalc {
using fmt::println;

void print_usage(std::string_view self) {
    println(stderr, "usage: {} [options] <program>", self);
}

void print_help(std::string_view self) {
    print_usage(self);
    println(stderr, "");
    println(stderr, "options:");
    println(stderr, "    -h,--help: show this message and exit.");
    println(stderr, "    --usage: show usage and exit.");
    println(stderr, "    --single-file: single file compilation mode.");
    println(stderr, "    --dump-tokens: dump tokenization result.");
    println(stderr, "    --dump-ast: dump parsed AST.");
    println(stderr, "    --dump-ast-json: dump parsed AST as json.");
    println(stderr,
            "    --dont-lower: only for use with --dump-ast-json. Will not "
            "lower the AST before printing.");
    println(stderr,
            "    --dump-individual-ast-json: dump parsed AST of each file as "
            "each is parsed as json.");
    println(stderr, "    --dump-ast-dot: dump parsed AST as a dot graph.");
    println(stderr,
            "    --dump-type-store-json: dump the entire type store as json.");
    println(stderr, "    --dump-ir-module: dump the entire IR module.");
    println(stderr, "    -o,--output: path to output file (QBE)");
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto argparse(int argc, char** argv) -> Args {
    auto it = yal::ArgIterator{.argc = argc, .argv = argv};

    std::string_view self;
    ASSERT(it.next(self), "no argv[0]");

    Args args;

    std::string_view arg;
    while (it.next(arg)) {
        if (arg == "-h" || arg == "--help") {
            print_help(self);
            exit(0);
        }

        if (arg == "--usage") {
            print_usage(self);
            exit(0);
        }

        if (arg == "--single-file") {
            args.single_file = true;
        } else if (arg == "--dump-tokens") {
            args.dump_tokens = true;
        } else if (arg == "--dump-ast") {
            args.dump_ast = true;
        } else if (arg == "--dump-ast-json") {
            args.dump_ast_json = true;
        } else if (arg == "--dont-lower") {
            args.dont_lower = true;
        } else if (arg == "--dump-individual-ast-json") {
            args.dump_individual_ast_json = true;
        } else if (arg == "--dump-ast-dot") {
            args.dump_ast_dot = true;
        } else if (arg == "--dump-type-store-json") {
            args.dump_type_store_json = true;
        } else if (arg == "--dump-ir-module") {
            args.dump_ir_module = true;
        } else if (arg == "-o" || arg == "--output") {
            if (!args.output.empty()) {
                fmt::println(stderr, "error: output already provided");
                exit(1);
            }

            if (!it.next(arg)) {
                fmt::println(stderr, "error: missing output file path");
                exit(1);
            }

            args.output = arg;
        } else if (args.program.empty()) {
            args.program = arg;
        } else {
            println(stderr, "error: unknown option: '{:?}'", arg);
            exit(1);
        }
    }

    return args;
}

}  // namespace yalc
