#include "argparser.hpp"

#include <cstdio>
#include <string_view>

#include "fmt/base.h"
#include "types.hpp"
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
    println(stderr, "    --dump-tokens: dump tokenization result.");
    println(stderr, "    --dump-ast: dump parsed AST.");
    println(stderr, "    --dump-ast-dot: dump parsed AST as a dot graph.");
}

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

        if (arg == "--dump-tokens") {
            args.dump_tokens = true;
        } else if (arg == "--dump-ast") {
            args.dump_ast = true;
        } else if (arg == "--dump-ast-json") {
            args.dump_ast_json = true;
        } else if (arg == "--dump-ast-dot") {
            args.dump_ast_dot = true;
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
