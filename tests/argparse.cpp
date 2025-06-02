#include "argparse.hpp"

#include <fmt/base.h>

#include <libassert/assert.hpp>

#include "utils.hpp"

using fmt::println;

void print_usage(std::string_view self) {
    println(stderr, "usage: {} [options] <program>", self);
}

void print_help(std::string_view self) {
    print_usage(self);

    // clang-format off
    println(stderr, "");
    println(stderr, "options:");
    println(stderr, "    -h,--help: show this message and exit.");
    println(stderr, "    --usage: show usage and exit.");
    println(stderr, "    --verbose: show more output (on stderr).");
    println(stderr, "    --file: single file compilation mode.");
    println(stderr, "    --dump <step>: dump the result of an internal compilation step. This option");
    println(stderr, "        accepts a list of options separated by a comma: step1,step2. The option");
    println(stderr, "        can also be passed multiple times: --dump step1 --dump step2");
    println(stderr, "        available steps:");
    println(stderr, "            tokens: dump tokenization result.");
    println(stderr, "            ast: dump parsed AST.");
    println(stderr, "            top-name-res: dump top-level name-resolved AST.");
    println(stderr, "            attributes: dump AST after attributes are applied.");
    println(stderr, "            name-res: dump AST full name resolutiofull name resolution.");
    println(stderr, "            sema: dump the AST of each function after semantic-analysis.");
    println(stderr, "            ir: dump the intermediate representation.");
    println(stderr, "            ir-lower: dump the intermediate representation after lowering.");
    println(stderr, "    -o,--output: path to output file (QBE)");
    // clang-format on
}

auto argparse(int argc, char** argv) -> Args {
    auto it = yal::ArgIterator{.argc = argc, .argv = argv};

    std::string_view self;
    DEBUG_ASSERT(it.next(self), "no argv[0]");

    Args args;

    std::string_view arg;
    while (it.next(arg)) {
        if (arg == "-h" || arg == "--help") {
            print_help(self);
            std::exit(0);
        }

        if (arg == "--verbose") {
            args.verbose = true;
        } else if (arg == "--ask") {
            args.ask = true;
        } else if (arg == "--diff") {
            args.diff = true;
        } else {
            println(stderr, "error: unknown option: '{:?}'", arg);
            std::exit(1);
        }
    }

    return args;
}
