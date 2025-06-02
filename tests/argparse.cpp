#include "argparse.hpp"

#include <fmt/base.h>

#include <algorithm>
#include <libassert/assert.hpp>

#include "tests.hpp"
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
    println(stderr, "    -v,--verbose: show more output (on stderr). Each time");
    println(stderr, "        this flag is passed, verbosity is increased (max {})", ut::VERBOSE_LEVEL_MAX);
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

        if (arg == "--usage") {
            print_usage(self);
            std::exit(0);
        }

        if (arg == "--verbose") {
            args.verbose += 1;
        } else if (arg.starts_with("-v")) {
            auto n = std::ranges::count_if(arg.substr(2),
                                           [](char c) { return c != 'v'; });
            if (n > 0) {
                // had some character that was not a 'v'
                println(stderr, "error: unknown option: {:?}", arg);
                std::exit(1);
            }

            args.verbose += arg.size() - 1;
        } else if (arg == "--ask") {
            args.ask = true;
        } else if (arg == "--diff") {
            args.diff = true;
        } else {
            println(stderr, "error: unknown option: {:?}", arg);
            std::exit(1);
        }
    }

    return args;
}
