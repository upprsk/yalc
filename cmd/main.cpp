#include <fmt/format.h>

#include "argparser.hpp"

auto main(int argc, char** argv) -> int {
    auto args = yalc::argparse(argc, argv);
    if (args.verbose) fmt::println(stderr, "args: {}", args);

    fmt::println("Hello, World!");

    if (args.verbose) fmt::println(stderr, "done!");
    return 0;
}
