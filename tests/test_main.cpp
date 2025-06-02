#include <fmt/format.h>

#include "argparse.hpp"

auto main(int argc, char** argv) -> int {
    auto args = argparse(argc, argv);

    fmt::println("[test] Hello, World!");

    if (args.verbose) fmt::println(stderr, "done!");
    return 0;
}
