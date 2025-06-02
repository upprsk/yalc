#include <fmt/format.h>

#include "argparser.hpp"
#include "file_store.hpp"

auto main(int argc, char** argv) -> int {
    auto args = yalc::argparse(argc, argv);
    if (args.verbose) fmt::println(stderr, "args: {}", args);

    auto fs = yal::FileStore{};

    fmt::println("Hello, World!");

    if (args.verbose) fmt::println(stderr, "done!");
    return 0;
}
