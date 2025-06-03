#include <fmt/format.h>

#include "argparse.hpp"
#include "tests.hpp"

auto main(int argc, char** argv) -> int {
    auto args = argparse(argc, argv);

    auto top = ut::group("");
    ut::add(top, yal::tests::file_store());
    ut::add(top, yal::tests::integration());

    auto opt = ut::Options{
        .verbose = args.verbose,
        .ask = args.ask,
        .diff = args.diff,
    };

    auto result = ut::run(opt, top);

    if (args.verbose) fmt::println(stderr, "done!");

    return result.has_failures();
}
