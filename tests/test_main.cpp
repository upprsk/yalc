#include <fmt/format.h>

#include "argparse.hpp"
#include "tests.hpp"

auto main(int argc, char** argv) -> int {
    auto args = argparse(argc, argv);

    std::vector<ut::Test> tests;
    tests.push_back(yal::tests::file_store());
    tests.push_back(yal::tests::integration());

    auto opt = ut::Options{
        .verbose = args.verbose,
        .ask = args.ask,
        .diff = args.diff,
    };

    auto result = ut::run_tests(opt, ut::new_test("top", std::move(tests)));

    if (args.verbose) fmt::println(stderr, "done!");
    ut::print_result(result);

    return result.failed + result.crashed > 0;
}
