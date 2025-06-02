#include <fmt/format.h>

#include "argparse.hpp"
#include "tests.hpp"

auto main(int argc, char** argv) -> int {
    auto args = argparse(argc, argv);

    std::vector<ut::Test> tests;
    tests.push_back(yal::tests::file_store());

    auto opt = ut::Options{.verbose = args.verbose};
    auto result = ut::run_tests(opt, ut::new_test("top", std::move(tests)));

    if (args.verbose) fmt::println(stderr, "done!");
    ut::print_result(result);

    return result.failed + result.crashed > 0;
}
