#include <cstdio>
#include <string_view>

#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "fmt/format.h"
#include "nlohmann/json.hpp"
#include "test_helpers.hpp"
#include "test_lower.hpp"
#include "test_name_res.hpp"
#include "test_parser.hpp"
#include "test_sema.hpp"
#include "test_tokenizer.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;

struct Args {
    bool                     verbose = false;
    bool                     ask = false;
    bool                     diff = false;
    std::vector<std::string> filters;
};

void print_usage(std::string_view self) {
    fmt::println(stderr, "usage: {} [options] <program>", self);
}

void print_help(std::string_view self) {
    using fmt::println;

    print_usage(self);
    println(stderr, "");
    println(stderr, "options:");
    println(stderr, "    -h,--help: show this message and exit.");
    println(stderr, "    --usage: show usage and exit.");
    println(stderr, "    -v,--verbose: show output of all failed tests.");
    println(stderr,
            "    --ask: when a test fails, ask to update the expected value.");
    println(stderr,
            "    --diff: when a test fails, show a diff between expected value "
            "and what we got.");
    println(stderr,
            "    -f,--filter: filter tests by tags or name. This flag can be "
            "passed multiple times to add more filters.");
}

auto argparse(int argc, char** argv) -> Args {
    auto it = yal::ArgIterator{.argc = argc, .argv = argv};

    std::string_view self;
    ASSERT(it.next(self), "no argv[0]");

    Args args;

    std::string_view arg;
    while (it.next(arg)) {
        if (arg == "--ask") {
            args.ask = true;
        } else if (arg == "--diff") {
            args.diff = true;
        } else if (arg == "-v" || arg == "--verbose") {
            args.verbose = true;
        } else if (arg == "--filter" || arg == "-f") {
            if (!it.next(arg)) {
                fmt::println(stderr, "error: missing argument to filter");
                exit(1);
            }

            args.filters.emplace_back(arg);
        } else if (arg == "--help" || arg == "-h") {
            print_help(self);
            exit(0);
        } else if (arg == "--usage") {
            print_help(self);
            exit(0);
        } else {
            fmt::println(stderr, "error: unknown option: '{:?}'", arg);
            exit(1);
        }
    }

    return args;
}

auto real_main(Args args) -> int {
    libassert::set_failure_handler(handle_assertion);

    int ok{};
    int failed{};

    TestParams p{.filters = {},
                 .verbose = args.verbose,
                 .use_diff = args.diff,
                 .ask_for_updates = args.ask};
    for (auto const& s : args.filters) p.filters.insert(s);

    {
        auto [tok, tfailed] = test_tokenizer(p);
        ok += tok;
        failed += tfailed;
    }

    {
        auto [tok, tfailed] = test_parser(p);
        ok += tok;
        failed += tfailed;
    }

    {
        auto [tok, tfailed] = test_name_res(p);
        ok += tok;
        failed += tfailed;
    }

    {
        auto [tok, tfailed] = test_sema(p);
        ok += tok;
        failed += tfailed;
    }

    {
        auto [tok, tfailed] = test_lower(p);
        ok += tok;
        failed += tfailed;
    }

    fmt::println("{} tests, {} success, {} failed", ok + failed, ok, failed);
    return failed > 0;
}

auto main(int argc, char** argv) -> int {
    CPPTRACE_TRY {
        auto args = argparse(argc, argv);
        return real_main(args);
    }
    CPPTRACE_CATCH(std::exception const& ex) {
        fmt::println(stderr, "{} {}",
                     fmt::styled("expection:", fmt::fg(fmt::color::red)),
                     ex.what());
        cpptrace::from_current_exception().print();

        return 1;
    }

    return 0;
}
