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
#include "test_tokenizer.hpp"
#include "tokenizer.hpp"
#include "utils.hpp"

using json = nlohmann::json;

struct Args {
    bool ask = false;
};

auto argparse(int argc, char** argv) -> Args {
    auto             it = yal::ArgIterator{.argc = argc, .argv = argv};
    std::string_view arg;
    ASSERT(it.next(arg), "no argv[0]");

    Args args;

    while (it.next(arg)) {
        if (arg == "--ask") {
            args.ask = true;
        } else {
            fmt::println(stderr, "error: unknown option: '{:?}'", arg);
            exit(1);
        }
    }

    return args;
}

auto real_main(Args args) -> int {
    libassert::set_failure_handler(handle_assertion);

    TestParams p{.ask_for_updates = args.ask};
    test_tokenizer(p);

    return 0;
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
