#include <memory>
#include <optional>

#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/ranges.h"
#include "parser.hpp"
#include "sema.hpp"
#include "tokenizer.hpp"
#include "types.hpp"
#include "typing.hpp"
#include "utils.hpp"

auto main(int argc, char** argv) -> int {
    CPPTRACE_TRY {
        if (argc < 2) {
            fmt::println(stderr, "usage: {} <program>", argv[0]);
            return 1;
        }

        auto contents = yal::read_entire_file(argv[1]);
        if (!contents) {
            fmt::println(stderr, "failed to read file: {}", argv[1]);
            return 1;
        }

        auto er = yal::ErrorReporter{*contents, argv[1]};
        auto tokens = yal::tokenize(*contents, er);
        auto [ast, root] = yal::parse(tokens, *contents, er);

        fmt::println("{}", yal::FatNodeHandle{.ast = &ast, .node = root});

        auto ts = yal::TypeStore::new_store();
        auto m = yal::sema(ast, ts, root, er);

        fmt::println("{}",
                     yal::FatNodeHandle{.ast = &ast, .node = root, .ts = &ts});

        for (auto const& func : m.funcs) {
            func.disasm(stdout, ts);
            fmt::println(stdout, "");
        }

        {
            auto                                   dotgraph = "ast.gv";
            std::unique_ptr<FILE, void (*)(FILE*)> f = {
                fopen(dotgraph, "wb"), [](auto f) { fclose(f); }};
            if (!f) {
                fmt::println(stderr, "failed to open: {}", dotgraph);
                return 1;
            }

            ast.dump_dot(f.get(), root, &ts);
        }

        return er.had_error() ? 1 : 0;
    }
    CPPTRACE_CATCH(std::exception const& ex) {
        fmt::println(stderr, "exception: {}", ex.what());
        cpptrace::from_current_exception().print();

        return 1;
    }
}
