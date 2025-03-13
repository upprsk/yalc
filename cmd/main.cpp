#include <memory>
#include <optional>

#include "argparser.hpp"
#include "codegen_qbe.hpp"
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
        auto args = yalc::argparse(argc, argv);

        auto contents = yal::read_entire_file(args.program);
        if (!contents) {
            fmt::println(stderr, "failed to read file: {}", args.program);
            return 1;
        }

        auto er = yal::ErrorReporter{*contents, args.program};
        auto tokens = yal::tokenize(*contents, er);
        if (args.dump_tokens) {
            fmt::println("{}", tokens);
        }

        auto [ast, root] = yal::parse(tokens, *contents, er);
        if (args.dump_ast) {
            fmt::println("{}", yal::FatNodeHandle{.ast = &ast, .node = root});
        }

        auto ts = yal::TypeStore::new_store();
        auto m = yal::sema(ast, ts, root, er);

        fmt::println(stderr, "{}",
                     yal::FatNodeHandle{.ast = &ast, .node = root, .ts = &ts});

        for (auto const& func : m.funcs) {
            func.disasm(stderr, ts);
        }

        yal::codegen::qbe::codegen(m, ts, er, stdout);

        return er.had_error() ? 1 : 0;
    }
    CPPTRACE_CATCH(std::exception const& ex) {
        fmt::println(stderr, "exception: {}", ex.what());
        cpptrace::from_current_exception().print();

        return 1;
    }
}
