#include <cstdio>
#include <filesystem>
#include <memory>
#include <optional>

#include "argparser.hpp"
#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "fmt/ranges.h"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"
#include "types.hpp"
#include "utils.hpp"

using json = nlohmann::json;

auto real_main(yalc::Args const& args) -> int {
    // NOTE: if we have something like include paths, this is where we add them
    auto fs = yal::FileStore{};

    auto root = fs.add(args.program);
    if (!root.is_valid()) {
        fmt::println(stderr, "error: failed to open/read {}", args.program);
        return 1;
    }

    auto er = yal::ErrorReporter{fs, stderr};
    auto er_for_root = er.for_file(root);
    auto tokens = yal::tokenize(er_for_root.get_source(), er_for_root);
    er.update_error_count(er_for_root);

    if (args.dump_tokens) {
        fmt::println("{}", tokens);
    }

    auto [ast, ast_root] = yal::parse(tokens, er_for_root);
    er.update_error_count(er_for_root);

    if (args.dump_ast) {
        fmt::println(
            stderr, "NOTE: this has not been implemented, use --dump-ast-json");
        fmt::println("{}", ast.fatten(ast_root));
        return 1;
    }

    if (args.dump_ast_json) {
        json j = ast.fatten(ast_root);
        fmt::println("{}", j.dump());
    }

    yal::resolve_names(ast, ast_root, er, fs,
                       {.dump_tokens = args.dump_tokens,
                        .dump_ast = args.dump_ast,
                        .dump_ast_json = args.dump_ast_json});

    return er.had_error() ? 1 : 0;
}

auto main(int argc, char** argv) -> int {
    CPPTRACE_TRY {
        auto args = yalc::argparse(argc, argv);
        return real_main(args);
    }
    CPPTRACE_CATCH(std::exception const& ex) {
        fmt::println(stderr, "{} {}",
                     fmt::styled("expection:", fmt::fg(fmt::color::red)),
                     ex.what());
        cpptrace::from_current_exception().print();

        return 1;
    }
}
