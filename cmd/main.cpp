#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <memory>
#include <optional>

#include "argparser.hpp"
#include "ast.hpp"
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
#include "utils.hpp"
#include "yal.hpp"

using json = nlohmann::json;

auto real_main(yalc::Args const& args) -> int {
    // NOTE: if we have something like include paths, this is where we add them
    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{fs, stderr};

    auto opt = yal::Options{.dump_tokens = args.dump_tokens,
                            .dump_ast = args.dump_ast,
                            .dump_each_ast_json = args.dump_individual_ast_json,
                            .single_file = args.single_file};

    auto [ast, root] = yal::load_and_parse(fs, er, args.program, opt);
    if (!root.is_valid()) return 1;

    auto prj_root = yal::resolve_names(ast, root, er, fs, opt);
    if (args.dump_ast_json) {
        json j = ast.fatten(prj_root);
        fmt::println("{}", j.dump());
    }

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
