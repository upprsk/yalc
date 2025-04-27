#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <memory>
#include <optional>

#include "argparser.hpp"
#include "ast.hpp"
#include "compile/compile_qbe.hpp"
#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "fmt/ranges.h"
#include "lower.hpp"
#include "name-res.hpp"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "sema.hpp"
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
                            .single_file = args.single_file,
                            .dump_type_store = args.dump_type_store_json};

    auto [ast, root] = yal::load_and_parse(fs, er, args.program, opt);
    if (!root) return 1;

    auto ts = yal::types::TypeStore{};
    auto prj_root = yal::resolve_names(ast, root, er, fs, ts, opt);

    yal::sema::sema_ast(ast, prj_root, er, ts, opt);
    // if (args.dump_ast_json) {
    //     json j = *prj_root;
    //     j["ds"] = *ast.get_decl_store();
    //     fmt::println("{}", j.dump());
    // }

    if (er.had_error()) return 1;

    yal::lower::lower_ast(ast, prj_root, er, ts, opt);
    if (args.dump_ast_json) {
        json j = *prj_root;
        j["ds"] = *ast.get_decl_store();
        fmt::println("{}", j.dump());
        return 0;
    }

    if (er.had_error()) return 1;

    yal::compile::qbe::compile(ast, prj_root, er, ts, opt);

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
