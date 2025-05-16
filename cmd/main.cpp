#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <memory>
#include <optional>

#include "argparser.hpp"
#include "ast.hpp"
#include "codegen/codegen_qbe.hpp"
#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "fmt/ranges.h"
#include "ir-build.hpp"
#include "ir.hpp"
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
    if (er.had_error()) {
        if (args.dump_ast_json) {
            json j = *prj_root;
            j["ds"] = *ast.get_decl_store();
            fmt::println("{}", j.dump());
        }
        return 1;
    }

    if (args.dont_lower && args.dump_ast_json) {
        json j = *prj_root;
        j["ds"] = *ast.get_decl_store();
        fmt::println("{}", j.dump());
        return 0;
    }

    yal::lower::lower_ast(ast, prj_root, er, ts, opt);
    if (args.dump_ast_json) {
        json j = *prj_root;
        j["ds"] = *ast.get_decl_store();
        fmt::println("{}", j.dump());
        return 0;
    }

    if (er.had_error()) return 1;

    auto module = yal::ir::build(ast, prj_root, er, ts, opt);
    if (er.had_error()) return 1;

    if (args.dump_ir_module) {
        yal::ir::disasm_module(stdout, module);
        // NOTE: when dumping IR module, do not compile down to QBE
        return 0;
    }

    auto out = stdout;
    if (!args.output.empty()) {
        out = fopen(args.output.c_str(), "wb");
    }

    yal::codegen::qbe::codegen(out, module, er, ts, opt);

    if (!args.output.empty()) {
        fclose(out);
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
