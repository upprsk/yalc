#include <fmt/base.h>
#include <fmt/format.h>

#include <filesystem>
#include <nlohmann/json.hpp>

#include "argparser.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "file_store.hpp"
#include "name_res.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

auto main(int argc, char** argv) -> int {
    auto args = yalc::argparse(argc, argv);
    if (args.verbose) fmt::println(stderr, "args: {}", args);

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{&fs, stderr, args.error_format};

    // in case we are in single file mode, we want to add just the given file,
    // otherwise we want to add the given directory
    if (args.single_file) {
        auto id = fs.add_file(args.program);
        if (id.is_invalid()) {
            fmt::println(stderr, "invalid file: {}", args.program);

            if (std::filesystem::is_directory(args.program)) {
                fmt::println(
                    stderr, "note: {} is a directory, maybe try without --file",
                    args.program);
            }

            return 1;
        }

        if (args.verbose) {
            auto f = fs.get_file_by_id(id);
            fmt::println(stderr, "program: {} ({}B)", f->full_path,
                         f->contents.size());
        }

        auto tokens = yal::tokenize(er.for_file(id));
        if (args.dump_tokens) {
            nlohmann::json j = tokens;
            fmt::println("{}", j.dump(2));
        }

        auto ast = yal::ast::Ast{};
        auto root = yal::parse_into_ast(tokens, ast, er.for_file(id),
                                        {.verbose = args.verbose_parser});

        if (args.dump_parsed_ast) {
            nlohmann::json j = *root;
            fmt::println("{}", j.dump(2));
        }

        auto mod = yal::perform_name_resolution(
            ast, std::array{root}, er, {.verbose = args.dump_named_ast});
        if (args.dump_named_ast) {
            nlohmann::json j = *mod;
            fmt::println("{}", j.dump(2));
        }

        // do not compile, just analyse and report
        if (args.just_analyse) {
        }

    } else {
        auto id = fs.add_dir(args.program);
        if (id.is_invalid()) {
            fmt::println(stderr, "invalid directory: {}", args.program);

            if (std::filesystem::is_regular_file(args.program)) {
                fmt::println(
                    stderr, "note: {} is a regular file, maybe try with --file",
                    args.program);
            }

            return 1;
        }

        if (args.verbose) {
            auto d = fs.get_dir_by_id(id);
            fmt::println(stderr, "program directory: {} ({} files)",
                         d->full_path, d->files.size());

            for (auto fileid : d->files) {
                auto f = fs.get_file_by_id(fileid);
                fmt::println(stderr, "- file: {} ({}B)", f->full_path,
                             f->contents.size());
            }
        }
    }

    if (args.verbose) fmt::println(stderr, "done!");
    return 0;
}
