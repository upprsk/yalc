#include <fmt/base.h>
#include <fmt/format.h>

#include <filesystem>
#include <nlohmann/json.hpp>
#include <string_view>

#include "argparser.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "file_store.hpp"
#include "parser.hpp"
#include "symbol.hpp"
#include "tokenizer.hpp"

auto ingest_file(yalc::Args const& args, yal::LocalErrorReporter const& er)
    -> yal::ast::File {
    auto tokens = yal::tokenize(er);
    if (args.dump.has_tokens()) {
        nlohmann::json j = tokens;
        fmt::println("{}", j.dump(2));
    }

    auto file_ast = yal::ast::File{};
    yal::parse_into_ast_file(tokens, file_ast, er,
                             {.verbose = args.verbose.has_parser()});

    if (args.dump.has_ast()) {
        nlohmann::json j = file_ast;
        fmt::println("{}", j.dump(2));
    }

    return file_ast;
}

auto main(int argc, char** argv) -> int {
    auto args = yalc::argparse(argc, argv);
    if (args.verbose.has_yalc()) fmt::println(stderr, "args: {}", args);

    auto fs = yal::FileStore{};
    auto er = yal::ErrorReporter{&fs, stderr, args.error_format};

    // in case we are in single file mode, we want to add just the given file,
    // and not scan anything other than imports otherwise we want to add the
    // given directory
    if (args.single_file) {
        auto id = fs.add_file(args.program);
        if (id.is_invalid()) {
            fmt::println(stderr, "invalid file: {}", args.program);
            return 1;
        }

        if (args.verbose.has_yalc()) {
            auto f = fs.get_file_by_id(id);
            fmt::println(stderr, "program: {} ({}B)", f->full_path,
                         f->contents.size());
        }

        ingest_file(args, er.for_file(id));
    } else {
        auto id = fs.add_file(args.program);
        if (id.is_invalid()) {
            fmt::println(stderr, "invalid file: {}", args.program);
            return 1;
        }

        auto dir_id = fs.get_dir_containing(id);
        auto dir = fs.get_dir_by_id(dir_id);
        if (args.verbose.has_yalc()) {
            fmt::println(stderr, "program directory: {} ({} files)",
                         dir->full_path, dir->files.size());

            for (auto fileid : dir->files) {
                auto f = fs.get_file_by_id(fileid);
                fmt::println(stderr, "- file: {} ({}B)", f->full_path,
                             f->contents.size());
            }
        }

        ingest_file(args, er.for_file(id));
    }

    if (args.verbose.has_yalc()) fmt::println(stderr, "done!");
    return 0;
}
