#include <fmt/base.h>
#include <fmt/format.h>

#include <filesystem>
#include <nlohmann/json.hpp>
#include <string_view>

#include "argparser.hpp"
#include "ast.hpp"
#include "ast_sort.hpp"
#include "error_reporter.hpp"
#include "file_store.hpp"
#include "node.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include "symbol.hpp"
#include "tokenizer.hpp"
#include "types.hpp"

void print_directory_info(yal::FileStore const&      fs,
                          yal::FileStore::Dir const& dir) {
    fmt::println(stderr, "program directory: {} ({} files)", dir.full_path,
                 dir.files.size());

    for (auto fileid : dir.files) {
        auto f = fs.get_file_by_id(fileid);
        fmt::println(stderr, "- file: {} ({}B)", f->original_path,
                     f->contents.size());
    }
}

void print_modules_info(yal::FileStore const& fs,
                        std::string_view      root_module_name) {
    auto modules = fs.get_all_modules();
    fmt::println(stderr, "scan found {} modules (root is {:?})", modules.size(),
                 root_module_name);

    for (auto const& module : modules) {
        fmt::println(stderr, "module {:?}", module.name);
        for (auto const& fid : module.files) {
            auto f = fs.get_file_by_id(fid);
            fmt::println(stderr, "- {}", f->original_path);
        }
    }
}

auto tokenize_and_parse(yalc::Args const&              args,
                        yal::LocalErrorReporter const& er) -> yal::ast::File {
    auto tokens = yal::tokenize(er);
    if (args.dump.has_tokens()) {
        nlohmann::json j = tokens;
        fmt::println("{}", j.dump(2));
    }

    auto file_ast = yal::ast::File{};
    yal::parse_into_ast_file(tokens, file_ast, er,
                             {.verbose = args.verbose.has_parser()});

    return file_ast;
}

auto parse_module_name(yalc::Args const&              args,
                       yal::LocalErrorReporter const& er)
    -> yal::ast::ModuleDecl {
    // TODO: make something more efficient than tokenizing the entire file. Or
    // we can cache this!
    auto tokens = yal::tokenize(er);
    if (args.dump.has_tokens()) {
        nlohmann::json j = tokens;
        fmt::println("{}", j.dump(2));
    }

    auto mod = yal::parse_module_declaration(tokens, er);
    // TODO: do we want to dump this like we do with ast?

    return mod;
}

void find_modules_in_dir(yalc::Args const& args, yal::ErrorReporter& er,
                         yal::FileStore& fs, yal::FileStore::Dir const& dir) {
    for (auto file_id : dir.files) {
        auto mod = parse_module_name(args, er.for_file(file_id));
        if (mod.name.empty()) {
            if (args.verbose.has_yalc()) {
                fmt::println(stderr,
                             "failed to parse module declaration of: {}",
                             fs.get_file_by_id(file_id)->original_path);
            }

            continue;
        }

        auto mod_id = fs.find_module(mod.name, dir.id);
        if (mod_id.is_invalid()) {
            // create a new module for it
            auto m = fs.add_module(mod.name, dir.id);
            fs.add_file_to_module(m, file_id);
            continue;
        }

        fs.add_file_to_module(mod_id, file_id);
    }
}

auto create_module_from_files(yalc::Args const& args, yal::FileStore const& fs,
                              yal::ErrorReporter& er,
                              yal::ModuleId       root_module_id,
                              yal::ast::File      root_file_ast)
    -> yal::ast::Module {
    auto root_module = fs.get_module_by_id(root_module_id);
    DEBUG_ASSERT(root_module != nullptr);

    std::vector<yal::ast::File> files_of_module;
    files_of_module.push_back(std::move(root_file_ast));
    for (auto fid : root_module->files) {
        if (fid != root_file_ast.get_module_name_loc().fileid) {
            auto file_ast = tokenize_and_parse(args, er.for_file(fid));
            files_of_module.push_back(std::move(file_ast));
        }
    }

    return yal::ast::Module{.name = std::string{root_module->name},
                            .files = std::move(files_of_module)};
}

auto main_single_file(yalc::Args const& args, yal::FileStore& fs,
                      yal::ErrorReporter& er) -> int {
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

    auto file_ast = tokenize_and_parse(args, er.for_file(id));
    if (args.dump.has_ast()) {
        nlohmann::json j = file_ast;
        fmt::println("{}", j.dump(2));
    }

    std::vector<yal::ast::File> files;
    files.push_back(std::move(file_ast));

    auto module = yal::ast::Module{
        .name = std::string{file_ast.get_module_name()},
        .files = std::move(files),
    };

    auto flat_module =
        yal::ast::sort::perform_sort(er, std::move(module),
                                     {.verbose_deps = args.verbose.has_deps(),
                                      .verbose_sort = args.verbose.has_sort()});
    if (args.dump.has_module()) {
        nlohmann::json j = flat_module;
        fmt::println("{}", j.dump(2));
    }

    yal::sema::perform_sema(er, flat_module);

    return 0;
}

auto main_default(yalc::Args const& args, yal::FileStore& fs,
                  yal::ErrorReporter& er) -> int {
    auto root_fid = fs.add_file(args.program);
    if (root_fid.is_invalid()) {
        fmt::println(stderr, "invalid file: {}", args.program);
        return 1;
    }

    auto dir_id = fs.get_dir_containing(root_fid);
    auto dir = fs.get_dir_by_id(dir_id);
    DEBUG_ASSERT(dir.has_value());
    if (args.verbose.has_yalc()) print_directory_info(fs, *dir);

    auto root_file_ast = tokenize_and_parse(args, er.for_file(root_fid));
    if (root_file_ast.get_module_name().empty()) {
        fmt::println(stderr, "can not determine module of: {}", args.program);
        fmt::println(stderr, "forgot `module module_name;`?");

        return 1;
    }

    auto root_module_id =
        fs.add_module(root_file_ast.get_module_name(), dir_id);

    find_modules_in_dir(args, er, fs, *dir);
    if (args.verbose.has_yalc()) {
        print_modules_info(fs, root_file_ast.get_module_name());
    }

    auto module = create_module_from_files(args, fs, er, root_module_id,
                                           std::move(root_file_ast));
    if (args.dump.has_ast()) {
        nlohmann::json j = module;
        fmt::println("{}", j.dump(2));
    }

    auto flat_module =
        yal::ast::sort::perform_sort(er, std::move(module),
                                     {.verbose_deps = args.verbose.has_deps(),
                                      .verbose_sort = args.verbose.has_sort()});
    if (args.dump.has_module()) {
        nlohmann::json j = flat_module;
        fmt::println("{}", j.dump(2));
    }

    yal::sema::perform_sema(er, flat_module);

    return 0;
}

auto main(int argc, char** argv) -> int {
    auto args = yalc::argparse(argc, argv);
    if (args.verbose.has_yalc()) fmt::println(stderr, "args: {}", args);

    auto fs = yal::FileStore{};
    auto ss = yal::SymbolStore{};
    auto er = yal::ErrorReporter{&fs, stderr, args.error_format};

    // in case we are in single file mode, we want to add just the given file,
    // and not scan anything other than imports otherwise we want to add the
    // given directory
    if (args.single_file) {
        if (int r = main_single_file(args, fs, er)) return r;
    } else {
        if (int r = main_default(args, fs, er)) return r;
    }

    if (args.verbose.has_yalc()) fmt::println(stderr, "done!");
    return 0;
}
