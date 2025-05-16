#include "yal.hpp"

#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

namespace yal {

auto load_and_parse_into_ast(FileStore& fs, ErrorReporter& er, FileId fileid,
                             ast::Ast& dst_ast, Options const& opt)
    -> ast::Node* {
    auto erf = er.for_file(fileid);
    auto tokens = tokenize(fs.get_contents(fileid), erf);
    er.update_error_count(erf);

    if (opt.dump_tokens) {
        fmt::println("{}", tokens);
    }

    auto file_root = parse_into(tokens, dst_ast, erf);
    er.update_error_count(erf);

    if (!file_root) {
        PANIC("NOTE: got no output from `parse_into`");
    }

    if (opt.dump_ast) {
        fmt::println(
            stderr, "NOTE: this has not been implemented, use --dump-ast-json");
        fmt::println("{}", *file_root);
    }

    if (opt.dump_each_ast_json) {
        json j = *file_root;
        json ds = *dst_ast.get_decl_store();
        fmt::println("{}\n{}", j.dump(), ds.dump());
    }

    return file_root;
}

auto load_and_parse(FileStore& fs, ErrorReporter& er, FileId fileid,
                    Options const& opt) -> std::pair<ast::Ast, ast::Node*> {
    ast::Ast ast;
    auto     root = load_and_parse_into_ast(fs, er, fileid, ast, opt);
    return std::make_pair(std::move(ast), root);
}

}  // namespace yal
