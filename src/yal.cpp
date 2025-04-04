#include "yal.hpp"

#include "ast-node-id.hpp"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"

namespace yal {

auto load_and_parse_into_ast(FileStore& fs, ErrorReporter& er,
                             std::filesystem::path filepath, ast::Ast& dst_ast,
                             Options const& opt) -> ast::NodeId {
    auto fileid = fs.add(filepath);
    if (!fileid.is_valid()) {
        fmt::println(stderr, "error: failed to open/read {}",
                     filepath.string());

        return ast::NodeId::invalid();
    }

    auto erf = er.for_file(fileid);
    auto tokens = tokenize(erf.get_source(), erf);
    er.update_error_count(erf);

    if (opt.dump_tokens) {
        fmt::println("{}", tokens);
    }

    auto file_root = parse_into(tokens, dst_ast, erf);
    er.update_error_count(erf);

    if (opt.dump_ast) {
        fmt::println(
            stderr, "NOTE: this has not been implemented, use --dump-ast-json");
        fmt::println("{}", dst_ast.fatten(file_root));
    }

    if (opt.dump_each_ast_json) {
        json j = dst_ast.fatten(file_root);
        fmt::println("{}", j.dump());
    }

    return file_root;
}

auto load_and_parse(FileStore& fs, ErrorReporter& er,
                    std::filesystem::path filepath, Options const& opt)
    -> std::pair<ast::Ast, ast::NodeId> {
    ast::Ast ast;
    auto     root = load_and_parse_into_ast(fs, er, filepath, ast, opt);
    return std::make_pair(ast, root);
}

}  // namespace yal
