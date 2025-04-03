#pragma once

#include <filesystem>
#include <utility>

#include "ast-node-id.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"

namespace yal {

struct Options {
    bool dump_tokens;
    bool dump_ast;
    bool dump_ast_json;
};

auto load_and_parse_into_ast(FileStore& fs, ErrorReporter& er,
                             std::filesystem::path filepath, ast::Ast& dst_ast,
                             Options const& opt) -> ast::NodeId;

auto load_and_parse(FileStore& fs, ErrorReporter& er,
                    std::filesystem::path filepath, Options const& opt)
    -> std::pair<ast::Ast, ast::NodeId>;

}  // namespace yal
