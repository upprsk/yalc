#pragma once

#include <filesystem>
#include <utility>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"

namespace yal {

struct Options {
    bool dump_tokens = false;
    bool dump_ast = false;
    bool dump_each_ast_json = false;
    bool single_file = false;
    bool dump_type_store = false;
    bool verbose_sema = false;
    bool verbose_lowering = false;
    bool verbose_ir_build = true;
};

auto load_and_parse_into_ast(FileStore& fs, ErrorReporter& er, FileId fileid,
                             ast::Ast& dst_ast, Options const& opt)
    -> ast::Node*;

auto load_and_parse(FileStore& fs, ErrorReporter& er, FileId fileid,
                    Options const& opt) -> std::pair<ast::Ast, ast::Node*>;

}  // namespace yal
