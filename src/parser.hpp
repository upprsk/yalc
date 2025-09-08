#pragma once

#include <span>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"
#include "tokenizer.hpp"

namespace yal {

struct ParseOptions {
    bool verbose{};
};

void parse_into_ast_file(std::span<Token const> tokens, ast::File& ast_file,
                         LocalErrorReporter const& er,
                         ParseOptions const&       opt = {});

auto parse_module_declaration(std::span<Token const>    tokens,
                              LocalErrorReporter const& er,
                              ParseOptions const& opt = {}) -> ast::ModuleDecl;

}  // namespace yal
