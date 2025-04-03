#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yal {

auto parse(std::span<Token const> tokens, ErrorReporterForFile& er)
    -> std::pair<ast::Ast, ast::NodeId>;

auto parse_into(std::span<Token const> tokens, ast::Ast& ast,
                ErrorReporterForFile& er) -> ast::NodeId;

}  // namespace yal
