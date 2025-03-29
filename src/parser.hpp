#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yal {

auto parse(std::span<Token const> tokens, ErrorReporterForFile& er)
    -> std::pair<ast::Ast, ast::NodeId>;

}  // namespace yal
