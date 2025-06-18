#pragma once

#include <span>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"
#include "tokenizer.hpp"

namespace yal {

auto parse_into_ast(std::span<Token const> tokens, ast::Ast& ast,
                    LocalErrorReporter const& er) -> ast::NodeFile*;

}  // namespace yal
