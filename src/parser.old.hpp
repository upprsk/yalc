#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yal {

auto parse_expr(std::span<Token const> tokens, std::string_view source,
                ErrorReporter& er) -> std::pair<Ast, NodeHandle>;

auto parse(std::span<Token const> tokens, std::string_view source,
           ErrorReporter& er) -> std::pair<Ast, NodeHandle>;

}  // namespace yal
