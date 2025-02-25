#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yal {

auto parse(std::span<const Token> tokens,std::string_view source, ErrorReporter& er)
    -> std::pair<Ast, NodeHandle>;

}  // namespace yal
