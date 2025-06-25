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

auto parse_into_ast(std::span<Token const> tokens, ast::Ast& ast,
                    LocalErrorReporter const& er, ParseOptions const& opt = {})
    -> ast::NodeFile*;

}  // namespace yal
