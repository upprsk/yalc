#pragma once

#include "ast.hpp"
#include "yal.hpp"

namespace yal::compile::qbe {

void compile(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
             types::TypeStore& ts, Options const& opt);

}  // namespace yal::compile::qbe
