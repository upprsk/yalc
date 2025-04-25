#pragma once

#include "ast.hpp"
#include "yal.hpp"

namespace yal::lower {

void lower_ast(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
               types::TypeStore& ts, Options const& opt);

}  // namespace yal::lower
