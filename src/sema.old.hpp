#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "types.hpp"
#include "yal.hpp"

namespace yal::sema {

void sema_ast(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
              types::TypeStore& ts, Options const& opt);

}  // namespace yal::sema
