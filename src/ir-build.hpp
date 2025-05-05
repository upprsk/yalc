#pragma once

#include "ast.hpp"
#include "ir.hpp"
#include "yal.hpp"

namespace yal::ir {

auto build(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
           types::TypeStore& ts, Options const& opt) -> Module;
}
