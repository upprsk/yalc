#pragma once

#include "ast-node-id.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"

namespace yal {

auto resolve_names(ast::Ast& ast, ast::NodeId root, ErrorReporter& er)
    -> ast::NodeId;

}  // namespace yal
