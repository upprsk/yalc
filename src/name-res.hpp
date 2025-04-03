#pragma once

#include "ast-node-id.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "yal.hpp"

namespace yal {

auto resolve_names(ast::Ast& ast, ast::NodeId root, ErrorReporter& er,
                   FileStore& fs, Options const& opt) -> ast::NodeId;

}  // namespace yal
