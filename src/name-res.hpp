#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "types.hpp"
#include "yal.hpp"

namespace yal {

auto resolve_names(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
                   FileStore& fs, types::TypeStore& ts, Options const& opt)
    -> ast::Node*;

}  // namespace yal
