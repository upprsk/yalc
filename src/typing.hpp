#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "yal.hpp"

namespace yal {

void perform_typing(ast::Ast& ast, ast::NodeId root, ErrorReporter& er,
                    Options const& opt);

}  // namespace yal
