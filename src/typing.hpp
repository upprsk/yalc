#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "types.hpp"
#include "yal.hpp"

namespace yal {

void perform_typing(ast::Ast& ast, ast::NodeId root, ErrorReporter& er,
                    types::TypeStore& ts, Options const& opt);

}  // namespace yal
