#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "hlir.hpp"
#include "types.hpp"

namespace yal {

auto sema(Ast& ast, TypeStore& ts, NodeHandle root, ErrorReporter& er)
    -> hlir::Module;

}  // namespace yal
