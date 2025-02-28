#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "types.hpp"

namespace yal {

auto pass_add_types(NodeHandle n, Ast& ast, TypeStore& ts, ErrorReporter& er)
    -> TypeHandle;

}  // namespace yal
