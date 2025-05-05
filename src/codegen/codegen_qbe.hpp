#pragma once

#include "ast.hpp"
#include "ir.hpp"
#include "yal.hpp"

namespace yal::codegen::qbe {

void codegen(FILE* out, ir::Module const& module, ErrorReporter& er,
             types::TypeStore& ts, Options const& opt);

}  // namespace yal::codegen::qbe
