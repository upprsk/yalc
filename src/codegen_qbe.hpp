#pragma once

#include "error_reporter.hpp"
#include "hlir.hpp"

namespace yal::codegen::qbe {

void codegen(hlir::Module const& mod, TypeStore const& ts, ErrorReporter& er,
             FILE* f);

}  // namespace yal::codegen::qbe
