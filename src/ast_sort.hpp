#pragma once

#include <string_view>

#include "arena.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"
#include "symbol.hpp"

namespace yal::ast::sort {

void perform_sort(ErrorReporter& er, Module const& module);

}  // namespace yal::ast::sort
