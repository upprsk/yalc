#pragma once

#include <string_view>

#include "arena.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"
#include "symbol.hpp"

namespace yal::ast::sort {

struct Options {
    bool verbose_deps = false;
    bool verbose_sort = false;
};

void perform_sort(ErrorReporter& er, Module const& module, Options const& opt = {});

}  // namespace yal::ast::sort
