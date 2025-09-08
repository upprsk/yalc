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

auto perform_sort(ErrorReporter& er, Module&& module, Options const& opt = {})
    -> FlatModule;

}  // namespace yal::ast::sort
