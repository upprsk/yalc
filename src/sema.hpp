#pragma once

#include <string_view>

#include "arena.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"
#include "symbol.hpp"

namespace yal::sema {

struct Options {
    bool verbose_coercions = false;
};

void perform_sema(ErrorReporter& er, ast::FlatModule const& module,
                  Options const& opts = {});

}  // namespace yal::sema
