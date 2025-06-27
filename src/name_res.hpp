#pragma once

#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"

namespace yal {

struct NameResOptions {
    bool verbose = false;
};

auto perform_name_resolution(ast::Ast&                       ast,
                             std::span<ast::NodeFile* const> root,
                             ErrorReporter const& er, NameResOptions const& opt)
    -> ast::NodeFlatModule*;

}  // namespace yal
