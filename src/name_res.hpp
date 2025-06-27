#pragma once

#include "ast.hpp"
#include "decl.hpp"
#include "error_reporter.hpp"
#include "node.hpp"

namespace yal {

struct NameResOptions {
    bool verbose = false;
    bool log_decl_dependencies = false;
    bool dump_dependencies_as_mermaid = false;
};

auto sort_declarations_and_resolve_top_level(ast::Ast& ast, DeclStore& ds,
                             std::span<ast::NodeFile* const> root,
                             ErrorReporter& er, NameResOptions const& opt)
    -> ast::NodeFlatModule*;

}  // namespace yal
