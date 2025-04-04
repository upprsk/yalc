#pragma once

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ast-node-id.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"

namespace yal::sort {

auto topo_sort_top_decls(ast::Ast& ast, ast::NodeId mod, ErrorReporter& er,
                         FileStore& fs) -> std::vector<ast::NodeId>;

}  // namespace yal::sort
