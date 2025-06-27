#include "name_res.hpp"

#include "ast.hpp"
#include "error_reporter.hpp"
#include "node.hpp"

namespace yal {

class NameRes {
    ast::Ast&             ast;
    ErrorReporter const&  er;
    NameResOptions const& opt;

public:
    NameRes(ErrorReporter const& er, ast::Ast& ast, NameResOptions const& opt)
        : ast{ast}, er{er}, opt{opt} {}

    auto run(ast::NodeFile* root) -> ast::NodeFlatModule* {
        return ast.new_node_flat_module(root->get_loc(), root->get_children(),
                                        root->get_module_name());
    }
};

auto perform_name_resolution(ast::Ast&                       ast,
                             std::span<ast::NodeFile* const> root,
                             ErrorReporter const& er, NameResOptions const& opt)
    -> ast::NodeFlatModule* {
    auto nres = NameRes{er, ast, opt};
    return nres.run(root[0]);
}

}  // namespace yal
