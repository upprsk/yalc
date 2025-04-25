#include "lower.hpp"

#include <ranges>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "types.hpp"
// FIXME: remove this
#include "nlohmann/json.hpp"

namespace yal::lower {

using ast::Ast;
using ast::Node;
using ast::NodeKind;
namespace conv = ast::conv;

namespace rv = std::ranges::views;

struct Context {
    ErrorReporter*    er{};
    types::TypeStore* ts{};
};

// FIXME: this is a copy from sema.cpp
auto count_init_exprs(conv::ExprPack const& pack) -> size_t {
    size_t expr_count{};
    for (auto init : pack.items) {
        auto ty = init->get_type();
        ASSERT(ty != nullptr);

        if (ty->is_pack()) {
            // multiple returns of some function
            expr_count += ty->inner.size();
        } else {
            expr_count++;
        }
    }

    return expr_count;
}

// ============================================================================

void visit_node(Ast& ast, Node* node, Context& ctx) {
    if (node == nullptr) return;

    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };
    auto visit_span = [&](Context& ctx, std::span<Node* const> nodes) {
        for (auto node : nodes) visit_node(ast, node, ctx);
    };

    auto visit_children = [&](Context& ctx, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, Context& ctx) {
                visit_node(ast, node, ctx);
            },
            ctx);
    };

    auto& er = *ctx.er;
    auto& ts = *ctx.ts;

    if (node->is_oneof(ast::NodeKind::Assign)) {
        auto data = conv::assign(*node);
        auto lhs = data.get_lhs().items;
        auto rhs = data.get_rhs().items;

        std::vector<Node*> items;
        for (size_t lhs_idx{}; auto [rhs_idx, rhs_item] : rv::enumerate(rhs)) {
            if (lhs_idx >= lhs.size()) break;

            auto rhs_type = rhs_item->get_type();

            // result of calling some function
            if (rhs_type->is_pack()) {
                PANIC("not implemented");
            }

            // a simple initializer expression
            else {
                if (lhs[lhs_idx]->is_oneof(ast::NodeKind::Id) &&
                    conv::id(*lhs[lhs_idx]).is_discard()) {
                    // discarding the value, add with discard

                    auto d = ast.new_discard(rhs_item->get_loc(), rhs_item);
                    d->set_type(ts.get_void());
                    items.push_back(d);

                    lhs_idx++;
                    continue;
                }

                PANIC("Not implemented");

                lhs_idx++;
            }
        }

        for (auto it : items) {
            json j = *it;
            er.report_debug(it->get_loc(), "de-sugared assign into: {}",
                            j.dump());
        }

        return;
    }

    er.report_warn(node->get_loc(), "node not lowered ({})", node->get_kind());
    visit_children(ctx, node);
}

void lower_ast(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
               types::TypeStore& ts, Options const& opt) {
    auto ctx = Context{.er = &er, .ts = &ts};
    visit_node(ast, root, ctx);
}

}  // namespace yal::lower
