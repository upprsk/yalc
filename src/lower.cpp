#include "lower.hpp"

#include <algorithm>
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
                std::vector<Node*> lhs_items;
                for (auto _ : rhs_type->inner) {
                    if (lhs_idx >= lhs.size()) break;
                    auto lhs_item = lhs[lhs_idx];

                    if (conv::is_discard_id(*lhs_item)) {
                        lhs_items.push_back(
                            ast.new_discarded(lhs_item->get_loc()));
                        lhs_idx++;
                        continue;
                    }

                    lhs_items.push_back(lhs_item);
                    lhs_idx++;
                }

                // in case all of the items are Discarded, just make the whole
                // thing a Discard
                auto are_all_discarded =
                    std::ranges::all_of(lhs_items, [](Node* n) {
                        return n->is_oneof(ast::NodeKind::Discarded);
                    });
                if (are_all_discarded) {
                    auto d = ast.new_discard(node->get_loc(), rhs_item);
                    d->set_type(ts.get_void());
                    items.push_back(d);

                    continue;
                }

                auto loc = lhs_items.at(0)->get_loc().extend(
                    lhs_items.at(lhs_items.size() - 1)->get_loc());
                // FIXME: add types to the expr pack
                auto new_lhs_pack = ast.new_expr_pack(loc, lhs_items);

                auto d = ast.new_assign_stmt(node->get_loc(),
                                             ast::NodeKind::AssignDirectPack,
                                             new_lhs_pack, rhs_item);
                d->set_type(ts.get_void());
                items.push_back(d);
            }

            // a simple initializer expression
            else {
                auto lhs_item = lhs[lhs_idx];
                if (conv::is_discard_id(*lhs_item)) {
                    // discarding the value, add with discard

                    visit(ctx, rhs_item);

                    auto d = ast.new_discard(rhs_item->get_loc(), rhs_item);
                    d->set_type(ts.get_void());
                    items.push_back(d);

                    lhs_idx++;
                    continue;
                }

                // convert to a direct assignment
                auto d = ast.new_assign_stmt(node->get_loc(),
                                             ast::NodeKind::AssignDirect,
                                             lhs_item, rhs_item);
                d->set_type(ts.get_void());
                items.push_back(d);

                lhs_idx++;
            }
        }

        node->transmute_to_unscoped_group(ast.allocate_node_span(items));
        return;
    }

    if (node->is_oneof(ast::NodeKind::Block)) {
        visit_children(ctx, node);

        // cleanup unscoped grousp?
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
