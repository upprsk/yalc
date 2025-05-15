#include "lower.hpp"

#include <algorithm>
#include <iterator>
#include <ranges>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "types.hpp"

namespace yal::lower {

using ast::Ast;
using ast::Node;
using ast::NodeKind;
namespace conv = ast::conv;

namespace rv = std::ranges::views;

struct Context {
    ErrorReporter*    er{};
    types::TypeStore* ts{};
    Options const*    opt{};
};

// ============================================================================

void visit_node(Ast& ast, Node* node, Context& ctx);

void visit_var_decl(Ast& ast, Node* node, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };

    auto& ts = *ctx.ts;

    auto data = conv::var_decl(*node);

    std::vector<Node*> items;

    if (data.inits == nullptr) {
        // got no initializers
        return;
    }

    // got initializers
    auto ids = data.get_ids().ids;
    auto inits = data.get_inits().items;

    for (size_t id_idx{}; auto [init_idx, init] : rv::enumerate(inits)) {
        if (id_idx >= ids.size()) break;

        auto init_type = init->get_type();

        // result of calling some function
        if (init_type->is_pack()) {
            std::vector<Node*> id_items;

            for (auto _ : init_type->inner) {
                if (id_idx >= ids.size()) break;

                if (conv::id(*ids[id_idx]).is_discard()) {
                    id_items.push_back(
                        ast.new_discarded(ids[id_idx]->get_loc()));
                    id_idx++;
                    continue;
                }

                id_items.push_back(ids[id_idx]);
                id_idx++;
            }

            visit(ctx, init);

            auto d = ast.new_decl_local_var_direct_pack(node->get_loc(),
                                                        id_items, init);
            d->set_type(ts.get_void());
            items.push_back(d);
        }

        // a simple initializer expression
        else {
            auto id = ids[id_idx];
            if (conv::id(*id).is_discard()) {
                auto d = ast.new_discard(id->get_loc(), init);
                d->set_type(ts.get_void());
                items.push_back(d);
                id_idx++;
                init_idx++;
                continue;
            }

            visit(ctx, init);

            auto d = ast.new_decl_local_var_direct(node->get_loc(), init);
            d->set_type(ts.get_void());
            d->set_decl(id->get_decl());
            items.push_back(d);
            id_idx++;
        }

        init_idx++;
    }

    node->transmute_to_unscoped_group(ast.allocate_node_span(items));
}

void visit_assign(Ast& ast, Node* node, Context& ctx) {
    auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx); };

    auto& ts = *ctx.ts;

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
                    auto d = ast.new_discarded(lhs_item->get_loc());
                    d->set_type(ts.get_void());
                    lhs_items.push_back(d);
                    lhs_idx++;
                    continue;
                }

                lhs_items.push_back(lhs_item);
                lhs_idx++;
            }

            // in case all of the items are Discarded, just make the whole
            // thing a Discard
            auto are_all_discarded = std::ranges::all_of(
                lhs_items,
                [](Node* n) { return n->is_oneof(ast::NodeKind::Discarded); });
            if (are_all_discarded) {
                auto d = ast.new_discard(node->get_loc(), rhs_item);
                d->set_type(ts.get_void());
                items.push_back(d);

                continue;
            }

            auto loc = lhs_items.at(0)->get_loc().extend(
                lhs_items.at(lhs_items.size() - 1)->get_loc());
            auto new_lhs_pack = ast.new_expr_pack(loc, lhs_items);
            new_lhs_pack->set_type(ts.new_pack(
                lhs_items |
                rv::transform([](Node* n) { return n->get_type(); }) |
                std::ranges::to<std::vector>()));

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
                                         ast::NodeKind::AssignDirect, lhs_item,
                                         rhs_item);
            d->set_type(ts.get_void());
            items.push_back(d);

            lhs_idx++;
        }
    }

    node->transmute_to_unscoped_group(ast.allocate_node_span(items));
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_call(Ast& ast, Node* node, Context& ctx) {
    // auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx);
    // };

    auto visit_children = [&](Context& ctx, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, Context& ctx) {
                visit_node(ast, node, ctx);
            },
            ctx);
    };

    // auto& ts = *ctx.ts;
    auto& er = *ctx.er;

    auto data = conv::call(*node);
    auto func_type = data.callee->get_type()->as_func();

    // direct call
    if (data.callee->is_oneof(ast::NodeKind::Id)) {
        auto decl = data.callee->get_decl();
        ASSERT(decl != nullptr);

        node->transmute_to_call_direct(decl, data.args);
        visit_children(ctx, node);
    }

    // method call
    else if (data.callee->is_oneof(ast::NodeKind::Field) &&
             func_type.is_bound) {
        auto decl = data.callee->get_decl();
        ASSERT(decl != nullptr);

        auto params = func_type.get_params_direct();
        ASSERT(params.size() > 0);

        auto first_type = params[0];
        auto receiver = conv::field(*data.callee).receiver;
        auto receiver_type = receiver->get_type();

        if (*first_type == *receiver_type) {
            // both are the same, just transmute to a direct call
            std::vector args{receiver};
            std::ranges::copy(data.args, std::back_inserter(args));
            node->transmute_to_call_direct(decl, ast.allocate_node_span(args));
            visit_children(ctx, node);
        }

        else {
            // the receiver is a pointer, but the expected value is not a
            // pointer
            if (receiver_type->is_ptr() && !first_type->is_ptr()) {
                PANIC("not implemented: pointer receiver but value param",
                      *receiver_type, *first_type);
            }

            // the receiver is a value, but the expected value is a pointer.
            // Needs to take the address of the receiver
            else if (!receiver_type->is_ptr() && first_type->is_ptr()) {
                // TODO: need to check if the resulting pointer is const by
                // checking for lvalues

                // convert `a.print()` to
                // print(&a)

                // FIXME: just setting it to the receiver type may be a problem
                // FIXME: need to set the receiving decl to be a stack-var
                auto new_receiver = ast.new_unary_expr(
                    receiver->get_loc(), ast::NodeKind::AddrOf, receiver);
                new_receiver->set_type(receiver_type);

                std::vector args{new_receiver};
                std::ranges::copy(data.args, std::back_inserter(args));
                node->transmute_to_call_direct(decl,
                                               ast.allocate_node_span(args));
                visit_children(ctx, node);
            }

            else {
                PANIC(
                    "automatic conversion on method calls have not been "
                    "implemented");
            }
        }
    }

    // indirect call
    else {
        er.report_error(node->get_loc(), "indirect calls not implemented");
        PANIC("not implemented");
    }
}

void visit_node(Ast& ast, Node* node, Context& ctx) {
    if (node == nullptr) return;

    // auto visit = [&](Context& ctx, Node* node) { visit_node(ast, node, ctx);
    // };

    auto visit_children = [&](Context& ctx, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, Context& ctx) {
                visit_node(ast, node, ctx);
            },
            ctx);
    };

    auto& er = *ctx.er;
    // auto& ts = *ctx.ts;

    if (node->is_oneof(ast::NodeKind::VarDecl)) {
        visit_var_decl(ast, node, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Assign)) {
        visit_assign(ast, node, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Call)) {
        visit_call(ast, node, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Block)) {
        visit_children(ctx, node);

        // cleanup unscoped grousp?
        return;
    }

    if (ctx.opt->verbose_lowering)
        er.report_warn(node->get_loc(), "node not lowered ({})",
                       node->get_kind());
    visit_children(ctx, node);
}

void lower_ast(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
               types::TypeStore& ts, Options const& opt) {
    auto ctx = Context{.er = &er, .ts = &ts, .opt = &opt};
    visit_node(ast, root, ctx);
}

}  // namespace yal::lower
