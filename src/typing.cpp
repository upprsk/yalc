#include "typing.hpp"

#include <vector>

#include "ast-node-visitor.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "type-id.hpp"
#include "types.hpp"

namespace yal {
using ast::Node;
namespace conv = ast::conv;

struct Typer : public ast::Visitor<ast::Ast> {
    explicit Typer(ast::Ast& ast, types::TypeStore& ts, ErrorReporter& er)
        : ast::Visitor<ast::Ast>{ast}, ts{&ts}, er{&er} {}

    void visit_add(Node const& node, conv::Binary const& data) override {
        // TODO: propagate inference?
        // Here we should pass to `rhs` what `lhs` got so that inference works
        // as expected over there
        visit(data.lhs);
        visit(data.rhs);

        auto lhs_tyid = ast->get_node_type(data.lhs.as_ref());
        auto rhs_tyid = ast->get_node_type(data.rhs.as_ref());

        // no type for left, just forward right
        if (rhs_tyid.is_valid() && !lhs_tyid.is_valid()) {
            ast->get_node_ref(node.get_id().as_ref()).set_type(rhs_tyid);
            return;
        }

        // no type for right, just forward left
        if (!rhs_tyid.is_valid() && lhs_tyid.is_valid()) {
            ast->get_node_ref(node.get_id().as_ref()).set_type(lhs_tyid);
            return;
        }

        // no types at all, nothing we can do
        if (!rhs_tyid.is_valid() && !lhs_tyid.is_valid()) {
            ast->get_node_ref(node.get_id().as_ref()).set_type(ts->get_error());
            return;
        }

        auto lhs = ts->get(lhs_tyid.as_ref());
        auto rhs = ts->get(rhs_tyid.as_ref());

        if ((lhs.is_integral() && rhs.is_integral()) ||
            (lhs.is_float() && rhs.is_float())) {
            // yay! both are integers, both can be added. Try and coerce rhs to
            // lhs
            auto ty = ts->coerce(rhs_tyid, lhs_tyid);
            if (!ty.is_valid()) {
                er->report_error(node.get_loc(),
                                 "invalid operators for add: {} and {} cant be "
                                 "implicitly converted",
                                 lhs, rhs);
                er->report_note(ast->get_node_loc(data.lhs.as_ref()),
                                "this has type {}", lhs);
                er->report_note(ast->get_node_loc(data.rhs.as_ref()),
                                "this has type {}", rhs);
            }

            ast->get_node_ref(node.get_id().as_ref()).set_type(ty);
            return;
        }

        // TODO: implement the addition of other types. Missing:
        // - operator overloads
        er->report_error(
            node.get_loc(),
            "invalid operators for add: {} and {} do not support addition", lhs,
            rhs);
        er->report_note(ast->get_node_loc(data.lhs.as_ref()),
                        "this has type {}", lhs);
        er->report_note(ast->get_node_loc(data.rhs.as_ref()),
                        "this has type {}", rhs);
        ast->get_node_ref(node.get_id().as_ref()).set_type(ts->get_error());
    }

    void visit_int(Node const& node, uint64_t /*value*/) override {
        // TODO: use inference to get the currently expected type
        auto ty = ts->get_i32();
        ast->get_node_ref(node.get_id().as_ref()).set_type(ty);
    }

    // FIXME: remove this from AST
    void visit_float(Node const& node, float /*value*/) override {
        auto ty = ts->get_f32();
        ast->get_node_ref(node.get_id().as_ref()).set_type(ty);
    }

    void visit_double(Node const& node, double /*value*/) override {
        // TODO: use inference to get the currently expected type
        auto ty = ts->get_f64();
        ast->get_node_ref(node.get_id().as_ref()).set_type(ty);
    }

    void visit_str(Node const& node, std::string_view /*s*/) override {
        auto ty = ts->get_strview();
        ast->get_node_ref(node.get_id().as_ref()).set_type(ty);
    }

    // ------------------------------------------------------------------------

    types::TypeStore* ts;
    ErrorReporter*    er;
};

void perform_typing(ast::Ast& ast, ast::NodeId root, ErrorReporter& er,
                    Options const& opt) {
    auto ts = types::TypeStore{};
    ts.add_builtins();

    auto typer = Typer{ast, ts, er};
    typer.visit(root);

    if (opt.dump_type_store) {
        json j = ts;
        fmt::println("{}", j.dump());
    }
}

}  // namespace yal
