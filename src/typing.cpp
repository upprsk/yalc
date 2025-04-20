#include "typing.hpp"

#include <string_view>
#include <vector>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"
#include "types.hpp"

namespace yal {
using ast::Node;
namespace conv = ast::conv;

struct Typer : public ast::Visitor<ast::Ast> {
    explicit Typer(ast::Ast& ast, types::TypeStore& ts, ErrorReporter& er)
        : ast::Visitor<ast::Ast>{ast}, ts{&ts}, er{&er} {}

    // -----------------------------------------------------------------------

    void visit_var_decl(Node const& node, conv::VarDecl const& data) override {
        visit(data.ids);

        if (data.types.is_valid()) visit(data.types);

        // TODO: setup type inference for inits here in case we have types

        if (data.inits.is_valid()) visit(data.inits);

        auto const& ids = ast->get_node(data.ids.as_ref());

        // handle when we have both type annotation and init
        if (data.types.is_valid() && data.inits.is_valid()) {
            auto id_packs = conv::id_pack(*ast, ids);

            auto const& types =
                ts->get(ast->get_node_type(data.types.as_ref()).as_ref());
            auto const& inits =
                ts->get(ast->get_node_type(data.inits.as_ref()).as_ref());

            ASSERT(types.kind == types::TypeKind::Pack);
            ASSERT(inits.kind == types::TypeKind::Pack);

            if (id_packs.ids.size() != types.inner.size()) {
                er->report_error(
                    node.get_loc(),
                    "mismatch on number of identifiers and types in "
                    "declaration. Got {} identifiers but {} types",
                    id_packs.ids.size(), types.inner.size());
            }

            if (id_packs.ids.size() != inits.inner.size()) {
                er->report_error(
                    node.get_loc(),
                    "mismatch on number of identifiers and initializers in "
                    "declaration. Got {} identifiers but {} initializers",
                    id_packs.ids.size(), inits.inner.size());
            }

            for (auto id : id_packs.ids) {
                auto const& name = ast->get_identifier(id.as_id());
                er->report_debug(ids.get_loc(), "got decl for name: {:?}",
                                 name);
            }
        }

        // handle when we have just the initializer
        else if (!data.types.is_valid() && data.inits.is_valid()) {
            auto id_packs = conv::id_pack(*ast, ids);

            auto const& inits =
                ts->get(ast->get_node_type(data.inits.as_ref()).as_ref());
            ASSERT(inits.kind == types::TypeKind::Pack);

            if (id_packs.ids.size() != inits.inner.size()) {
                er->report_error(
                    node.get_loc(),
                    "mismatch on number of identifiers and initializers in "
                    "declaration. Got {} identifiers but {} initializers",
                    id_packs.ids.size(), inits.inner.size());
            }
        }

        // handle when we have just the types
        else if (data.types.is_valid() && !data.inits.is_valid()) {
        }

        // this is invalid
        else {
            UNREACHABLE("no types and no initializer");
        }
    }

    void visit_expr_pack(Node const&           node,
                         conv::ExprPack const& data) override {
        std::vector<types::TypeId> tys;
        for (auto const& expr : data.items) {
            visit(expr);
            tys.push_back(ast->get_node_type(expr.as_ref()));
        }

        ast->get_node_ref(node.get_id().as_ref())
            .set_type(ts->new_type(types::TypeKind::Pack, tys));
    }

    // -----------------------------------------------------------------------

    void visit_add(Node const& node, conv::Binary const& data) override {
        visit_arith(node, data, "addition");
    }

    void visit_sub(Node const& node, conv::Binary const& data) override {
        visit_arith(node, data, "subtraction");
    }

    void visit_mul(Node const& node, conv::Binary const& data) override {
        visit_arith(node, data, "multiplication");
    }

    void visit_div(Node const& node, conv::Binary const& data) override {
        visit_arith(node, data, "division");
    }

    // -----------------------------------------------------------------------

    void visit_id(Node const& node, conv::Id const& id) override {
        if (!id.to.is_valid()) {
            er->report_warn(node.get_loc(), "identifier {:?} not found",
                            id.name);
            return;
        }

        auto const& decl = ast->get_decl_store()->get_decl(id.to);
        if (!decl.value.type.is_valid()) {
            er->report_warn(node.get_loc(),
                            "declaration of {:?} does not have a type assigned",
                            id.name);

            ast->get_node_ref(node.get_id().as_ref()).set_type(ts->get_error());
            return;
        }

        ast->get_node_ref(node.get_id().as_ref()).set_type(decl.value.type);
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

    // ========================================================================

    void visit_arith(Node const& node, conv::Binary const& data,
                     std::string_view op) {
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
            // yay! both are integers, both can be used. Try and coerce rhs to
            // lhs
            auto ty = ts->coerce(rhs_tyid, lhs_tyid);
            if (!ty.is_valid()) {
                er->report_error(node.get_loc(),
                                 "invalid operators for {}: {} and {} cant be "
                                 "implicitly converted",
                                 op, lhs, rhs);
                er->report_note(ast->get_node_loc(data.lhs.as_ref()),
                                "this has type {}", lhs);
                er->report_note(ast->get_node_loc(data.rhs.as_ref()),
                                "this has type {}", rhs);
            }

            ast->get_node_ref(node.get_id().as_ref()).set_type(ty);
            return;
        }

        // TODO: implement the arith of other types. Missing:
        // - operator overloads
        er->report_error(
            node.get_loc(),
            "invalid operators for {}: {} and {} do not support {}", op, lhs,
            rhs, op);
        er->report_note(ast->get_node_loc(data.lhs.as_ref()),
                        "this has type {}", lhs);
        er->report_note(ast->get_node_loc(data.rhs.as_ref()),
                        "this has type {}", rhs);
        ast->get_node_ref(node.get_id().as_ref()).set_type(ts->get_error());
    }

    // ------------------------------------------------------------------------

    types::TypeStore* ts;
    ErrorReporter*    er;
};

void perform_typing(ast::Ast& ast, ast::NodeId root, ErrorReporter& er,
                    types::TypeStore& ts, Options const& opt) {
    auto typer = Typer{ast, ts, er};
    typer.visit(root);

    if (opt.dump_type_store) {
        json j = ts;
        fmt::println("{}", j.dump());
    }
}

}  // namespace yal
