#include "name-res.hpp"

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <ranges>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ast-node-conv.hpp"
#include "ast-node-id.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "error_reporter.hpp"
#include "libassert/assert.hpp"

namespace yal {
using ast::Ast;
using ast::Node;
using ast::NodeId;
namespace conv = ast::conv;

struct DependsVisitor : public ast::Visitor<> {
    using Dag = std::unordered_map<NodeId, std::unordered_set<NodeId>>;

    struct Local {
        std::string_view name;
        size_t           level;
    };

    explicit DependsVisitor(Ast const& ast, ErrorReporter* er)
        : ast::Visitor<>{ast}, er{er} {}

    // ========================================================================

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void visit_before_source_file(Node const& /*node*/,
                                  conv::SourceFile const& data) override {
        for (auto source_file_child : data.children) {
            auto node = ast->get_node(source_file_child.as_ref());
            if (node.get_kind() == ast::NodeKind::TopVarDecl) {
                auto names = conv::top_var_decl_names(*ast, node);
                for (auto id : names.ids) {
                    define_top(ast->get_identifier(id.as_id()),
                               source_file_child);
                }
            }

            else if (node.get_kind() == ast::NodeKind::TopDefDecl) {
                auto names = ast::conv::top_def_decl_names(*ast, node);

                for (auto id : names.ids) {
                    define_top(ast->get_identifier(id.as_id()),
                               source_file_child);
                }
            }

            else if (node.get_kind() == ast::NodeKind::FuncDecl ||
                     node.get_kind() == ast::NodeKind::FuncDeclWithCVarArgs) {
                auto names = conv::func_decl_name(*ast, node);

                // if we have just one id, then this is not namespaced. We take
                // care of namespaced functions on another pass.
                if (names.ids.size() == 1) {
                    define_top(ast->get_identifier(names.ids[0].as_id()),
                               source_file_child);
                }
            }

            else {
                PANIC(
                    "received top-level kind was not implemented in name "
                    "resolution",
                    node.get_kind());
            }
        }
    }

    void visit_after_source_file(Node const& /*node*/,
                                 conv::SourceFile const& data) override {
        auto rev_dag = build_reverse_dag();

        for (auto source_file_child : data.children) {
            auto node = ast->get_node(source_file_child.as_ref());

            if (node.get_kind() == ast::NodeKind::FuncDecl ||
                node.get_kind() == ast::NodeKind::FuncDeclWithCVarArgs) {
                auto names = conv::func_decl_name(*ast, node);

                // namespaced function, this means that everyone that depends on
                // the wrapped type also possibly depends on the namespaced
                // functions.
                if (names.ids.size() != 1) {
                    // find what is the type that is beeing wrapped
                    auto ty =
                        lookup_top(ast->get_identifier(names.ids[0].as_id()));
                    if (!ty.is_valid()) break;

                    // find all things that depend on the wrapped type
                    auto it = rev_dag.find(ty);
                    if (it == end(rev_dag)) break;

                    for (auto const dep_on_ty : it->second) {
                        if (dep_on_ty != source_file_child) {
                            // make it depend on the namespaced function
                            dag[dep_on_ty].insert(source_file_child);
                        }
                    }
                }
            }
        }
    }

    void visit_before_func_decl(Node const& node,
                                conv::FuncDecl const& /*data*/) override {
        current_decl = node.get_id();
    }

    void visit_after_func_decl(Node const& /*node*/,
                               conv::FuncDecl const& /*data*/) override {
        current_decl = NodeId::invalid();
    }

    void visit_before_top_var_decl(Node const& node,
                                   conv::TopVarDecl const& /*data*/) override {
        current_decl = node.get_id();
    }

    void visit_after_top_var_decl(Node const& /*node*/,
                                  conv::TopVarDecl const& /*data*/) override {
        current_decl = NodeId::invalid();
    }

    void visit_before_top_def_decl(Node const& node,
                                   conv::TopDefDecl const& /*data*/) override {
        current_decl = node.get_id();
    }

    void visit_after_top_def_decl(Node const& /*node*/,
                                  conv::TopDefDecl const& /*data*/) override {
        current_decl = NodeId::invalid();
    }

    void visit_before_block(Node const& /*node*/,
                            conv::Block const& /*data*/) override {
        level++;
    }

    void visit_after_block(Node const& /*node*/,
                           conv::Block const& /*data*/) override {
        ASSERT(level > 0);

        clean_level();
        level--;
    }

    void visit_var_decl(Node const& /*node*/,
                        conv::VarDecl const& data) override {
        if (data.types.is_valid()) visit(data.types);
        if (data.inits.is_valid()) visit(data.inits);

        if (level > 0) {
            auto names = conv::id_pack(*ast, ast->get_node(data.ids.as_ref()));
            for (auto id : names.ids) {
                add_local(ast->get_identifier(id.as_id()));
            }
        }
    }

    void visit_id(Node const& /*node*/, std::string_view id) override {
        if (is_local(id)) return;

        auto top = lookup_top(id);
        if (!top.is_valid()) return;

        ASSERT(current_decl.is_valid());
        dag[current_decl].insert(top);
    }

    // ========================================================================

    constexpr void define_top(std::string_view name, NodeId id) {
        tops[name] = id;
        dag[id] = {};
    }

    constexpr auto lookup_top(std::string_view name) const -> NodeId {
        auto it = tops.find(name);
        if (it == tops.end()) return NodeId::invalid();

        return it->second;
    }

    constexpr auto build_reverse_dag() const -> Dag {
        Dag rev_dag;
        for (auto const& [key, deps_on] : dag) {
            for (auto const& dep_on : deps_on) rev_dag[dep_on].insert(key);
        }

        return rev_dag;
    }

    // ========================================================================

    constexpr void clean_level() {
        env.erase(begin(std::ranges::remove_if(
                      env, [&](Local const& l) { return l.level == level; })),
                  env.end());
    }

    constexpr void add_local(std::string_view name) {
        ASSERT(level > 0);
        env.push_back({name, level});
    }

    constexpr auto is_local(std::string_view name) const -> bool {
        auto v = env | std::ranges::views::reverse;
        auto it = std::ranges::find_if(
            v, [&](Local const& l) { return l.name == name; });
        return it != end(v);
    }

    Dag                                          dag;
    std::unordered_map<std::string_view, NodeId> tops;
    std::vector<Local>                           env;
    size_t                                       level = 0;

    NodeId         current_decl = NodeId::invalid();
    ErrorReporter* er;
};

struct TopoSorter {
    void sort() {
        while (!unmarked.empty() && !should_stop) {
            auto n = *(unmarked.end() - 1);
            unmarked.pop_back();
            visit(n);
        }
    }

    void visit(NodeId n) {
        if (perm.contains(n)) return;
        if (temp.contains(n)) {
            // graph has a least one cycle
            er->report_error(ast->get_node_loc(n.as_ref()),
                             "dependency cycle found on declarations");
            should_stop = true;
            return;
        }

        temp.insert(n);

        if (auto it = dag->find(n); it != end(*dag)) {
            for (auto m : it->second) visit(m);
        }

        perm.insert(n);
        sorted.push_back(n);
    }

    DependsVisitor::Dag const* dag;

    std::vector<NodeId>        sorted;
    std::unordered_set<NodeId> temp;
    std::unordered_set<NodeId> perm;
    std::vector<NodeId>        unmarked;

    Ast const*     ast;
    ErrorReporter* er;

    bool should_stop = false;
};

auto topo_sort(Ast const& ast, ErrorReporter& er,
               DependsVisitor::Dag const& dag) -> std::vector<NodeId> {
    std::vector<NodeId> unmarked;
    for (auto const& [n, _] : dag) {
        unmarked.push_back(n);
    }

    auto ts = TopoSorter{
        .dag = &dag,
        .sorted = {},
        .temp = {},
        .perm = {},
        .unmarked = unmarked,
        .ast = &ast,
        .er = &er,
    };

    ts.sort();

    return ts.sorted;
}

auto resolve_names(ast::Ast& ast, ast::NodeId root, ErrorReporter& er)
    -> ast::NodeId {
    auto node = ast.get_node(root.as_ref());
    ASSERT(node.get_kind() == ast::NodeKind::SourceFile);

    auto mod_decl = ast.get_node(node.get_first().as_ref());
    ASSERT(mod_decl.get_kind() == ast::NodeKind::ModuleDecl);

    auto mod_name = ast.get_identifier(mod_decl.get_first().as_id());
    auto mod =
        ast.new_module(node.get_loc(), mod_name, std::array{node.get_id()});

    auto dv = DependsVisitor{ast, &er};
    dv.visit(root);

    auto order = topo_sort(ast, er, dv.dag);
    for (auto const& n : order) {
        auto node = ast.get_node(n.as_ref());
        er.report_warn(node.get_loc(), "should run {}", n);
    }

    return mod;
}

}  // namespace yal
