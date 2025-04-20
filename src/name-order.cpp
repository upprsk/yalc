#include "name-order.hpp"

#include <ranges>
#include <string_view>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/ranges.h"

namespace yal::sort {

using ast::Ast;
using ast::Node;
namespace conv = ast::conv;

using Dag = std::unordered_map<Node*, std::unordered_set<Node*>>;
using TopMap = std::unordered_map<std::string_view, Node*>;

struct TopoSorter {
    void sort() {
        while (!unmarked.empty() && !should_stop) {
            auto n = *(unmarked.end() - 1);
            unmarked.pop_back();
            visit(n);
        }
    }

    void visit(Node* n) {
        if (perm.contains(n)) return;
        if (temp.contains(n)) {
            // graph has a least one cycle
            er->report_error(n->get_loc(),
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

    Dag const* dag;

    std::vector<Node*>        sorted;
    std::unordered_set<Node*> temp;
    std::unordered_set<Node*> perm;
    std::vector<Node*>        unmarked;

    Ast const*     ast;
    ErrorReporter* er;

    bool should_stop = false;
};

auto topo_sort(Ast const& ast, ErrorReporter& er, Dag const& dag)
    -> std::vector<Node*> {
    std::vector<Node*> unmarked;
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

struct State {
    Dag                                dag;
    std::unordered_map<FileId, TopMap> file_tops;
    TopMap                             tops;

    ErrorReporter* er;
};

struct Context {
    State*                               state;
    std::unordered_set<std::string_view> decls;
    FileId                               current_file = FileId::invalid();
    Node*                                current_decl = nullptr;

    size_t   depth = 0;
    Context* parent = nullptr;

    [[nodiscard]] constexpr auto sub() -> Context {
        return {.state = state,
                .decls = {},
                .current_file = current_file,
                .current_decl = current_decl,
                .depth = depth + 1,
                .parent = this};
    }

    [[nodiscard]] constexpr auto sub_with(FileId current_file) -> Context {
        return {.state = state,
                .decls = {},
                .current_file = current_file,
                .current_decl = current_decl,
                .depth = depth + 1,
                .parent = this};
    }

    [[nodiscard]] constexpr auto sub_with(Node* current_decl) -> Context {
        return {.state = state,
                .decls = {},
                .current_file = current_file,
                .current_decl = current_decl,
                .depth = depth + 1,
                .parent = this};
    }

    [[nodiscard]] constexpr auto lookup_local(std::string_view name) const
        -> bool {
        if (auto it = decls.find(name); it != end(decls)) return true;
        return parent ? parent->lookup_local(name) : false;
    }

    constexpr void add_local(std::string_view name) { decls.insert(name); }

    constexpr void insert_dep(Node* node, Node* deps_on) const {
        state->dag[deps_on].insert(node);
    }

    constexpr void define_top_file(std::string_view name, Node* id) const {
        ASSERT(id != nullptr);

        state->file_tops[current_file][name] = id;
        state->dag[id] = {};
    }

    constexpr void define_top(std::string_view name, Node* id) const {
        ASSERT(id != nullptr);

        state->tops[name] = id;
        state->dag[id] = {};
    }

    [[nodiscard]] constexpr auto lookup_top(std::string_view name) const
        -> Node* {
        // lookup at the current file first
        auto& ft = current_file_top();
        if (auto it = ft.find(name); it != end(ft)) return it->second;

        // then at the module level
        auto it = state->tops.find(name);
        if (it != state->tops.end()) return it->second;

        return nullptr;
    }

    [[nodiscard]] constexpr auto current_file_top() const -> TopMap& {
        ASSERT(current_file.is_valid());

        if (auto it = state->file_tops.find(current_file);
            it != end(state->file_tops))
            return it->second;

        return state->tops;
    }
};

constexpr auto build_reverse_dag(Dag const& dag) -> Dag {
    Dag rev_dag;
    for (auto const& [key, deps_on] : dag) {
        for (auto const& dep_on : deps_on) rev_dag[dep_on].insert(key);
    }

    return rev_dag;
}

constexpr auto is_file_private_decl(conv::Decorators decorators) -> bool {
    return conv::decorators_private_kind(decorators) == conv::PrivateKind::File;
}

void hoist_top_var_decl(Context& ctx, Node* node,
                        ast::conv::TopVarDecl const& top) {
    if (is_file_private_decl(top.get_decorators())) {
        auto names = conv::id_pack(*top.get_decl().ids);
        for (auto id : names.ids) {
            ctx.define_top_file(id->get_data_str(), node);
        }

        return;
    }

    auto names = conv::id_pack(*top.get_decl().ids);
    for (auto id : names.ids) {
        ctx.define_top(id->get_data_str(), node);
    }
}

void hoist_top_def_decl(Context& ctx, Node* node,
                        ast::conv::TopDefDecl const& top) {
    if (is_file_private_decl(top.get_decorators())) {
        auto names = conv::id_pack(*top.get_decl().ids);
        for (auto id : names.ids) {
            ctx.define_top_file(id->get_data_str(), node);
        }

        return;
    }

    auto names = conv::id_pack(*top.get_decl().ids);
    for (auto id : names.ids) {
        ctx.define_top(id->get_data_str(), node);
    }
}

void hoist_func_decl(State& state, Context& ctx, Node* node,
                     ast::conv::FuncDecl const& top) {
    auto names = top.get_name();

    if (is_file_private_decl(top.get_decorators())) {
        ctx.define_top_file(names.ids[0]->get_data_str(), node);

        // // if we have just one id, then this is not namespaced. We
        // // take care of namespaced functions on another pass.
        // if (names.ids.size() == 1) {
        //
        // }
        //
        // // the dependencies should come from what we namespaced over,
        // // but if the thing we wrap is not defined, then it does not
        // // end-up in the DAG. So we dummy-define the thing we wrap
        // else {
        //     state.dag[node] = {};
        // }

        return;
    }

    // if we have just one id, then this is not namespaced. We
    // take care of namespaced functions on another pass.
    if (names.ids.size() == 1) {
        ctx.define_top(names.ids[0]->get_data_str(), node);
    }

    // the dependencies should come from what we namespaced over, but if
    // the thing we wrap is not defined, then it does not end-up in the
    // DAG. So we dummy-define the thing we wrap
    else {
        state.dag[node] = {};
    }
}

void hoist_top_decl(State& state, Context& ctx, Node* node) {
    ASSERT(node != nullptr);

    if (node->is_oneof(ast::NodeKind::TopVarDecl)) {
        hoist_top_var_decl(ctx, node, conv::top_var_decl(*node));
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        hoist_top_def_decl(ctx, node, conv::top_def_decl(*node));
        return;
    }

    if (node->is_oneof(ast::NodeKind::FuncDecl,
                       ast::NodeKind::FuncDeclWithCVarArgs)) {
        hoist_func_decl(state, ctx, node, conv::func_decl(*node));
        return;
    }

    UNREACHABLE("invalid node kind", node->get_kind());
}

void fixup_namespaced_function(Context& ctx, Node* func_node,
                               Dag const& rev_dag) {
    auto names = conv::func_decl(*func_node).get_name();

    // namespaced function, this means that everyone that
    // depends on the wrapped type also possibly depends on the
    // namespaced functions.
    if (names.ids.size() != 1) {
        // find what is the type that is beeing wrapped
        auto ty = ctx.lookup_top(conv::id(*names.ids[0]).name);
        if (!ty) return;

        // find all things that depend on the wrapped type
        auto it = rev_dag.find(ty);
        if (it == end(rev_dag)) return;

        fmt::println("ty: {}", *ty);
        for (auto const dep_on_ty : it->second) {
            fmt::println("dep_on_ty: {}", *dep_on_ty);
            if (dep_on_ty != func_node) {
                // make it depend on the namespaced function
                ctx.insert_dep(func_node, dep_on_ty);
            }
        }
        fmt::println("");
    }
}

void fixup_namespaced_functions(Context& ctx, ast::conv::Module const& data,
                                Dag const& rev_dag) {
    for (auto source_file : data.children) {
        auto source_file_data = conv::source_file(*source_file);
        auto sctx = ctx.sub_with(source_file->get_loc().fileid);

        for (auto node : source_file_data.children) {
            if (node->is_oneof(ast::NodeKind::FuncDecl,
                               ast::NodeKind::FuncDeclWithCVarArgs)) {
                fixup_namespaced_function(sctx, node, rev_dag);
            }
        }
    }
}

void visit_node(Ast& ast, Node* node, State& state, Context& ctx) {
    if (node == nullptr) return;

    auto visit = [&](Context& ctx, Node* node) {
        visit_node(ast, node, state, ctx);
    };

    auto visit_children = [&](Context& ctx, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, State& state, Context& ctx) {
                visit_node(ast, node, state, ctx);
            },
            state, ctx);
    };

    if (node->is_oneof(ast::NodeKind::Module)) {
        auto data = conv::module(*node);

        for (auto source_file : data.children) {
            auto source_file_data = conv::source_file(*source_file);
            for (auto source_file_child : source_file_data.children) {
                hoist_top_decl(state, ctx, source_file_child);
            }
        }

        visit_children(ctx, node);

        // auto rev_dag = build_reverse_dag(state.dag);
        // fixup_namespaced_functions(ctx, data, rev_dag);

        return;
    }

    if (node->is_oneof(ast::NodeKind::ImportStmt)) {
        state.er->report_bug(node->get_loc(),
                             "import statements have not been implemented");
        return;
    }

    if (node->is_oneof(ast::NodeKind::SourceFile)) {
        auto sctx = ctx.sub_with(node->get_loc().fileid);
        visit_children(sctx, node);
        return;
    }

    if (node->is_oneof(ast::NodeKind::FuncDecl,
                       ast::NodeKind::FuncDeclWithCVarArgs)) {
        auto data = conv::func_decl(*node);
        auto sctx = ctx.sub_with(node);

        visit(sctx, data.decorators);

        // we want to visit all but the last Id, so that namespaced functions
        // work correctly.
        //
        // NOTE: we should have at least one id in `name`
        auto name = data.get_name();
        auto name_ids = name.ids.subspan(0, name.ids.size() - 1);
        for (auto name : name_ids) {
            visit(sctx, name);
        }

        visit(sctx, data.gargs);
        visit(sctx, data.args);
        visit(sctx, data.ret);
        visit(sctx, data.body);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Block)) {
        auto sctx = ctx.sub();
        visit_children(sctx, node);
        return;
    }

    if (node->is_oneof(ast::NodeKind::NamedRet)) {
        auto data = conv::named_ret(*node);
        visit(ctx, data.type);

        ctx.add_local(data.name);
        return;
    }

    if (node->is_oneof(ast::NodeKind::FuncParam)) {
        auto data = conv::func_param(*node);
        visit(ctx, data.type);
        ctx.add_local(data.name);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopVarDecl)) {
        auto sctx = ctx.sub_with(node);
        visit_children(sctx, node);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        auto sctx = ctx.sub_with(node);
        visit_children(sctx, node);
        return;
    }

    if (node->is_oneof(ast::NodeKind::VarDecl)) {
        auto data = conv::var_decl(*node);
        visit(ctx, data.types);
        visit(ctx, data.inits);

        if (ctx.depth > 2) {
            for (auto name : data.get_ids().ids) {
                ctx.add_local(conv::id(*name).name);
            }
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::DefDecl)) {
        auto data = conv::def_decl(*node);
        if (ctx.depth > 2) {
            for (auto name : data.get_ids().ids) {
                ctx.add_local(conv::id(*name).name);
            }
        }

        visit(ctx, data.types);
        visit(ctx, data.inits);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        if (ctx.lookup_local(data.name)) return;

        auto top = ctx.lookup_top(data.name);
        if (!top) {
            // state.er->report_warn(
            //     node->get_loc(), "name {:?} not found in top-level",
            //     data.name);
            return;
        }

        ASSERT(ctx.current_decl != nullptr);
        if (ctx.current_decl != top) ctx.insert_dep(top, ctx.current_decl);
        return;
    }

    // just visit the children to reach something interesting
    visit_children(ctx, node);
}

auto topo_sort_top_decls(ast::Ast& ast, ast::Node* mod, ErrorReporter& er)
    -> std::vector<ast::Node*> {
    auto state = State{.dag = {}, .file_tops = {}, .tops = {}, .er = &er};
    auto ctx = Context{.state = &state, .decls = {}};
    visit_node(ast, mod, state, ctx);

    // for (auto const& [k, v] : state.dag) {
    //     er.report_warn(k->get_loc(), "this depends on {} things", v.size());
    //
    //     for (auto const& d : v) {
    //         er.report_debug(d->get_loc(), "this", v.size());
    //     }
    // }

    return topo_sort(ast, er, state.dag);
}

}  // namespace yal::sort
