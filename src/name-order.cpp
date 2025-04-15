#include "name-order.hpp"

#include <ranges>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"

namespace yal::sort {

using ast::Ast;
using ast::Node;
namespace conv = ast::conv;

// FIXME: this is defined in name-res as well.
static constexpr inline auto YAL_EXTENSION = ".yal";

void do_the_thing(FileStore& fs, ErrorReporter& er, Location const& from,
                  std::string_view path) {
    namespace sfs = std::filesystem;

    sfs::path from_filepath = fs.get_filename(from.fileid);
    auto      import_path = sfs::absolute(from_filepath).parent_path() / path;

    er.report_bug(
        from, "importing has not been implemented, showing some debug info");

    er.report_debug(from, "import of {:?} from {:?} (full path: {:?})", path,
                    from_filepath.string(), import_path.string());
    if (sfs::is_directory(import_path)) {
        for (auto const& item : sfs::directory_iterator{import_path}) {
            if (!item.is_regular_file()) continue;

            auto const& item_path = item.path();
            if (!item_path.filename().string().ends_with(YAL_EXTENSION))
                continue;

            er.report_debug(from, "import of {:?} found file {:?}", path,
                            item_path.string());
        }
    }

    else if (sfs::is_regular_file(import_path)) {
        er.report_bug(from,
                      "importing from single files has not been implemented "
                      "(trying to import {:?})",
                      import_path.string());
    }
}

struct DependsVisitor : public ast::Visitor<> {
    using Dag = std::unordered_map<Node*, std::unordered_set<Node*>>;
    using TopMap = std::unordered_map<std::string_view, Node*>;

    struct Local {
        std::string_view name;
        size_t           level;
    };

    explicit DependsVisitor(Ast const& ast, ErrorReporter* er, FileStore* fs)
        : ast::Visitor<>{ast}, er{er}, fs{fs} {}

    // ========================================================================

    void visit_before_module(Node& /*node*/,
                             conv::Module const& data) override {
        for (auto source_file : data.children) {
            auto source_file_data = conv::source_file(*source_file);
            for (auto source_file_child : source_file_data.children) {
                hoist_top_decl(source_file_child);
            }
        }
    }

    void visit_after_module(Node& /*node*/, conv::Module const& data) override {
        auto rev_dag = build_reverse_dag();

        for (auto source_file : data.children) {
            auto source_file_data = conv::source_file(*source_file);
            for (auto node : source_file_data.children) {
                if (node->get_kind() == ast::NodeKind::FuncDecl ||
                    node->get_kind() == ast::NodeKind::FuncDeclWithCVarArgs) {
                    fixup_namespaced_function(node, rev_dag);
                }
            }
        }
    }

    // ------------------------------------------------------------------------

    // FIXME: this function has a stupid ammount of duplication
    void hoist_top_decl(Node* node) {
        ASSERT(node != nullptr);

        if (node->get_kind() == ast::NodeKind::TopVarDecl) {
            auto top = conv::top_var_decl(*node);
            if (is_file_private_decl(top.decorators)) {
                auto names = conv::id_pack(*conv::top_var_decl(*node).decl.ids);
                for (auto id : names.ids) {
                    define_top_file(id->get_data_str(), node);
                }

                return;
            }

            auto names = conv::id_pack(*conv::top_var_decl(*node).decl.ids);
            for (auto id : names.ids) {
                define_top(id->get_data_str(), node);
            }

            return;
        }

        if (node->get_kind() == ast::NodeKind::TopDefDecl) {
            auto top = conv::top_def_decl(*node);
            if (is_file_private_decl(top.decorators)) {
                auto names = conv::id_pack(*top.decl.ids);
                for (auto id : names.ids) {
                    define_top_file(id->get_data_str(), node);
                }

                return;
            }

            auto names = conv::id_pack(*top.decl.ids);
            for (auto id : names.ids) {
                define_top(id->get_data_str(), node);
            }

            return;
        }

        if (node->get_kind() == ast::NodeKind::FuncDecl ||
            node->get_kind() == ast::NodeKind::FuncDeclWithCVarArgs) {
            auto top = conv::func_decl(*node);
            if (is_file_private_decl(top.decorators)) {
                auto names = top.name;

                // if we have just one id, then this is not namespaced. We
                // take care of namespaced functions on another pass.
                if (names.ids.size() == 1) {
                    define_top_file(names.ids[0]->get_data_str(), node);
                }

                // the dependencies should come from what we namespaced over,
                // but if the thing we wrap is not defined, then it does not
                // end-up in the DAG. So we dummy-define the thing we wrap
                else {
                    dag[node] = {};
                }

                return;
            }

            auto names = top.name;

            // if we have just one id, then this is not namespaced. We
            // take care of namespaced functions on another pass.
            if (names.ids.size() == 1) {
                define_top(names.ids[0]->get_data_str(), node);
            }

            // the dependencies should come from what we namespaced over, but if
            // the thing we wrap is not defined, then it does not end-up in the
            // DAG. So we dummy-define the thing we wrap
            else {
                dag[node] = {};
            }

            return;
        }

        if (node->get_kind() == ast::NodeKind::ImportStmt) {
            // in order to know what is the name that the import
            // statement declares, we need to do the full thing for the
            // imported module.
            auto import_stmt = conv::import_stmt(*node);
            do_the_thing(*fs, *er, node->get_loc(), import_stmt.path);
        }

        PANIC("top-level kind not implemented in hoisting of name resolution",
              node->get_kind());
    }

    auto is_file_private_decl(conv::Decorators decorators) const -> bool {
        return conv::decorators_private_kind(decorators) ==
               conv::PrivateKind::File;
    }

    void fixup_namespaced_function(Node* func_node, Dag const& rev_dag) {
        auto names = conv::func_decl(*func_node).name;

        // namespaced function, this means that everyone that
        // depends on the wrapped type also possibly depends on the
        // namespaced functions.
        if (names.ids.size() != 1) {
            // find what is the type that is beeing wrapped
            auto ty = lookup_top(conv::id(*names.ids[0]).name);
            if (!ty) return;

            // find all things that depend on the wrapped type
            auto it = rev_dag.find(ty);
            if (it == end(rev_dag)) return;

            for (auto const dep_on_ty : it->second) {
                if (dep_on_ty != func_node) {
                    // make it depend on the namespaced function
                    dag[dep_on_ty].insert(func_node);
                }
            }
        }
    }

    // ------------------------------------------------------------------------

    void visit_before_source_file(Node& node,
                                  conv::SourceFile const& /*data*/) override {
        current_file = node.get_loc().fileid;
    }

    void visit_after_source_file(Node& /*node*/,
                                 conv::SourceFile const& /*data*/) override {
        current_file = FileId::invalid();
    }

    // ------------------------------------------------------------------------

    void visit_before_func_decl(Node& node,
                                conv::FuncDecl const& /*data*/) override {
        current_decl = &node;
    }

    void visit_after_func_decl(Node& /*node*/,
                               conv::FuncDecl const& /*data*/) override {
        current_decl = nullptr;
    }

    void visit_before_top_var_decl(Node& node,
                                   conv::TopVarDecl const& /*data*/) override {
        current_decl = &node;
    }

    void visit_after_top_var_decl(Node& /*node*/,
                                  conv::TopVarDecl const& /*data*/) override {
        current_decl = nullptr;
    }

    void visit_before_top_def_decl(Node& node,
                                   conv::TopDefDecl const& /*data*/) override {
        current_decl = &node;
    }

    void visit_after_top_def_decl(Node& /*node*/,
                                  conv::TopDefDecl const& /*data*/) override {
        current_decl = nullptr;
    }

    // NOTE: this is basically a copy of the default implementation in Visitor<>
    void visit_func_decl(Node& node, conv::FuncDecl const& data) override {
        visit_before_func_decl(node, data);

        visit_decorators(*data.detail.decorators, data.decorators);

        if (data.name.ids.size() != 1) {
            visit(data.name.ids[0]);
        }

        // open a scope for the arguments and returns
        level++;

        visit_func_params(*data.detail.gargs, data.gargs);
        visit_func_params(*data.detail.args, data.args);
        visit_func_ret_pack(*data.detail.ret, data.ret);
        visit(data.body);

        visit_after_func_decl(node, data);

        // close the scope (could put this in visit_after_func_decl, but putting
        // it in another function would mean that I would forget it).
        level--;
    }

    void visit_before_block(Node& /*node*/,
                            conv::Block const& /*data*/) override {
        level++;
    }

    void visit_after_block(Node& /*node*/,
                           conv::Block const& /*data*/) override {
        ASSERT(level > 0);

        clean_level();
        level--;
    }

    void visit_named_ret(Node& /*node*/, conv::NamedRet const& data) override {
        add_local(data.name);
    }

    void visit_def_decl(Node& /*node*/, conv::DefDecl const& data) override {
        visit(data.types);
        visit(data.inits);

        if (level > 0) {
            auto names = conv::id_pack(*data.ids);
            for (auto id : names.ids) {
                add_local(id->get_data_str());
            }
        }
    }

    void visit_var_decl(Node& /*node*/, conv::VarDecl const& data) override {
        visit(data.types);
        visit(data.inits);

        if (level > 0) {
            auto names = conv::id_pack(*data.ids);
            for (auto id : names.ids) {
                add_local(id->get_data_str());
            }
        }
    }

    void visit_after_func_param(Node& /*node*/,
                                conv::FuncParam const& data) override {
        add_local(data.name);
    }

    void visit_id(Node& /*node*/, conv::Id const& id) override {
        if (is_local(id.name)) return;

        auto top = lookup_top(id.name);
        if (!top) return;

        ASSERT(current_decl != nullptr);
        dag[current_decl].insert(top);
    }

    // ========================================================================

    constexpr void define_top(std::string_view name, Node* id) {
        tops[name] = id;
        dag[id] = {};
    }

    constexpr void define_top_file(std::string_view name, Node* id) {
        file_tops[current_file][name] = id;
        dag[id] = {};
    }

    constexpr auto lookup_top(std::string_view name) const -> Node* {
        // lookup at the current file first
        auto& ft = current_file_top();
        if (auto it = ft.find(name); it != end(ft)) return it->second;

        // then at the module level
        auto it = tops.find(name);
        if (it == tops.end()) return nullptr;

        return it->second;
    }

    constexpr auto current_file_top() -> TopMap& {
        ASSERT(current_file.is_valid());
        if (auto it = file_tops.find(current_file); it != end(file_tops))
            return it->second;

        return tops;
    }

    constexpr auto current_file_top() const -> TopMap const& {
        if (auto it = file_tops.find(current_file); it != end(file_tops))
            return it->second;

        return tops;
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

    Dag                                dag;
    TopMap                             tops;
    std::unordered_map<FileId, TopMap> file_tops;
    std::vector<Local>                 env;
    size_t                             level = 0;
    FileId                             current_file;

    Node*          current_decl{};
    ErrorReporter* er;
    FileStore*     fs;
};

// ============================================================================

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

    DependsVisitor::Dag const* dag;

    std::vector<Node*>        sorted;
    std::unordered_set<Node*> temp;
    std::unordered_set<Node*> perm;
    std::vector<Node*>        unmarked;

    Ast const*     ast;
    ErrorReporter* er;

    bool should_stop = false;
};

auto topo_sort(Ast const& ast, ErrorReporter& er,
               DependsVisitor::Dag const& dag) -> std::vector<Node*> {
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

auto topo_sort_top_decls(ast::Ast& ast, ast::Node* mod, ErrorReporter& er,
                         FileStore& fs) -> std::vector<ast::Node*> {
    auto dv = DependsVisitor{ast, &er, &fs};
    dv.visit(mod);

    // for (auto const& [k, v] : dv.dag) {
    //     er.report_warn(k->get_loc(), "this depends on {} things", v.size());
    //
    //     for (auto const& d : v) {
    //         er.report_debug(d->get_loc(), "this", v.size());
    //     }
    // }

    return topo_sort(ast, er, dv.dag);
}

}  // namespace yal::sort
