#include "name-res.hpp"

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <iterator>
#include <numeric>
#include <ranges>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

#include "ast-node-conv.hpp"
#include "ast-node-id.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "decl-store.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/color.h"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "libassert/assert.hpp"
#include "yal.hpp"

namespace yal {
using ast::Ast;
using ast::Node;
using ast::NodeId;
namespace conv = ast::conv;

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
    using Dag = std::unordered_map<NodeId, std::unordered_set<NodeId>>;

    struct Local {
        std::string_view name;
        size_t           level;
    };

    explicit DependsVisitor(Ast const& ast, ErrorReporter* er, FileStore* fs)
        : ast::Visitor<>{ast}, er{er}, fs{fs} {}

    // ========================================================================

    void visit_before_module(Node const& /*node*/,
                             conv::Module const& data) override {
        for (auto source_file : data.children) {
            auto node = ast->get_node(source_file.as_ref());
            ASSERT(node.get_kind() == ast::NodeKind::SourceFile);

            auto source_file_data = conv::source_file(*ast, node);
            for (auto source_file_child : source_file_data.children) {
                hoist_top_decl(source_file_child);
            }
        }
    }

    void visit_after_module(Node const& /*node*/,
                            conv::Module const& data) override {
        auto rev_dag = build_reverse_dag();

        for (auto source_file : data.children) {
            auto node = ast->get_node(source_file.as_ref());
            ASSERT(node.get_kind() == ast::NodeKind::SourceFile);

            auto source_file_data = conv::source_file(*ast, node);
            for (auto source_file_child : source_file_data.children) {
                auto node = ast->get_node(source_file_child.as_ref());

                if (node.get_kind() == ast::NodeKind::FuncDecl ||
                    node.get_kind() == ast::NodeKind::FuncDeclWithCVarArgs) {
                    fixup_namespaced_function(source_file_child, node, rev_dag);
                }
            }
        }
    }

    // ------------------------------------------------------------------------

    void hoist_top_decl(NodeId top_decl) {
        auto node = ast->get_node(top_decl.as_ref());
        if (node.get_kind() == ast::NodeKind::TopVarDecl) {
            auto names = conv::top_var_decl_names(*ast, node);
            for (auto id : names.ids) {
                define_top(ast->get_identifier(id.as_id()), top_decl);
            }
        }

        else if (node.get_kind() == ast::NodeKind::TopDefDecl) {
            auto names = ast::conv::top_def_decl_names(*ast, node);

            for (auto id : names.ids) {
                define_top(ast->get_identifier(id.as_id()), top_decl);
            }
        }

        else if (node.get_kind() == ast::NodeKind::FuncDecl ||
                 node.get_kind() == ast::NodeKind::FuncDeclWithCVarArgs) {
            auto names = conv::func_decl_name(*ast, node);

            // if we have just one id, then this is not namespaced. We
            // take care of namespaced functions on another pass.
            if (names.ids.size() == 1) {
                define_top(ast->get_identifier(names.ids[0].as_id()), top_decl);
            }
        }

        else if (node.get_kind() == ast::NodeKind::ImportStmt) {
            // in order to know what is the name that the import
            // statement declares, we need to do the full thing for the
            // imported module.
            auto import_stmt = conv::import_stmt(*ast, node);
            do_the_thing(*fs, *er, node.get_loc(), import_stmt.path);
        }

        else {
            PANIC(
                "top-level kind not implemented in hoisting of name resolution",
                node.get_kind());
        }
    }

    void fixup_namespaced_function(NodeId top_decl, Node func_node,
                                   Dag const& rev_dag) {
        auto names = conv::func_decl_name(*ast, func_node);

        // namespaced function, this means that everyone that
        // depends on the wrapped type also possibly depends on the
        // namespaced functions.
        if (names.ids.size() != 1) {
            // find what is the type that is beeing wrapped
            auto ty = lookup_top(ast->get_identifier(names.ids[0].as_id()));
            if (!ty.is_valid()) return;

            // find all things that depend on the wrapped type
            auto it = rev_dag.find(ty);
            if (it == end(rev_dag)) return;

            for (auto const dep_on_ty : it->second) {
                if (dep_on_ty != top_decl) {
                    // make it depend on the namespaced function
                    dag[dep_on_ty].insert(top_decl);
                }
            }
        }
    }

    // ------------------------------------------------------------------------

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
    FileStore*     fs;
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

auto find_all_source_files(std::filesystem::path const& root_filepath)
    -> std::vector<std::filesystem::path> {
    namespace fs = std::filesystem;
    namespace rv = std::ranges::views;

    auto is_relative = root_filepath.is_relative();

    return fs::directory_iterator{fs::absolute(root_filepath).parent_path()} |
           rv::filter([](auto it) { return it.is_regular_file(); }) |
           rv::transform([](auto it) { return it.path(); }) |
           rv::transform(
               [&](auto it) { return is_relative ? fs::relative(it) : it; }) |
           rv::filter([](auto it) {
               return it.filename().string().ends_with(YAL_EXTENSION);
           }) |
           std::ranges::to<std::vector>();
}

auto parse_and_load_module_into_ast(std::filesystem::path const& filepath,
                                    Node const& mod_decl, ast::Ast& ast,
                                    FileStore& fs, ErrorReporter& er,
                                    Options const& opt) -> NodeId {
    auto src_file = load_and_parse_into_ast(fs, er, filepath, ast, opt);
    if (!src_file.is_valid()) {
        er.report_bug(mod_decl.get_loc(),
                      "when scanning for module files, failed to get {}",
                      filepath.string());
        return NodeId::invalid();
    }

    auto n = ast.get_node(src_file.as_ref());
    auto mod_found = ast.get_node(conv::source_file(ast, n).mod.as_ref());
    auto mod_found_name = conv::module_decl(ast, mod_found).name;

    auto mod_name = conv::module_decl(ast, mod_decl).name;
    if (mod_found_name != mod_name) {
        er.report_bug(mod_found.get_loc(),
                      "found mixed module name declarations in directory. "
                      "Want {:?} but found {:?}",
                      mod_name, mod_found_name);
        return NodeId::invalid();
    }

    return src_file;
}

auto parse_all_files_into_module(ast::Ast& ast, ast::Node const& start_node,
                                 Node const& mod_decl, FileStore& fs,
                                 ErrorReporter& er, Options const& opt) {
    auto start_filename = fs.get_filename(start_node.get_loc().fileid);
    auto all_files = find_all_source_files(start_filename);

    std::vector<NodeId> files{start_node.get_id()};
    for (auto const& filepath : all_files) {
        // don't re-parse ourselves
        if (std::filesystem::equivalent(filepath, start_filename)) continue;

        auto src_file = parse_and_load_module_into_ast(filepath, mod_decl, ast,
                                                       fs, er, opt);
        if (src_file.is_valid()) files.push_back(src_file);
    }

    auto mod_name = conv::module_decl(ast, mod_decl).name;
    return ast.new_module(start_node.get_loc(), mod_name, files);
}

// ============================================================================

struct NameSolver : public ast::Visitor<Ast> {
    explicit NameSolver(FileStore& fs, ErrorReporter& er, Ast& ast)
        : ast::Visitor<Ast>{ast}, er{&er}, fs{&fs} {
        define_buitins();
    }

    void define_buitins() {
        // push the root scope
        push_env();

        // FIXME: how do we reference back to this?
        define_at_top("usize", NodeId::invalid());
        define_at_top("isize", NodeId::invalid());

        define_at_top("i64", NodeId::invalid());
        define_at_top("u64", NodeId::invalid());
        define_at_top("i32", NodeId::invalid());
        define_at_top("u32", NodeId::invalid());
        define_at_top("i16", NodeId::invalid());
        define_at_top("u16", NodeId::invalid());
        define_at_top("i8", NodeId::invalid());
        define_at_top("u8", NodeId::invalid());

        define_at_top("f32", NodeId::invalid());
        define_at_top("f64", NodeId::invalid());

        define_at_top("bool", NodeId::invalid());

        define_at_top("true", NodeId::invalid());
        define_at_top("false", NodeId::invalid());

        define_at_top("nil", NodeId::invalid());
    }

    struct Env {
        constexpr void set(std::string const& name, DeclId id) {
            items[name] = id;
        }

        constexpr auto get(std::string const& name) const -> DeclId {
            if (auto it = items.find(name); it != end(items)) return it->second;
            return DeclId::invalid();
        }

        std::string                             scope_name;
        std::unordered_map<std::string, DeclId> items;
    };

    // ========================================================================

    void open_file(NodeId source_file) { push_env_of(source_file); }

    void close_file(NodeId source_file) { pop_env_into(source_file); }

    // ========================================================================

    void visit_before_module(Node const& /*node*/,
                             conv::Module const& data) override {
        push_env(std::string{data.name});
    }

    void visit_after_module(Node const& /*node*/,
                            conv::Module const& /*data*/) override {
        pop_env();
    }

    // ------------------------------------------------------------------------

    void visit_before_block(Node const& /*node*/,
                            conv::Block const& /*data*/) override {
        push_env();
    }

    void visit_after_block(Node const& /*node*/,
                           conv::Block const& /*data*/) override {
        pop_env();
    }

    // ------------------------------------------------------------------------

    void visit_before_func_decl(Node const&           node,
                                conv::FuncDecl const& data) override {
        auto names = conv::id_pack(*ast, ast->get_node(data.name.as_ref()));
        if (names.ids.size() == 1) {
            auto name = ast->get_identifier(names.ids[0].as_id());
            define_at_mod(std::string{name}, node.get_id());
        }

        for (auto const& id : names.ids) {
            push_env(std::string{ast->get_identifier(id.as_id())});
        }
    }

    void visit_after_func_decl(Node const& /*node*/,
                               conv::FuncDecl const& data) override {
        auto names = conv::id_pack(*ast, ast->get_node(data.name.as_ref()));
        pop_env(names.ids.size());
    }

    void visit_after_var_decl(Node const&          node,
                              conv::VarDecl const& data) override {
        auto names = conv::id_pack(*ast, ast->get_node(data.ids.as_ref()));
        for (auto nid : names.ids) {
            auto const& name = ast->get_identifier(nid.as_id());
            define_at_top(name, node.get_id());
        }
    }

    void visit_after_def_decl(Node const&          node,
                              conv::DefDecl const& data) override {
        auto names = conv::id_pack(*ast, ast->get_node(data.ids.as_ref()));
        for (auto nid : names.ids) {
            auto const& name = ast->get_identifier(nid.as_id());
            define_at_top(name, node.get_id());
        }
    }

    void visit_after_func_param(Node const&            node,
                                conv::FuncParam const& data) override {
        define_at_top(std::string{data.name}, node.get_id());
    }

    // ------------------------------------------------------------------------

    void visit_id(Node const& node, std::string_view id) override {
        auto decl_id = lookup_name(std::string{id});
        if (!decl_id.is_valid()) {
            er->report_error(node.get_loc(), "undefined identifier {:?}", id);
            return;
        }

        auto const& decl = ds.get_decl(decl_id);

        if (decl.node.is_valid()) {
            er->report_note(
                node.get_loc(),
                "found decl for id {:?}: {}: local_name={}, name={}, node={}",
                id, decl_id, decl.local_name, decl.name, decl.node);
            er->report_debug(ast->get_node_loc(decl.node.as_ref()),
                             "defined here");
        }
    }

    // ========================================================================

    [[nodiscard]] constexpr auto lookup_name(std::string const& name) const
        -> DeclId {
        for (auto const& env : envs | std::ranges::views::reverse) {
            if (auto id = env.get(name); id.is_valid()) return id;
        }

        return DeclId::invalid();
    }

    auto gen_name(std::string const& local_name) -> std::string {
        namespace rv = std::ranges::views;

        auto s = fmt::to_string(
            fmt::join(envs | rv::filter([](Env const& env) {
                          return !env.scope_name.empty();
                      }) | rv::transform([](Env const& env) {
                          return static_cast<std::string_view>(env.scope_name);
                      }),
                      "."));
        if (s.empty()) return local_name;
        return fmt::format("{}.{}", s, local_name);
    }

    constexpr auto define_at_file(std::string const& name, NodeId node)
        -> DeclId {
        return define_at(*get_file_env(), name, node);
    }

    constexpr auto define_at_mod(std::string const& name, NodeId node)
        -> DeclId {
        return define_at(*get_mod_env(), name, node);
    }

    constexpr auto define_at_top(std::string const& name, NodeId node)
        -> DeclId {
        return define_at(*get_top_env(), name, node);
    }

    constexpr auto define_at(Env& env, std::string const& name, NodeId node)
        -> DeclId {
        if (auto prev = lookup_name(name); prev.is_valid()) {
            er->report_error(ast->get_node_loc(node.as_ref()),
                             "redefinition of identifier '{}'", name);
            er->report_note(ast->get_node_loc(ds.get_decl(prev).node.as_ref()),
                            "previous value defined here");
            return DeclId::invalid();
        }

        auto decl = ds.gen_decl({.name = gen_name(name),
                                 .local_name = name,
                                 .node = node,
                                 .flags = {}});

        env.set(name, decl);

        return decl;
    }

    constexpr void push_env(std::string name = "") {
        envs.push_back({.scope_name = name, .items = {}});
    }

    constexpr void push_env_of(NodeId source_id) {
        push_env();
        auto top = get_top_env();
        for (auto const& [k, v] : file_envs[source_id].items) {
            top->set(k, v);
        }
    }

    constexpr void pop_env_into(NodeId source_id) {
        auto& dst = file_envs[source_id].items;
        auto  top = get_top_env();
        for (auto const& [k, v] : top->items) {
            dst[k] = v;
        }
    }

    constexpr void pop_env() { envs.pop_back(); }
    constexpr void pop_env(size_t cnt) {
        for (size_t i = 0; i < cnt; i++) {
            envs.pop_back();
        }
    }

    constexpr auto get_top_env() -> Env* { return &envs.at(envs.size() - 1); }
    constexpr auto get_root_env() -> Env* { return &envs.at(0); }
    constexpr auto get_mod_env() -> Env* { return &envs.at(1); }
    constexpr auto get_file_env() -> Env* { return &envs.at(2); }

    // ------------------------------------------------------------------------

    DeclStore ds;

    // Env                             mod_env;
    std::unordered_map<NodeId, Env> file_envs;
    std::vector<Env>                envs;

    ErrorReporter* er;
    FileStore*     fs;
};

// ============================================================================

auto make_map_of_decl_to_source_file(Ast const& ast, NodeId mod)
    -> std::unordered_map<NodeId, NodeId> {
    std::unordered_map<NodeId, NodeId> decl_to_source_file;

    auto mod_node = ast.get_node(mod.as_ref());
    ASSERT(mod_node.get_kind() == ast::NodeKind::Module);

    for (auto source_file : conv::module(ast, mod_node).children) {
        auto node = ast.get_node(source_file.as_ref());
        ASSERT(node.get_kind() == ast::NodeKind::SourceFile);

        // sort children based on order
        auto children = conv::source_file(ast, node).children;
        for (auto child : children) {
            decl_to_source_file[child] = source_file;
        }
    }

    return decl_to_source_file;
}

// ============================================================================

auto resolve_names(ast::Ast& ast, ast::NodeId root, ErrorReporter& er,
                   FileStore& fs, Options const& opt) -> ast::NodeId {
    auto node = ast.get_node(root.as_ref());
    ASSERT(node.get_kind() == ast::NodeKind::SourceFile);

    auto mod_decl = ast.get_node(conv::source_file(ast, node).mod.as_ref());
    ASSERT(mod_decl.get_kind() == ast::NodeKind::ModuleDecl);

    auto mod = parse_all_files_into_module(ast, node, mod_decl, fs, er, opt);

    auto dv = DependsVisitor{ast, &er, &fs};
    dv.visit(mod);

    auto order = topo_sort(ast, er, dv.dag);
    // for (auto const& n : order) {
    //     auto node = ast.get_node(n.as_ref());
    //     er.report_debug(node.get_loc(), "should run {}", n);
    // }

    auto source_file_to_decls = make_map_of_decl_to_source_file(ast, mod);

    auto ns = NameSolver{fs, er, ast};

    ns.visit_before_module(ast.get_node(mod.as_ref()),
                           conv::module(ast, ast.get_node(mod.as_ref())));

    for (auto const& ordered : order) {
        auto source_file = source_file_to_decls[ordered];
        ns.open_file(source_file);
        ns.visit(ordered);
        ns.close_file(source_file);
    }

    ns.visit_after_module(ast.get_node(mod.as_ref()),
                          conv::module(ast, ast.get_node(mod.as_ref())));

    return mod;
}

}  // namespace yal
