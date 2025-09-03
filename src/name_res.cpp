#include "name_res.hpp"

#include <algorithm>
#include <libassert/assert.hpp>
#include <ranges>
#include <string_view>

#include "arena.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "location.hpp"
#include "node.hpp"
#include "symbol.hpp"

namespace yal {

/*
 * TODO: remove all decl stuff from here! We NEED a 2 stage name resolution
 * system. Also, want to experiment with macros, and that requires 2 stages (one
 * for discovery and another for expansion).
 */

namespace rv = std::ranges::views;
namespace udense = ankerl::unordered_dense;

// Handle initial sorting of top-level names. This does not actually assign
// declarations, just finds out the order in which both full name resolution and
// semantic-analysis should run.
namespace sort {

// Store names that are on the top-level scope.
class TopName {
    std::string_view name;
    std::string_view topid;
    Location         loc;
    ast::Node*       node{};

public:
    constexpr TopName() = default;
    constexpr TopName(std::string_view name, std::string_view topid,
                      Location loc, ast::Node* node)
        : name{name}, topid{topid}, loc{loc}, node{node} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_topid() const -> std::string_view {
        return topid;
    }

    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }

    [[nodiscard]] constexpr auto get_node() const -> ast::Node* { return node; }
};

// Store names that are on the local scope.
class LocalName {
    std::string_view name;
    Location         loc;

public:
    constexpr LocalName() = default;
    constexpr LocalName(std::string_view name, Location loc)
        : name{name}, loc{loc} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }
};

// ----------------------------------------------------------------------------

// Environment map for the top-level items and their dependencies.
class TopEnv {
    udense::map<std::string_view, TopName> items;

    udense::map<std::string_view, udense::set<std::string_view>> depends;

public:
    TopEnv() = default;

    auto define(std::string_view name, std::string_view topid,
                Location const& loc, ast::Node* node) -> TopName {
        auto v = TopName{name, topid, loc, node};
        items[name] = v;
        return v;
    }

    void define_pending(std::string_view name, std::string_view topid,
                        Location loc) {
        ASSERT(!name.empty());
        items[name] = TopName{name, topid, loc, nullptr};
    }

    void add_depends(std::string_view item, std::string_view depends_on) {
        if (!item.empty() && !depends_on.empty())
            depends[item].insert(depends_on);
    }

    [[nodiscard]] auto lookup(std::string_view name) const -> TopName const* {
        if (auto it = items.find(name); it != items.end()) return &it->second;
        return nullptr;
    }

    [[nodiscard]] auto get_depends(std::string_view name) const
        -> udense::set<std::string_view> const* {
        if (auto it = depends.find(name); it != depends.end())
            return &it->second;
        return nullptr;
    }

    [[nodiscard]] constexpr auto get_all_items() const
        -> udense::map<std::string_view, TopName> const& {
        return items;
    }

    [[nodiscard]] constexpr auto get_all_depends() const
        -> udense::map<std::string_view, udense::set<std::string_view>> const& {
        return depends;
    }
};

// Environment map for the local items.
class LocalEnv {
    udense::map<std::string_view, LocalName> items;

    LocalEnv* parent{};

public:
    LocalEnv(LocalEnv* parent = nullptr) : parent{parent} {}

    auto child() -> LocalEnv { return {this}; }

    void define(std::string_view name, Location loc) {
        items[name] = LocalName{name, loc};
    }

    [[nodiscard]] auto lookup(std::string_view name) const -> LocalName const* {
        if (auto it = items.find(name); it != items.end()) return &it->second;

        return parent ? parent->lookup(name) : nullptr;
    }
};

// ============================================================================

class TopologicalSorter {
    std::vector<ast::Node*>     sorted;  // not const to avoid some copying
    std::vector<TopName const*> unmarked;
    udense::set<TopName const*> temp;
    udense::set<TopName const*> perm;

    udense::set<ast::Node const*> sorted_set;

    TopEnv const&  env;
    ErrorReporter& er;

    NameResOptions const& opt;

public:
    TopologicalSorter(TopEnv const& env, ErrorReporter& er,
                      NameResOptions const& opt)
        : env{env}, er{er}, opt{opt} {}

    void sort() {
        populate_unmarked();

        sorted.clear();
        temp.clear();
        perm.clear();

        while (!unmarked.empty()) {
            // get last element and pop
            auto n = *(unmarked.end() - 1);
            unmarked.pop_back();

            visit(n);
        }
    }

    [[nodiscard]] constexpr auto get_sorted() const
        -> std::vector<ast::Node*> const& {
        return sorted;
    }

    constexpr void move_sorted(std::vector<ast::Node*>& out) const {
        out = std::move(sorted);
    }

private:
    void populate_unmarked() {
        unmarked.clear();
        unmarked.reserve(env.get_all_items().size());
        for (auto const& [_, n] : env.get_all_items()) {
            if (n.get_node()) unmarked.push_back(&n);
        }

        std::ranges::sort(unmarked, [](TopName const* a, TopName const* b) {
            return a->get_loc().span.begin < b->get_loc().span.begin;
        });
    }

    void visit(TopName const* t) {
        if (perm.contains(t)) return;
        if (temp.contains(t)) {
            // cycle found
            if (opt.verbose) {
                er.report_error(t->get_loc(), "found dependency cycle on: {}",
                                t->get_name());
            }

            return;
        }

        temp.insert(t);
        if (auto depends = env.get_depends(t->get_name())) {
            for (auto const& d : *depends) {
                if (auto t = env.lookup(d); t && t->get_node()) visit(t);
            }
        }

        if (auto n = t->get_node(); !sorted_set.contains(n)) {
            sorted.push_back(n);
            sorted_set.insert(n);
        }

        perm.insert(t);
    }
};

// ============================================================================

class Sorter {
    ast::Ast&             ast;
    ErrorReporter&        er;
    NameResOptions const& opt;

    std::string_view detected_module_name;
    mem::Arena       scratch;

    TopEnv top_env;

public:
    Sorter(ErrorReporter& er, ast::Ast& ast, NameResOptions const& opt)
        : ast{ast}, er{er}, opt{opt} {}

    // find all dependencies and return sorted top-level declarations
    auto sort(std::span<ast::NodeFile* const> files) -> ast::NodeFlatModule* {
        find_dependencies(files);

        return ast.new_node_flat_module({}, sort_decls(), detected_module_name);
    }

    // Dumps the current state of the dependency chart to the given stream in
    // the mermaid diagram format.
    void dump_mermaid_dependency_flowchart(FILE* out) {
        fmt::println(out, "flowchart BT");
        fmt::println(out, "");

        for (auto const& [name, v] : top_env.get_all_items()) {
            if (v.get_node())
                fmt::println(out, "node_{0}[\"{0}\"]", name);
            else
                fmt::println(out, "node_{0}[\"{0} (unbound)\"]", name);

            if (auto depends = top_env.get_depends(name)) {
                for (auto dname : *depends) {
                    fmt::println(out, "node_{} --> node_{}", name, dname);
                }
            }
        }
    }

private:
    void fixup_topids() {
        std::vector<std::string_view> changelist;

        for (auto const& [topid, top] : top_env.get_all_items()) {
            if (auto depends = top_env.get_depends(top.get_topid())) {
                changelist.clear();

                for (auto const& dep : *depends) {
                    changelist.push_back(dep);
                }

                for (auto const& dep : changelist)
                    top_env.add_depends(top.get_name(), dep);
            }
        }
    }

    // ------------------------------------------------------------------------

    auto sort_decls() -> std::vector<ast::Node*> {
        auto ts = TopologicalSorter{top_env, er, opt};
        ts.sort();

        std::vector<ast::Node*> sorted;
        ts.move_sorted(sorted);

        return sorted;
    }

    // ------------------------------------------------------------------------

    void find_dependencies(std::span<ast::NodeFile* const> files) {
        for (auto file : files) find_deps_in_file(file);
        fixup_topids();
    }

    void find_deps_in_file(ast::NodeFile* file) {
        detect_module_name(file);

        for (auto child : file->get_children()) {
            if (child == nullptr) {
                // should not be null
                er.report_bug(file->get_loc(), "found null child in file node");

                continue;
            }

            find_deps_of_top_level(child);
        }
    }

    void find_deps_of_top_level(ast::Node* node) {
        // FIXME: handle @private(.file) decorators
        switch (node->get_kind()) {
            case ast::NodeKind::Var:
                find_deps_of_var(static_cast<ast::NodeVar*>(node));
                break;

            case ast::NodeKind::Def:
                find_deps_of_def(static_cast<ast::NodeDef*>(node));
                break;

            case ast::NodeKind::Func:
                find_deps_of_func(static_cast<ast::NodeFunc*>(node));
                break;

            case ast::NodeKind::Err: break;

            default:
                er.report_bug(node->get_loc(),
                              "unexpected node in top-level: {}",
                              node->get_kind());
        }
    }

    // ------------------------------------------------------------------------

    void find_deps_of_var(ast::NodeVar* node) {
        // we actually join all of the names and inits into one resolution-unit,
        // as we can not understand the relations between names, types and inits
        // until we have full types.

        auto topid = make_topid(node->get_names());
        auto local = LocalEnv{};

        find_deps_of_attributes(topid, local, node->get_attributes());
        find_deps_of_node_pack(topid, local, node->get_types());
        find_deps_of_node_pack(topid, local, node->get_inits());

        define_top_names(topid, node, node->get_names());
    }

    void find_deps_of_def(ast::NodeDef* node) {
        // we actually join all of the names and inits into one resolution-unit,
        // as we can not understand the relations between names, types and inits
        // until we have full types.

        auto topid = make_topid(node->get_names());
        auto local = LocalEnv{};

        define_top_names(topid, node, node->get_names());
        find_deps_of_attributes(topid, local, node->get_attributes());
        find_deps_of_node_pack(topid, local, node->get_types());
        find_deps_of_node_pack(topid, local, node->get_inits());
    }

    void find_deps_of_func(ast::NodeFunc* node) {
        auto loc = node->get_loc();

        auto name = node->get_name();
        auto name_loc = node->get_name_span().localize(loc.fileid);

        auto attached_type = node->get_attached_type();
        ASSERT(attached_type.empty(), "attached types not implemented");

        if (name.empty()) {
            auto error_name = ast.dupe_string("<error>");
            name = error_name;
            node->set_name(error_name);

            if (opt.verbose) {
                er.report_warn(name_loc, "[deps] got empty function name");
            }
        }

        // NOTE: this needs to change for attached types (maybe?)
        auto topid = name;

        auto local = LocalEnv{};

        // run before defining the name
        find_deps_of_attributes(topid, local, node->get_attributes());

        top_env.define(name, topid, loc, node);

        find_deps_of_func_args(topid, local, node->get_gargs());
        find_deps_of_func_args(topid, local, node->get_args());
        find_deps_of_func_ret(topid, local, node->get_ret());

        if (auto body = node->get_body()) {
            auto body_local = local.child();
            find_deps_of_node(topid, body_local, body);
        }
    }

    // ------------------------------------------------------------------------

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void find_deps_of_attributes(std::string_view topid, LocalEnv& local,
                                 ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto child : node->get_children()) {
            ASSERT(child != nullptr);
            ASSERT(child->get_kind() == ast::NodeKind::Attribute);

            auto attr_node = static_cast<ast::NodeAttribute*>(child);
            auto name = attr_node->get_qualified_name();
            if (name.empty()) name = attr_node->get_name();

            if (auto item = local.lookup(name)) {
                if (opt.log_decl_dependencies) {
                    er.report_debug(node->get_loc(),
                                    "Resolved attribute @{:?} to local", name);
                    er.report_note(item->get_loc(), "{:?} defined here",
                                   item->get_name());
                }
            }

            else {
                if (auto top = top_env.lookup(name)) {
                    if (opt.log_decl_dependencies) {
                        er.report_debug(node->get_loc(),
                                        "Resolved attribute @{:?} to global",
                                        name);
                        er.report_note(top->get_loc(), "{:?} defined here",
                                       top->get_name());
                    }
                } else {
                    top_env.define_pending(name, "", node->get_loc());
                }

                top_env.add_depends(topid, name);
            }

            for (auto kv : attr_node->get_children()) {
                if (kv) find_deps_of_node(topid, local, kv);
            }
        }
    }

    void find_deps_of_node_pack(std::string_view topid, LocalEnv& local,
                                ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto attr : node->get_children()) {
            ASSERT(attr != nullptr);

            find_deps_of_node(topid, local, attr);
        }
    }

    void find_deps_of_func_args(std::string_view topid, LocalEnv& local,
                                ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto arg : node->get_children()) {
            ASSERT(arg != nullptr);
            ASSERT(arg->get_kind() == ast::NodeKind::FuncArg);

            auto arg_node = static_cast<ast::NodeFuncArg*>(arg);
            if (arg_node->get_type())
                find_deps_of_node(topid, local, arg_node->get_type());

            // NOTE: not defining the decl for the arg here. We might actually
            // have to stop defining decls completally in this stage.

            local.define(arg_node->get_name(), arg_node->get_loc());
        }
    }

    void find_deps_of_func_ret(std::string_view topid, LocalEnv& local,
                               ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto ret : node->get_children()) {
            ASSERT(ret != nullptr);

            if (ret->get_kind() == ast::NodeKind::FuncNamedRet) {
                auto named_ret = static_cast<ast::NodeFuncNamedRet*>(ret);
                if (named_ret->get_type())
                    find_deps_of_node(topid, local, named_ret->get_type());

                local.define(named_ret->get_name(), named_ret->get_loc());
                continue;
            }

            find_deps_of_node(topid, local, ret);
        }
    }

    // ========================================================================

    void find_deps_of_node(std::string_view current_top_id, LocalEnv& local,
                           ast::Node* node) {
        if (node->get_kind() == ast::NodeKind::Id) {
            find_deps_of_id(current_top_id, local,
                            static_cast<ast::NodeId*>(node));
            return;
        }

        if (node->get_kind() == ast::NodeKind::Block) {
            auto inner_local = local.child();
            for (auto child : node->get_children()) {
                if (child)
                    find_deps_of_node(current_top_id, inner_local, child);
            }

            return;
        }

        if (node->get_kind() == ast::NodeKind::Var) {
            find_deps_of_local_var(current_top_id, local,
                                   static_cast<ast::NodeVar*>(node));
            return;
        }

        if (node->get_kind() == ast::NodeKind::Def) {
            er.report_bug(node->get_loc(), "NOT IMPLEMENTED: {}",
                          node->get_kind());
            return;
        }

        if (node->get_kind() == ast::NodeKind::Func) {
            er.report_bug(node->get_loc(), "NOT IMPLEMENTED: {}",
                          node->get_kind());
            return;
        }

        if (node->get_kind() == ast::NodeKind::FuncArg) {
            er.report_bug(node->get_loc(), "NOT IMPLEMENTED: {}",
                          node->get_kind());
            return;
        }

        for (auto child : node->get_children()) {
            if (child) find_deps_of_node(current_top_id, local, child);
        }
    }

    void find_deps_of_local_var(std::string_view current_top_id,
                                LocalEnv& local, ast::NodeVar* node) {
        find_deps_of_node_pack(current_top_id, local, node->get_attributes());
        find_deps_of_node_pack(current_top_id, local, node->get_types());
        find_deps_of_node_pack(current_top_id, local, node->get_inits());

        define_local_names(local, node->get_names());
    }

    void find_deps_of_id(std::string_view current_top_id, LocalEnv& local,
                         ast::NodeId* node) {
        auto value = node->get_value();

        if (auto item = local.lookup(value)) {
            if (opt.log_decl_dependencies) {
                er.report_debug(node->get_loc(), "Resolved {:?} to local",
                                value);
                er.report_note(item->get_loc(), "{:?} defined here",
                               item->get_name());
            }

            return;
        }

        if (auto top = top_env.lookup(value)) {
            if (opt.log_decl_dependencies) {
                er.report_debug(node->get_loc(), "Resolved {:?} to global",
                                value);
                er.report_note(top->get_loc(), "{:?} defined here",
                               top->get_name());
            }
        } else {
            top_env.define_pending(value, "", node->get_loc());
        }

        top_env.add_depends(current_top_id, value);
    }

    // ========================================================================

    void define_top_names(std::string_view topid, ast::Node* decl_node,
                          ast::NodePack* names_node) {
        if (names_node == nullptr) return;

        for (auto node : names_node->get_children()) {
            if (node == nullptr || node->is_err()) continue;

            ASSERT(node->get_kind() == ast::NodeKind::Id);
            auto name_node = static_cast<ast::NodeId*>(node);

            auto name = name_node->get_value();
            if (auto top = top_env.lookup(name); top && top->get_node()) {
                er.report_error(name_node->get_loc(),
                                "redefinition of identifier {:?}", name);
                er.report_note(top->get_loc(), "previous definition here");
            }

            if (!topid.empty()) {
                top_env.define(name, topid, node->get_loc(), decl_node);
            }
        }
    }

    void define_local_names(LocalEnv& local, ast::NodePack* names_node) {
        if (names_node == nullptr) return;

        for (auto node : names_node->get_children()) {
            if (node == nullptr || node->is_err()) continue;

            ASSERT(node->get_kind() == ast::NodeKind::Id);
            auto name_node = static_cast<ast::NodeId*>(node);

            auto name = name_node->get_value();
            local.define(name, node->get_loc());
        }
    }

    // ------------------------------------------------------------------------

    auto make_topid(ast::NodePack* names_node) -> std::string_view {
        if (names_node == nullptr) return "";

        std::string top_id;
        auto&&      it = std::back_inserter(top_id);

        for (auto node : names_node->get_children()) {
            if (node == nullptr) {
                if (opt.verbose)
                    er.report_warn(node->get_loc(),
                                   "got null name node in declaration");

                continue;
            }

            if (node->is_err()) {
                if (opt.verbose)
                    er.report_warn(node->get_loc(),
                                   "got error name node in declaration");

                continue;
            }

            ASSERT(node->get_kind() == ast::NodeKind::Id);
            auto name_node = static_cast<ast::NodeId*>(node);

            fmt::format_to(it, "_{}", name_node->get_value());
        }

        return scratch.alloc_string_view(std::string_view{top_id}.substr(1));
    }

    // ========================================================================

    void detect_module_name(ast::NodeFile* file) {
        auto file_module_name = file->get_module_name();

        // detect the module name in case we have not do so already
        if (detected_module_name.empty()) {
            if (!file_module_name.empty()) {
                detected_module_name = file_module_name;
            }

            else if (opt.verbose) {
                er.report_warn(file->get_loc(), "missing module name in file");
            }
        }

        // check if the module name is the same as the detected one
        else if (!file_module_name.empty() &&
                 detected_module_name != file_module_name) {
            er.report_error(file->get_loc(),
                            "file has different module declaration, "
                            "expected {:?} but got {:?}",
                            detected_module_name, file_module_name);
        }
    }
};

}  // namespace sort

namespace name_res {

// Store names that are on the top-level scope.
class Resolved {
    std::string_view name;

    ast::Node* node{};
    ast::Node* decl_node{};
    Symbol*    decl{};

public:
    constexpr Resolved() = default;
    constexpr Resolved(std::string_view name, ast::Node* node,
                       ast::Node* decl_node, Symbol* decl)
        : name{name}, node{node}, decl_node{decl_node}, decl{decl} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_node() const -> ast::Node* { return node; }
    [[nodiscard]] constexpr auto get_decl() const -> Symbol* { return decl; }
};

class Env {
    udense::map<std::string_view, Resolved> items;

    Env* parent{};

public:
    Env() = default;
    Env(Env* parent) : parent{parent} {}

    [[nodiscard]] auto child() -> Env { return {this}; }

    void define(std::string_view name, ast::Node* node, ast::Node* decl_node,
                Symbol* decl) {
        items[name] = {name, node, decl_node, decl};
    }

    [[nodiscard]] auto lookup(std::string_view name) const -> Resolved const* {
        if (auto it = items.find(name); it != items.end()) return &it->second;

        return parent ? parent->lookup(name) : nullptr;
    }
};

class NameRes {
    ast::Ast&             ast;
    SymbolStore&          ds;
    ErrorReporter&        er;
    NameResOptions const& opt;

public:
    NameRes(ast::Ast& ast, SymbolStore& ds, ErrorReporter& er,
            NameResOptions const& opt)
        : ast{ast}, ds{ds}, er{er}, opt{opt} {}

    void resolve(ast::NodeFlatModule* mod) {
        // TODO: define root environment
        auto env = Env{};

        resolve_node(env, mod);
    }

private:
    void resolve_node(Env& env, ast::Node* node) {
        if (node == nullptr) return;

        switch (node->get_kind()) {
            case ast::NodeKind::Var:
                resolve_var(env, static_cast<ast::NodeVar*>(node));
                break;

            case ast::NodeKind::Def:
                resolve_def(env, static_cast<ast::NodeDef*>(node));
                break;

            case ast::NodeKind::Func:
                resolve_func(env, static_cast<ast::NodeFunc*>(node));
                break;

            case ast::NodeKind::Attribute:
                resolve_attr(env, static_cast<ast::NodeAttribute*>(node));
                break;

            case ast::NodeKind::FuncArg:
                resolve_func_arg(env, static_cast<ast::NodeFuncArg*>(node));
                break;

            case ast::NodeKind::FuncNamedRet:
                resolve_func_named_ret(
                    env, static_cast<ast::NodeFuncNamedRet*>(node));
                break;

            case ast::NodeKind::Block:
                resolve_block(env, static_cast<ast::NodeBlock*>(node));
                break;

            case ast::NodeKind::Id:
                resolve_id(env, static_cast<ast::NodeId*>(node));
                break;

            default:
                for (auto const& child : node->get_children()) {
                    resolve_node(env, child);
                }
        }
    }

    // ------------------------------------------------------------------------

    void resolve_var(Env& env, ast::NodeVar* node) {
        resolve_node(env, node->get_attributes());
        resolve_node(env, node->get_types());
        resolve_node(env, node->get_inits());

        define_names(env, node, node->get_names());
    }

    void resolve_def(Env& env, ast::NodeDef* node) {
        resolve_node(env, node->get_attributes());
        resolve_node(env, node->get_types());

        define_names(env, node, node->get_names());
        resolve_node(env, node->get_inits());
    }

    void resolve_func(Env& penv, ast::NodeFunc* node) {
        auto loc = node->get_loc();

        auto name = node->get_name();
        auto name_loc = node->get_name_span().localize(loc.fileid);

        auto attached_type = node->get_attached_type();
        ASSERT(attached_type.empty(), "attached types not implemented");

        if (name.empty()) {
            auto error_name = ast.dupe_string("<error>");
            name = error_name;
            node->set_name(error_name);

            if (opt.verbose) {
                er.report_warn(name_loc, "[name res] got empty function name");
            }
        }

        // FIXME: generate actual link name
        auto decl = ds.new_sym(name_loc, name, name);
        node->set_decl(decl);
        penv.define(name, node, node, decl);

        auto env = penv.child();
        resolve_node(env, node->get_attributes());
        resolve_node(env, node->get_gargs());
        resolve_node(env, node->get_args());
        resolve_node(env, node->get_ret());
        resolve_node(env, node->get_body());
    }

    void resolve_attr(Env& env, ast::NodeAttribute* node) {
        auto name = node->get_qualified_name();
        if (name.empty()) name = node->get_name();

        if (auto v = env.lookup(name)) {
            node->set_decl(v->get_decl());
        }

        // undefined identifier
        else {
            report_undefined_identifier(name, node->get_loc());
        }
    }

    void resolve_func_arg(Env& env, ast::NodeFuncArg* node) {
        // FIXME: generate actual link name
        auto name = node->get_name();
        auto decl = ds.new_sym(node->get_loc(), name, name);
        node->set_decl(decl);
        env.define(name, node, node, decl);
    }

    void resolve_func_named_ret(Env& env, ast::NodeFuncNamedRet* node) {
        // FIXME: generate actual link name
        auto name = node->get_name();
        auto decl = ds.new_sym(node->get_loc(), name, name);
        node->set_decl(decl);
        env.define(name, node, node, decl);
    }

    void resolve_block(Env& penv, ast::NodeBlock* node) {
        auto env = penv.child();
        for (auto const& child : node->get_children()) {
            resolve_node(env, child);
        }
    }

    void resolve_id(Env& env, ast::NodeId* node) {
        auto name = node->get_value();
        if (auto v = env.lookup(name)) {
            node->set_decl(v->get_decl());
        }

        // undefined identifier
        else {
            report_undefined_identifier(name, node->get_loc());
        }
    }

    // ------------------------------------------------------------------------

    void define_names(Env& env, ast::Node* decl_node,
                      ast::NodePack* names_node) {
        if (names_node == nullptr) return;

        for (auto node : names_node->get_children()) {
            if (node == nullptr || node->is_err()) continue;

            ASSERT(node->get_kind() == ast::NodeKind::Id);
            auto name_node = static_cast<ast::NodeId*>(node);

            auto name = name_node->get_value();

            // FIXME: make actual link name
            auto decl = ds.new_sym(node->get_loc(), name, name);
            decl_node->set_decl(decl);
            env.define(name, node, decl_node, decl);
        }
    }

    // ========================================================================

    void report_undefined_identifier(std::string_view name, Location loc) {
        er.report_error(loc, "undefined identifier: {:?}", name);
    }
};

}  // namespace name_res

auto sort_declarations_and_resolve_top_level(
    ast::Ast& ast, SymbolStore& ds, std::span<ast::NodeFile* const> root,
    ErrorReporter& er, NameResOptions const& opt) -> ast::NodeFlatModule* {
    ast::NodeFlatModule* resolved;

    {
        auto srt = sort::Sorter{er, ast, opt};
        resolved = srt.sort(root);

        if (opt.dump_dependencies_as_mermaid)
            srt.dump_mermaid_dependency_flowchart(stdout);
    }

    auto nres = name_res::NameRes{ast, ds, er, opt};
    nres.resolve(resolved);

    return resolved;
}

}  // namespace yal
