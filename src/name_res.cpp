#include "name_res.hpp"

#include <libassert/assert.hpp>
#include <ranges>
#include <string_view>

#include "arena.hpp"
#include "ast.hpp"
#include "decl.hpp"
#include "error_reporter.hpp"
#include "location.hpp"
#include "node.hpp"

namespace yal {

/*
 * TODO: remove all decl stuff from here! We NEED a 2 stage name resolution
 * system. Also, want to experiment with macros, and that requires 2 stages (one
 * for discovery and another for expansion).
 */

namespace rv = std::ranges::views;
namespace udense = ankerl::unordered_dense;

class TopName {
    std::string_view name;
    std::string_view topid;
    Location         loc;
    Decl*            decl{};
    ast::Node*       node{};

public:
    constexpr TopName() = default;
    constexpr TopName(std::string_view name, std::string_view topid,
                      Location loc, Decl* decl, ast::Node* node)
        : name{name}, topid{topid}, loc{loc}, decl{decl}, node{node} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_topid() const -> std::string_view {
        return topid;
    }

    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }

    [[nodiscard]] constexpr auto get_node() const -> ast::Node* { return node; }

    [[nodiscard]] constexpr auto get_decl() const -> Decl* { return decl; }
};

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

class TopEnv {
    udense::map<std::string_view, TopName> items;

    udense::map<std::string_view, udense::set<std::string_view>> depends;

public:
    TopEnv() = default;

    auto define(std::string_view name, std::string_view topid, Decl* decl,
                ast::Node* node) -> TopName {
        auto v = TopName{name, topid, decl->get_loc(), decl, node};
        items[name] = v;
        return v;
    }

    void define_pending(std::string_view name, std::string_view topid,
                        Location loc) {
        ASSERT(!name.empty());
        items[name] = TopName{name, topid, loc, nullptr, nullptr};
    }

    void add_depends(std::string_view item, std::string_view depends_on) {
        if (!item.empty() && !depends_on.empty())
            depends[item].insert(depends_on);
    }

    [[nodiscard]] auto lookup(std::string_view name) const -> TopName const* {
        if (auto it = items.find(name); it != items.end()) return &it->second;
        return nullptr;
    }

    auto get_depends(std::string_view name)
        -> udense::set<std::string_view> const* {
        if (auto it = depends.find(name); it != depends.end())
            return &it->second;
        return nullptr;
    }

    auto get_all_items() -> udense::map<std::string_view, TopName> const& {
        return items;
    }

    auto get_all_depends()
        -> udense::map<std::string_view, udense::set<std::string_view>> const& {
        return depends;
    }
};

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

class NameRes {
    ast::Ast&             ast;
    ErrorReporter&        er;
    NameResOptions const& opt;

    DeclStore& decl_store;

    std::string_view detected_module_name;
    mem::Arena       scratch;

    TopEnv top_env;

public:
    NameRes(ErrorReporter& er, ast::Ast& ast, DeclStore& decl_store,
            NameResOptions const& opt)
        : ast{ast}, er{er}, opt{opt}, decl_store{decl_store} {}

    auto run(std::span<ast::NodeFile* const> files) -> ast::NodeFlatModule* {
        for (auto file : files) {
            run_for_file(file);
        }

        std::span<ast::Node*> children;
        return ast.new_node_flat_module({}, children, detected_module_name);
    }

    // Dumps the current state of the dependency chart to the given stream in
    // the mermaid diagram format.
    void dump_mermaid_dependency_flowchart(FILE* out) {
        fmt::println(out, "flowchart BT");
        fmt::println(out, "");

        for (auto const& [name, v] : top_env.get_all_items()) {
            auto decl = v.get_decl();
            if (decl)
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
    void run_for_file(ast::NodeFile* file_root) {
        detect_module_name(file_root);

        for (auto child : file_root->get_children()) {
            if (child == nullptr) {
                // should not be null
                er.report_bug(file_root->get_loc(),
                              "found null child in file node");

                continue;
            }

            run_for_top(child);
        }

        fixup_topids();
    }

    void run_for_top(ast::Node* node) {
        switch (node->get_kind()) {
            case ast::NodeKind::Var:
                run_for_top_var(static_cast<ast::NodeVar*>(node));
                break;

            case ast::NodeKind::Def:
                run_for_top_def(static_cast<ast::NodeDef*>(node));
                break;

            case ast::NodeKind::Func:
                run_for_top_func(static_cast<ast::NodeFunc*>(node));
                break;

            default:
                er.report_bug(node->get_loc(),
                              "unexpected node in top-level: {}",
                              node->get_kind());
        }
    }

    void run_for_top_var(ast::NodeVar* node) {
        // we actually join all of the names and inits into one resolution-unit,
        // as we can not understand the relations between names, types and inits
        // until we have full types.

        auto topid = build_var_def_topid(node->get_names());
        auto local = LocalEnv{};

        run_for_attributes(topid, local, node->get_attributes());
        run_for_node_pack(topid, local, node->get_types());
        run_for_node_pack(topid, local, node->get_inits());

        define_top_names(topid, node->get_names());
    }

    void run_for_top_def(ast::NodeDef* node) {
        // we actually join all of the names and inits into one resolution-unit,
        // as we can not understand the relations between names, types and inits
        // until we have full types.

        auto topid = build_var_def_topid(node->get_names());
        auto local = LocalEnv{};

        define_top_names(topid, node->get_names());
        run_for_attributes(topid, local, node->get_attributes());
        run_for_node_pack(topid, local, node->get_types());
        run_for_node_pack(topid, local, node->get_inits());
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void run_for_top_func(ast::NodeFunc* node) {
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
                er.report_warn(name_loc, "got empty function name");
            }
        }

        // NOTE: this needs to change for attached types (maybe?)
        auto topid = name;

        auto local = LocalEnv{};

        // run before defining the name
        run_for_attributes(topid, local, node->get_attributes());

        auto decl = decl_store.new_decl(name_loc, name, name);
        top_env.define(name, topid, decl, node);

        run_for_func_args(topid, local, node->get_gargs());
        run_for_func_args(topid, local, node->get_args());
        run_for_func_ret(topid, local, node->get_ret());

        if (auto body = node->get_body()) {
            auto body_local = local.child();
            run_for_node(topid, body_local, body);
        }
    }

    void run_for_func_args(std::string_view topid, LocalEnv& local,
                           ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto arg : node->get_children()) {
            ASSERT(arg != nullptr);
            ASSERT(arg->get_kind() == ast::NodeKind::FuncArg);

            auto arg_node = static_cast<ast::NodeFuncArg*>(arg);
            if (arg_node->get_type())
                run_for_node(topid, local, arg_node->get_type());

            // NOTE: not defining the decl for the arg here. We might actually
            // have to stop defining decls completally in this stage.

            local.define(arg_node->get_name(), arg_node->get_loc());
        }
    }

    void run_for_func_ret(std::string_view topid, LocalEnv& local,
                          ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto ret : node->get_children()) {
            ASSERT(ret != nullptr);

            if (ret->get_kind() == ast::NodeKind::FuncNamedRet) {
                auto named_ret = static_cast<ast::NodeFuncNamedRet*>(ret);
                if (named_ret->get_type())
                    run_for_node(topid, local, named_ret->get_type());

                local.define(named_ret->get_name(), named_ret->get_loc());
                continue;
            }

            run_for_node(topid, local, ret);
        }
    }

    // ------------------------------------------------------------------------

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    void run_for_attributes(std::string_view topid, LocalEnv& local,
                            ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto child : node->get_children()) {
            ASSERT(child != nullptr);
            ASSERT(child->get_kind() == ast::NodeKind::Attribute);

            auto attr_node = static_cast<ast::NodeAttribute*>(child);
            auto name = attr_node->get_name();

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
                if (kv) run_for_node(topid, local, kv);
            }
        }
    }

    void run_for_node_pack(std::string_view topid, LocalEnv& local,
                           ast::NodePack* node) {
        if (node == nullptr) return;

        for (auto attr : node->get_children()) {
            ASSERT(attr != nullptr);

            run_for_node(topid, local, attr);
        }
    }

    // ------------------------------------------------------------------------

    void define_top_names(std::string_view topid, ast::NodePack* names_node) {
        if (names_node == nullptr) return;

        for (auto node : names_node->get_children()) {
            if (node == nullptr || node->is_err()) continue;

            ASSERT(node->get_kind() == ast::NodeKind::Id);
            auto name_node = static_cast<ast::NodeId*>(node);

            auto name = name_node->get_value();
            if (auto top = top_env.lookup(name); top && top->get_decl()) {
                er.report_error(name_node->get_loc(),
                                "redefinition of identifier {:?}", name);
                er.report_note(top->get_loc(), "previous definition here");
            }

            if (!topid.empty()) {
                auto decl =
                    decl_store.new_decl(name_node->get_loc(), name, name);
                top_env.define(name, topid, decl, node);
            }
        }
    }

    auto build_var_def_topid(ast::NodePack* names_node) -> std::string_view {
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

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
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

            if (auto decl = top.get_decl()) {
                auto node = top.get_node();
                ASSERT(node != nullptr);

                node->set_decl(decl);
            } else {
                er.report_error(top.get_loc(), "undefined identifier: {:?}",
                                top.get_name());
            }
        }
    }

    // ========================================================================

    void run_for_node(std::string_view current_top_id, LocalEnv& local,
                      ast::Node* node) {
        if (node->get_kind() == ast::NodeKind::Id) {
            run_for_id(current_top_id, local, static_cast<ast::NodeId*>(node));
            return;
        }

        if (node->get_kind() == ast::NodeKind::Block) {
            auto inner_local = local.child();
            for (auto child : node->get_children()) {
                if (child) run_for_node(current_top_id, inner_local, child);
            }

            return;
        }

        if (node->get_kind() == ast::NodeKind::Var) {
            run_for_local_var(current_top_id, local,
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
            if (child) run_for_node(current_top_id, local, child);
        }
    }

    // ========================================================================

    void run_for_id(std::string_view current_top_id, LocalEnv& local,
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

    void run_for_local_var(std::string_view current_top_id, LocalEnv& local,
                           ast::NodeVar* node) {
        // we actually join all of the names and inits into one resolution-unit,
        // as we can not understand the relations between names, types and inits
        // until we have full types.

        run_for_node_pack(current_top_id, local, node->get_attributes());
        run_for_node_pack(current_top_id, local, node->get_types());
        run_for_node_pack(current_top_id, local, node->get_inits());

        define_local_names(local, node->get_names());
    }

    // ------------------------------------------------------------------------

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

    // ========================================================================

    void detect_module_name(ast::NodeFile* file_root) {
        // detect the module name in case we have not do so already
        if (detected_module_name.empty()) {
            auto file_module_name = file_root->get_module_name();
            if (!file_module_name.empty()) {
                detected_module_name = file_module_name;
            }

            else if (opt.verbose) {
                er.report_warn(file_root->get_loc(),
                               "missing module name in file");
            }
        }

        // check if the module name is the same as the detected one
        else {
            auto file_module_name = file_root->get_module_name();
            if (!file_module_name.empty() &&
                detected_module_name != file_module_name) {
                er.report_error(file_root->get_loc(),
                                "file has different module declaration, "
                                "expected {:?} but got {:?}",
                                detected_module_name, file_module_name);
            }
        }
    }
};

auto sort_declarations_and_resolve_top_level(
    ast::Ast& ast, DeclStore& ds, std::span<ast::NodeFile* const> root,
    ErrorReporter& er, NameResOptions const& opt) -> ast::NodeFlatModule* {
    auto nres = NameRes{er, ast, ds, opt};
    auto resolved = nres.run(root);

    if (opt.dump_dependencies_as_mermaid)
        nres.dump_mermaid_dependency_flowchart(stdout);

    return resolved;
}

}  // namespace yal
