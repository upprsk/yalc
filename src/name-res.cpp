#include "name-res.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
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
#include "name-order.hpp"
#include "yal.hpp"

namespace yal {
using ast::Ast;
using ast::Node;
using ast::NodeId;
namespace conv = ast::conv;

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

    if (!conv::module_decl_is_valid(mod_found)) {
        // er.report_error(mod_found.get_loc(), "invalid module found");
        return NodeId::invalid();
    }

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
    // when in single file mode, ignore scanning the directory
    if (opt.single_file) {
        auto mod_name = conv::module_decl(ast, mod_decl).name;
        return ast.new_module(start_node.get_loc(), mod_name,
                              std::array{start_node.get_id()});
    }

    auto start_file = start_node.get_loc().fileid;
    auto dir = fs.add_dir_for(start_file);
    auto all_files = fs.get_files_in_dir(dir);

    std::vector<NodeId> files{start_node.get_id()};
    for (auto const& file : all_files) {
        // don't re-parse ourselves
        if (start_file == file) continue;

        // FIXME: use the file id and not the filename for this function
        auto src_file = parse_and_load_module_into_ast(
            fs.get_filename(file), mod_decl, ast, fs, er, opt);
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
        uint32_t                                id;
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
            auto flags = DeclFlags::builder();
            if (is_file_private_decl(data.decorators))
                flags = flags.set_private_file();

            define_at_top(std::string{name}, node.get_id(), flags.build());
        } else {
            // this is namespaced, check that what we namespace under exists
            auto const& name = ast->get_identifier(names.ids[0].as_id());
            auto        decl_id = lookup_name(name);
            if (!decl_id.is_valid()) {
                er->report_error(node.get_loc(), "undefined identifier {:?}",
                                 name);
                return;
            }

            // TODO: store the namespace in the func decl
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

    void visit_before_top_var_decl(Node const& /*node*/,
                                   conv::TopVarDecl const& data) override {
        next_decl_is_file_private = is_file_private_decl(data.decorators);
    }

    void visit_after_var_decl(Node const&          node,
                              conv::VarDecl const& data) override {
        auto flags = DeclFlags::builder();
        if (next_decl_is_file_private) {
            next_decl_is_file_private = false;
            flags = flags.set_private_file();
        }

        auto names = conv::id_pack(*ast, ast->get_node(data.ids.as_ref()));
        for (auto nid : names.ids) {
            auto const& name = ast->get_identifier(nid.as_id());
            define_at_top(name, node.get_id(), flags.build());
        }
    }

    void visit_before_top_def_decl(Node const& /*node*/,
                                   conv::TopDefDecl const& data) override {
        next_decl_is_file_private = is_file_private_decl(data.decorators);
    }

    void visit_after_def_decl(Node const&          node,
                              conv::DefDecl const& data) override {
        auto flags = DeclFlags::builder();
        if (next_decl_is_file_private) {
            next_decl_is_file_private = false;
            flags = flags.set_private_file();
        }

        auto names = conv::id_pack(*ast, ast->get_node(data.ids.as_ref()));
        for (auto nid : names.ids) {
            auto const& name = ast->get_identifier(nid.as_id());
            define_at_top(name, node.get_id(), flags.build());
        }
    }

    void visit_after_func_param(Node const&            node,
                                conv::FuncParam const& data) override {
        define_at_top(std::string{data.name}, node.get_id());
    }

    void visit_after_ret_pack(Node const&          node,
                              conv::RetPack const& data) override {
        auto ret = data.ret;
        ASSERT((ret.size() & 1) == 0);

        for (size_t i = 0; i < ret.size(); i += 2) {
            if (ret[i].is_valid()) {
                define_at_top(ast->get_identifier(ret[i].as_id()),
                              node.get_id());
            }
        }
    }

    // ------------------------------------------------------------------------

    void visit_id(Node const& node, conv::Id const& id) override {
        auto decl_id = lookup_name(std::string{id.name});
        if (!decl_id.is_valid()) {
            er->report_error(node.get_loc(), "undefined identifier {:?}",
                             id.name);
            return;
        }

        auto& ref = ast->get_node_ref(node.get_id().as_ref());
        ref.set_second(NodeId::from_raw_data(decl_id.value()));

        // auto const& decl = ds.get_decl(decl_id);
        // if (decl.node.is_valid()) {
        //     er->report_note(
        //         node.get_loc(),
        //         "found decl for id {:?}: {}: local_name={}, name={},
        //         node={}", id.name, decl_id, decl.local_name, decl.name,
        //         decl.node);
        //     er->report_debug(ast->get_node_loc(decl.node.as_ref()),
        //                      "defined here");
        // }
    }

    // ========================================================================

    auto is_file_private_decl(std::span<NodeId const> decorators) const
        -> bool {
        return conv::decorators_private_kind(*ast, decorators) ==
               conv::PrivateKind::File;
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

    // constexpr auto define_at_file(std::string const& name, NodeId node)
    //     -> DeclId {
    //     return define_at(*get_file_env(), name, node);
    // }

    // constexpr auto define_at_mod(std::string const& name, NodeId node)
    //     -> DeclId {
    //     return define_at(*get_mod_env(), name, node);
    // }

    constexpr auto define_at_top(std::string const& name, NodeId node,
                                 DeclFlags flags = {}) -> DeclId {
        return define_at(*get_top_env(), name, node, flags);
    }

    constexpr auto define_at(Env& env, std::string const& name, NodeId node,
                             DeclFlags flags = {}) -> DeclId {
        // fmt::println(stderr, "define_at({:?})", name);
        // for (size_t i = 0; auto const& inner : envs) {
        //     if (env.id == inner.id)
        //         fmt::print(stderr, "*");
        //     else
        //         fmt::print(stderr, "-");
        //
        //     fmt::println(stderr, " {}: {:?} ({})", i, inner.scope_name,
        //                  inner.id);
        //
        //     for (auto const& [k, v] : inner.items) {
        //         fmt::println(stderr, "  - {:?}", k);
        //     }
        //
        //     i++;
        // }

        if (envs.size() > 2 && env.id == get_file_env()->id) {
            // check if the name is already defined in the file
            if (auto prev = env.get(name); prev.is_valid()) {
                er->report_error(ast->get_node_loc(node.as_ref()),
                                 "redefinition of identifier '{}'", name);
                er->report_note(
                    ast->get_node_loc(get_ds().get_decl(prev).node.as_ref()),
                    "previous value defined here");
                return DeclId::invalid();
            }

            // check if the name is already defined in the module
            if (auto prev = get_mod_env()->get(name); prev.is_valid()) {
                er->report_error(ast->get_node_loc(node.as_ref()),
                                 "redefinition of identifier '{}'", name);
                er->report_note(
                    ast->get_node_loc(get_ds().get_decl(prev).node.as_ref()),
                    "previous value defined here");
                return DeclId::invalid();
            }
        }

        auto decl = get_ds().gen_decl({.name = gen_name(name),
                                       .local_name = name,
                                       .node = node,
                                       .flags = flags});

        env.set(name, decl);

        return decl;
    }

    constexpr void push_env(std::string name = "") {
        envs.push_back({.scope_name = name, .items = {}, .id = next_env_id++});
    }

    constexpr void push_env_of(NodeId source_id) {
        push_env();
        auto top = get_top_env();
        for (auto const& [k, v] : file_envs[source_id].items) {
            top->set(k, v);
        }
    }

    constexpr void pop_env_into(NodeId source_id) {
        auto& file_env = file_envs[source_id].items;
        auto  mod_env = try_get_mod_env();
        auto  ds = ast->get_decl_store();

        auto top = get_top_env();
        for (auto const& [k, v] : top->items) {
            auto decl = ds->get_decl(v);

            // when the declaration is file private, we put it in the file env,
            // when it is module visible, we promote it to the module
            if (decl.is_private_file())
                file_env[k] = v;
            else if (mod_env != nullptr)
                mod_env->set(k, v);

            // we may have empty envs here in case there were undefined
            // indentifiers. This means that we don't have a mod_env.
        }

        pop_env();
    }

    constexpr void pop_env() {
        // we may have empty envs here in case there were undefined indentifiers
        if (!envs.empty()) envs.pop_back();
    }

    constexpr void pop_env(size_t cnt) {
        for (size_t i = 0; i < cnt; i++) {
            envs.pop_back();
        }
    }

    constexpr auto get_top_env() -> Env* { return &envs.at(envs.size() - 1); }
    constexpr auto get_root_env() -> Env* { return &envs.at(0); }
    constexpr auto get_mod_env() -> Env* { return &envs.at(1); }
    constexpr auto get_file_env() -> Env* { return &envs.at(2); }

    constexpr auto try_get_mod_env() -> Env* {
        if (envs.size() > 1) return get_mod_env();
        return nullptr;
    }

    constexpr auto get_ds() const -> DeclStore& {
        return *ast->get_decl_store();
    }

    // ------------------------------------------------------------------------

    // FIXME: this is a terrible way to do this.
    bool     next_decl_is_file_private = false;
    uint32_t next_env_id{};

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

    auto order = sort::topo_sort_top_decls(ast, mod, er, fs);
    // for (auto const& n : order) {
    //     auto node = ast.get_node(n.as_ref());
    //     er.report_debug(node.get_loc(), "should run {}", n);
    // }

    auto decls_to_source_files = make_map_of_decl_to_source_file(ast, mod);

    auto ns = NameSolver{fs, er, ast};

    ns.visit_before_module(ast.get_node(mod.as_ref()),
                           conv::module(ast, ast.get_node(mod.as_ref())));

    for (auto const& ordered : order) {
        auto source_file = decls_to_source_files[ordered];
        ns.open_file(source_file);
        ns.visit(ordered);
        ns.close_file(source_file);
    }

    ns.visit_after_module(ast.get_node(mod.as_ref()),
                          conv::module(ast, ast.get_node(mod.as_ref())));

    return mod;
}

}  // namespace yal
