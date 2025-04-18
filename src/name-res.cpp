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
#include "types.hpp"
#include "value.hpp"
#include "yal.hpp"

namespace yal {
using ast::Ast;
using ast::Node;
namespace conv = ast::conv;

auto parse_and_load_module_into_ast(std::filesystem::path const& filepath,
                                    Node const& mod_decl, ast::Ast& ast,
                                    FileStore& fs, ErrorReporter& er,
                                    Options const& opt) -> Node* {
    auto src_file = load_and_parse_into_ast(fs, er, filepath, ast, opt);
    if (!src_file) {
        er.report_bug(mod_decl.get_loc(),
                      "when scanning for module files, failed to get {}",
                      filepath.string());
        return nullptr;
    }

    auto mod_found = conv::source_file(*src_file).mod;
    if (!mod_found) {
        // er.report_error(mod_found.get_loc(), "invalid module found");
        return nullptr;
    }

    auto mod_found_name = conv::module_decl(*mod_found).name;
    auto mod_name = conv::module_decl(mod_decl).name;
    if (mod_found_name != mod_name) {
        er.report_bug(mod_found->get_loc(),
                      "found mixed module name declarations in directory. "
                      "Want {:?} but found {:?}",
                      mod_name, mod_found_name);
        return nullptr;
    }

    return src_file;
}

auto parse_all_files_into_module(ast::Ast& ast, ast::Node& start_node,
                                 Node const& mod_decl, FileStore& fs,
                                 ErrorReporter& er, Options const& opt) {
    // when in single file mode, ignore scanning the directory
    if (opt.single_file) {
        auto mod_name = conv::module_decl(mod_decl).name;
        return ast.new_module(start_node.get_loc(), mod_name,
                              std::array{&start_node});
    }

    auto start_file = start_node.get_loc().fileid;
    auto dir = fs.add_dir_for(start_file);
    auto all_files = fs.get_files_in_dir(dir);

    std::vector<Node*> files{&start_node};
    for (auto const& file : all_files) {
        // don't re-parse ourselves
        if (start_file == file) continue;

        // FIXME: use the file id and not the filename for this function
        auto src_file = parse_and_load_module_into_ast(
            fs.get_filename(file), mod_decl, ast, fs, er, opt);
        if (!src_file) files.push_back(src_file);
    }

    auto mod_name = conv::module_decl(mod_decl).name;
    return ast.new_module(start_node.get_loc(), mod_name, files);
}

// ============================================================================

struct NameSolver : public ast::Visitor<Ast> {
    explicit NameSolver(FileStore& fs, types::TypeStore& ts, ErrorReporter& er,
                        Ast& ast)
        : ast::Visitor<Ast>{ast}, er{&er}, fs{&fs}, ts{&ts} {
        define_buitins();
    }

    void define_buitins() {
        // push the root scope
        push_env();

        // FIXME: how do we reference back to this?
        // FIXME: need to set the `value` field to the type, not the `type`
        // field, as this needs to diferentiate between a type and the type
        // type.
        define_at_top("usize", nullptr,
                      {.type = ts->get_type(), .data = ts->get_usize()});
        define_at_top("isize", nullptr,
                      {.type = ts->get_type(), .data = ts->get_isize()});

        define_at_top("i64", nullptr,
                      {.type = ts->get_type(), .data = ts->get_i64()});
        define_at_top("u64", nullptr,
                      {.type = ts->get_type(), .data = ts->get_u64()});
        define_at_top("i32", nullptr,
                      {.type = ts->get_type(), .data = ts->get_i32()});
        define_at_top("u32", nullptr,
                      {.type = ts->get_type(), .data = ts->get_u32()});
        define_at_top("i16", nullptr,
                      {.type = ts->get_type(), .data = ts->get_i16()});
        define_at_top("u16", nullptr,
                      {.type = ts->get_type(), .data = ts->get_u16()});
        define_at_top("i8", nullptr,
                      {.type = ts->get_type(), .data = ts->get_i8()});
        define_at_top("u8", nullptr,
                      {.type = ts->get_type(), .data = ts->get_u8()});

        define_at_top("f32", nullptr,
                      {.type = ts->get_type(), .data = ts->get_f32()});
        define_at_top("f64", nullptr,
                      {.type = ts->get_type(), .data = ts->get_f64()});

        define_at_top("bool", nullptr);

        define_at_top("true", nullptr);
        define_at_top("false", nullptr);

        define_at_top("nil", nullptr);
    }

    struct Env {
        constexpr void set(std::string const& name, DeclId id) {
            items[name] = id;
        }

        // FIXME: creating a heap copy of `name` for use with the map
        constexpr void set(std::string_view name, DeclId id) {
            items[std::string{name}] = id;
        }

        constexpr auto get(std::string const& name) const -> DeclId {
            if (auto it = items.find(name); it != end(items)) return it->second;
            return DeclId::invalid();
        }

        constexpr auto get(std::string_view name) const -> DeclId {
            if (auto it = items.find(std::string{name}); it != end(items))
                return it->second;
            return DeclId::invalid();
        }

        std::string                             scope_name;
        std::unordered_map<std::string, DeclId> items;
        uint32_t                                id;
    };

    // ========================================================================

    void open_file(Node const* source_file) { push_env_of(source_file); }

    void close_file(Node const* source_file) { pop_env_into(source_file); }

    // ========================================================================

    void visit_before_module(Node& /*node*/,
                             conv::Module const& data) override {
        push_env(std::string{data.name});
    }

    void visit_after_module(Node& /*node*/,
                            conv::Module const& /*data*/) override {
        pop_env();
    }

    // ------------------------------------------------------------------------

    void visit_before_block(Node& /*node*/,
                            conv::Block const& /*data*/) override {
        push_env();
    }

    void visit_after_block(Node& /*node*/,
                           conv::Block const& /*data*/) override {
        pop_env();
    }

    // ------------------------------------------------------------------------

    void visit_func_decl(Node& node, conv::FuncDecl const& data) override {
        visit_before_func_decl(node, data);

        visit_decorators(*data.decorators, data.get_decorators());

        if (data.get_name().ids.size() == 1) {
            auto name = data.get_name().ids[0]->get_data_str();
            auto flags = DeclFlags::builder();
            if (is_file_private_decl(data.get_decorators()))
                flags = flags.set_private_file();

            define_at_top(name, &node, flags.build());
        } else {
            // this is namespaced, check that what we namespace under exists
            auto name = data.get_name().ids[0]->get_data_str();
            auto decl_id = lookup_name(name);
            if (!decl_id.is_valid()) {
                er->report_error(node.get_loc(), "undefined identifier {:?}",
                                 name);
            } else {
                // TODO: store the namespace in the func decl
            }
        }

        for (auto const& id : data.get_name().ids) {
            push_env(id->get_data_str());
        }

        visit_func_params(*data.gargs, data.get_gargs());
        visit_func_params(*data.args, data.get_args());
        visit_func_ret_pack(*data.ret, data.get_ret());
        visit(data.body);

        visit_after_func_decl(node, data);
    }

    void visit_after_func_decl(Node& /*node*/,
                               conv::FuncDecl const& data) override {
        pop_env(data.get_name().ids.size());
    }

    void visit_before_top_var_decl(Node& /*node*/,
                                   conv::TopVarDecl const& data) override {
        next_decl_is_file_private = is_file_private_decl(data.get_decorators());
    }

    void visit_var_decl(Node& node, conv::VarDecl const& data) override {
        visit(data.types);
        visit(data.inits);

        auto flags = DeclFlags::builder();
        if (next_decl_is_file_private) {
            next_decl_is_file_private = false;
            flags = flags.set_private_file();
        }

        auto names = conv::id_pack(*data.ids);
        for (auto nid : names.ids) {
            define_at_top(nid->get_data_str(), &node, flags.build());
        }
    }

    void visit_before_top_def_decl(Node& /*node*/,
                                   conv::TopDefDecl const& data) override {
        next_decl_is_file_private = is_file_private_decl(data.get_decorators());
    }

    void visit_def_decl(Node& node, conv::DefDecl const& data) override {
        visit(data.types);
        visit(data.inits);

        auto flags = DeclFlags::builder();
        if (next_decl_is_file_private) {
            next_decl_is_file_private = false;
            flags = flags.set_private_file();
        }

        auto names = conv::id_pack(*data.ids);
        for (auto nid : names.ids) {
            define_at_top(nid->get_data_str(), &node, flags.build());
        }
    }

    void visit_after_func_param(Node&                  node,
                                conv::FuncParam const& data) override {
        define_at_top(data.name, &node);
    }

    void visit_named_ret(Node& node, conv::NamedRet const& data) override {
        define_at_top(data.name, &node);
    }

    // ------------------------------------------------------------------------

    void visit_id(Node& node, conv::Id const& id) override {
        auto decl_id = lookup_name(std::string{id.name});
        if (!decl_id.is_valid()) {
            er->report_error(node.get_loc(), "undefined identifier {:?}",
                             id.name);
            return;
        }

        node.set_declid(decl_id);

        // auto const& decl = ast->get_decl_store()->get_decl(decl_id);
        // if (decl.node != nullptr) {
        //     er->report_note(node.get_loc(),
        //                     "found decl for id {:?}: {}: local_name={}, "
        //                     "name={}, node = {} ",
        //                     id.name, decl_id, decl.local_name, decl.name,
        //                     *decl.node);
        //     er->report_debug(decl.node->get_loc(), "defined here");
        // }
    }

    // ========================================================================

    auto is_file_private_decl(conv::Decorators decorators) const -> bool {
        return conv::decorators_private_kind(decorators) ==
               conv::PrivateKind::File;
    }

    // ========================================================================

    [[nodiscard]] constexpr auto lookup_name(std::string_view name) const
        -> DeclId {
        for (auto const& env : envs | std::ranges::views::reverse) {
            if (auto id = env.get(name); id.is_valid()) return id;
        }

        return DeclId::invalid();
    }

    auto gen_name(std::string_view local_name) -> std::string {
        namespace rv = std::ranges::views;

        auto s = fmt::to_string(
            fmt::join(envs | rv::filter([](Env const& env) {
                          return !env.scope_name.empty();
                      }) | rv::transform([](Env const& env) {
                          return static_cast<std::string_view>(env.scope_name);
                      }),
                      "."));
        if (s.empty()) return std::string{local_name};
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

    constexpr auto define_at_top(std::string_view name, Node* node,
                                 DeclFlags flags = {}) -> DeclId {
        return define_at(*get_top_env(), name, node, flags);
    }

    constexpr auto define_at_top(std::string_view name, Node* node, Value type)
        -> DeclId {
        return define_at(*get_top_env(), name, node, type);
    }

    constexpr auto define_at(Env& env, std::string_view name, Node* node)
        -> DeclId {
        return define_at(env, name, node, {}, {});
    }

    constexpr auto define_at(Env& env, std::string_view name, Node* node,
                             DeclFlags flags) -> DeclId {
        return define_at(env, name, node, {}, flags);
    }

    constexpr auto define_at(Env& env, std::string_view name, Node* node,
                             Value value) -> DeclId {
        return define_at(env, name, node, value, {});
    }

    constexpr auto define_at(Env& env, std::string_view name, Node* node,
                             Value value, DeclFlags flags) -> DeclId {
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
                er->report_error(node->get_loc(),
                                 "redefinition of identifier '{}'", name);
                er->report_note(get_ds().get_decl(prev).node->get_loc(),
                                "previous value defined here");
                return DeclId::invalid();
            }

            // check if the name is already defined in the module
            if (auto prev = get_mod_env()->get(name); prev.is_valid()) {
                er->report_error(node->get_loc(),
                                 "redefinition of identifier '{}'", name);
                er->report_note(get_ds().get_decl(prev).node->get_loc(),
                                "previous value defined here");
                return DeclId::invalid();
            }
        }

        auto decl = get_ds().gen_decl({
            .name = gen_name(name),
            .local_name = std::string{name},
            .node = node,
            .flags = flags,
            .value = value,
        });

        env.set(name, decl);
        if (node != nullptr) {
            node->set_declid(decl);
            // er->report_warn(node->get_loc(), "set declid of this to {}",
            // decl);
        }

        return decl;
    }

    constexpr void push_env(std::string_view name = "") {
        envs.push_back({.scope_name = std::string{name},
                        .items = {},
                        .id = next_env_id++});
    }

    constexpr void push_env_of(Node const* source_id) {
        push_env();
        auto top = get_top_env();
        for (auto const& [k, v] : file_envs[source_id].items) {
            top->set(k, v);
        }
    }

    constexpr void pop_env_into(Node const* source_id) {
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
    std::unordered_map<Node const*, Env> file_envs;
    std::vector<Env>                     envs;

    ErrorReporter*    er;
    FileStore*        fs;
    types::TypeStore* ts;
};

// ============================================================================

auto make_map_of_decl_to_source_file(Node* mod)
    -> std::unordered_map<Node const*, Node const*> {
    std::unordered_map<Node const*, Node const*> decl_to_source_file;

    for (auto source_file : conv::module(*mod).children) {
        // sort children based on order
        auto children = conv::source_file(*source_file).children;
        for (auto child : children) {
            decl_to_source_file[child] = source_file;
        }
    }

    return decl_to_source_file;
}

// ============================================================================

auto resolve_names(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
                   FileStore& fs, types::TypeStore& ts, Options const& opt)
    -> ast::Node* {
    auto mod_decl = conv::source_file(*root).mod;
    auto mod = parse_all_files_into_module(ast, *root, *mod_decl, fs, er, opt);

    auto order = sort::topo_sort_top_decls(ast, mod, er);
    // for (auto const& n : order) {
    //     auto node = ast.get_node(n.as_ref());
    //     er.report_debug(node.get_loc(), "should run {}", n);
    // }

    auto decls_to_source_files = make_map_of_decl_to_source_file(mod);

    auto ns = NameSolver{fs, ts, er, ast};

    ns.visit_before_module(*mod, conv::module(*mod));

    for (auto ordered : order) {
        auto source_file = decls_to_source_files[ordered];
        ns.open_file(source_file);
        ns.visit(ordered);
        ns.close_file(source_file);
    }

    ns.visit_after_module(*mod, conv::module(*mod));

    return mod;
}

}  // namespace yal
