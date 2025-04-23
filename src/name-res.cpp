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

#include "arena.hpp"
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

namespace rv = std::ranges::views;

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
    if (!mod_found || mod_found->is_err()) {
        er.report_error(mod_found->get_loc(), "invalid module found");
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
        // in case the module is an error, return an empty module
        if (mod_decl.is_err()) {
            return ast.new_module(mod_decl.get_loc(), "<error>", {});
        }

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
        if (src_file) files.push_back(src_file);
    }

    auto mod_name = conv::module_decl(mod_decl).name;
    return ast.new_module(start_node.get_loc(), mod_name, files);
}

// ============================================================================

struct Env {
    constexpr auto with_decl(Node* decl) const -> Env {
        // fmt::println(stderr, "with_decl({})", fmt::ptr(decl));

        return {
            .items = {},
            .name = name,
            .current_decl = decl,
            .parent = this,
        };
    }

    constexpr auto with_name(std::string name) const -> Env {
        // fmt::println(stderr, "with_name({:?})", name);

        return {
            .items = {},
            .name = name,
            .parent = this,
        };
    }

    constexpr auto unnamed() const -> Env {
        // fmt::println(stderr, "unnamed()");

        return {
            .items = {},
            .name = "",
            .parent = this,
        };
    }

    constexpr auto unnamed_lvalue() const -> Env {
        // fmt::println(stderr, "unnamed_lvalue()");

        return {
            .items = {},
            .name = "",
            .parent = this,
            .is_lvalue = true,
        };
    }

    constexpr void update(ErrorReporter& er, Env const& env) {
        for (auto const& [k, v] : env.items) {
            if (auto it = items.find(k); it != end(items)) {
                ASSERT(v->node != nullptr);
                er.report_error(v->node->get_loc(),
                                "redefinition of identifier: {:?}", k);
                if (it->second->node) {
                    er.report_note(it->second->node->get_loc(),
                                   "previously value defined here");
                }
            } else {
                items[k] = v;
            }
        }
    }

    constexpr void update_preserve_private(ErrorReporter& er, Env const& env) {
        for (auto const& [k, v] : env.items) {
            if (!v->is_private_file()) {
                if (auto it = items.find(k);
                    it != end(items) && it->second->node != v->node) {
                    ASSERT(v->node != nullptr);
                    er.report_error(v->node->get_loc(),
                                    "redefinition of identifier: {:?}", k);
                    if (it->second->node) {
                        er.report_note(it->second->node->get_loc(),
                                       "previously value defined here");
                    }
                } else {
                    items[k] = v;
                }
            }
        }
    }

    constexpr auto define(DeclStore& ds, std::string_view name, Node* node,
                          Value value, DeclFlags flags) -> Decl* {
        return define(ds, name, gen_link_name(name), node, value, flags);
    }

    constexpr auto define(DeclStore& ds, std::string_view name,
                          std::string_view link_name, Node* node, Value value,
                          DeclFlags flags) -> Decl* {
        auto d = ds.new_decl(gen_full_name(name), name, link_name, node, flags,
                             value);

        items[name] = d;
        return d;
    }

    constexpr auto define_ghost(DeclStore& ds, std::string_view name,
                                std::string_view link_name, Node* node,
                                Value value, DeclFlags flags) const -> Decl* {
        return ds.new_decl(gen_full_name(name), name, link_name, node, flags,
                           value);
    }

    constexpr auto define_ghost(DeclStore& ds, std::string_view name,
                                Node* node, Value value, DeclFlags flags) const
        -> Decl* {
        return define_ghost(ds, name, gen_link_name(name), node, value, flags);
    }

    constexpr auto lookup(std::string_view name) const -> Decl* {
        if (auto it = items.find(name); it != end(items)) return it->second;
        return parent ? parent->lookup(name) : nullptr;
    }

    constexpr auto gen_full_name(std::string_view name) const -> std::string {
        std::vector<std::string_view> names{name};
        for (auto it = this; it != nullptr; it = it->parent) {
            if (!it->name.empty()) names.push_back(it->name);
        }

        return fmt::to_string(fmt::join(names | rv::reverse, "."));
    }

    constexpr auto gen_link_name(std::string_view name) const -> std::string {
        std::vector<std::string_view> names{name};
        for (auto it = this; it != nullptr; it = it->parent) {
            if (!it->name.empty()) names.push_back(it->name);
        }

        return fmt::to_string(fmt::join(names | rv::reverse, "_"));
    }

    std::unordered_map<std::string_view, Decl*> items;

    std::string name;
    Node*       current_decl{};
    Env const*  parent{};

    bool is_lvalue{};
};

struct Context {
    constexpr auto for_file(Node const* node, Env* parent) -> Env& {
        if (auto it = file_envs.find(node); it != end(file_envs))
            return it->second;

        auto env = parent ? parent->unnamed() : Env{};
        file_envs[node] = env;
        return file_envs.at(node);
    }

    ErrorReporter* er;
    FileId         current_file;

    std::unordered_map<Node const*, Env> file_envs;
};

auto flags_from_decorators(conv::Decorators const& dec) -> DeclFlags {
    auto flags = DeclFlags::builder();
    auto priv = conv::decorators_private_kind(dec);
    if (priv == ast::conv::PrivateKind::Module)
        flags = flags.set_private();
    else if (priv == ast::conv::PrivateKind::File)
        flags = flags.set_private_file();

    return flags.build();
};

void visit_node(Ast& ast, Node* node, Context& ctx, Env& env);

auto envs_for_func_decl(conv::IdPack const& name, Env const& parent)
    -> std::vector<Env> {
    std::vector<Env> envs;
    envs.resize(name.ids.size());

    // push the first env
    envs[0] = parent.with_name(std::string{conv::id(*name.ids[0]).name});

    // and then one for each id
    for (auto const& [idx, id] : name.ids | rv::enumerate | rv::drop(1)) {
        envs[idx] = envs[idx - 1].with_name(std::string{conv::id(*id).name});
    }

    return envs;
}

void visit_func_decl(Ast& ast, Node* node, Context& ctx, Env& env) {
    auto visit = [&](Context& ctx, Env& env, Node* node) {
        visit_node(ast, node, ctx, env);
    };

    auto& ds = *ast.get_decl_store();
    auto  data = conv::func_decl(*node);
    auto  senv = env.unnamed();

    visit(ctx, senv, data.decorators);

    auto flags = flags_from_decorators(data.get_decorators());

    auto name = data.get_name();
    auto id = conv::id(*name.ids[0]);
    auto envs = envs_for_func_decl(name, senv);

    // non-namespaced function
    // We want to define it
    if (name.ids.size() == 1) {
        node->set_decl(env.define(ds, id.name, node, {}, flags));
    }

    // namespaced function
    // We want to lookup the namespacing identifier. The found decl is set
    // to the `id` node that is namespacing us. A "ghost" decl is created
    // for the namespaced function.
    else {
        auto decl = env.lookup(id.name);
        if (decl != nullptr) {
            name.ids[0]->set_decl(decl);

            // want to use the one before the top, to not include itself in the
            // namespacing and double the name
            auto n = conv::id(*name.ids[name.ids.size() - 1]);
            auto d = envs.at(envs.size() - 2)
                         .define_ghost(ds, n.name, node, {}, flags);
            node->set_decl(d);
        } else {
            ctx.er->report_error(name.ids[0]->get_loc(),
                                 "undefined identifier: {:?}", id.name);
        }
    }

    auto& top_env = envs.at(envs.size() - 1);
    visit(ctx, top_env, data.gargs);
    visit(ctx, top_env, data.args);
    visit(ctx, top_env, data.ret);
    visit(ctx, top_env, data.body);
}

void visit_def_decl(Ast& ast, Node* node, Context& ctx, Env& env) {
    auto visit = [&](Context& ctx, Env& env, Node* node) {
        visit_node(ast, node, ctx, env);
    };

    auto& ds = *ast.get_decl_store();
    auto  data = conv::def_decl(*node);

    auto flags = DeclFlags::builder();

    if (env.current_decl &&
        conv::top_def_decl(*env.current_decl).decl == node) {
        auto data = conv::top_def_decl(*env.current_decl);
        auto priv = conv::decorators_private_kind(data.get_decorators());
        if (priv == ast::conv::PrivateKind::Module)
            flags = flags.set_private();
        else if (priv == ast::conv::PrivateKind::File)
            flags = flags.set_private_file();
    }

    // first difine the ids at the current level
    for (auto const& id : data.get_ids().ids) {
        auto name = conv::id(*id);
        if (name.name != "_") {
            auto d = env.define(ds, name.name, node, {}, flags.build());
            id->set_decl(d);
        }
    }

    // then process the initializers and types one level deeper (so that
    // we can namespace all ids under the declared name)

    // in case we have explicit types, then we run them before the init
    if (data.types) {
        for (auto const& [_id, ty, init] :
             rv::zip(data.get_ids().ids, data.get_types().items,
                     data.get_inits().items)) {
            auto id = conv::id(*_id);
            auto senv = env.with_name(std::string{id.name});

            visit(ctx, senv, ty);
            visit(ctx, senv, init);
        }
    }

    // no explicit types, just the initializers
    // NOTE: parse errors may leave us with no initializers
    else if (data.inits) {
        for (auto const& [_id, init] :
             rv::zip(data.get_ids().ids, data.get_inits().items)) {
            auto id = conv::id(*_id);
            auto senv = env.with_name(std::string{id.name});

            visit(ctx, senv, init);
        }
    }
}

void visit_var_decl(Ast& ast, Node* node, Context& ctx, Env& env) {
    auto visit = [&](Context& ctx, Env& env, Node* node) {
        visit_node(ast, node, ctx, env);
    };

    auto& ds = *ast.get_decl_store();
    auto  data = conv::var_decl(*node);

    auto flags = DeclFlags::builder();

    if (env.current_decl &&
        conv::top_var_decl(*env.current_decl).decl == node) {
        auto data = conv::top_var_decl(*env.current_decl);
        auto priv = conv::decorators_private_kind(data.get_decorators());

        if (priv == ast::conv::PrivateKind::Module)
            flags = flags.set_private();
        else if (priv == ast::conv::PrivateKind::File)
            flags = flags.set_private_file();
    }

    // first process the initializers and types one level deeper (so that
    // we can namespace all ids under the declared name)

    // in case we have explicit types, then we run them before the init
    if (data.types && data.inits) {
        auto ids = data.get_ids().ids;
        auto types = data.get_types().items;
        auto inits = data.get_inits().items;
        auto count = std::max({ids.size(), types.size(), inits.size()});
        for (size_t i = 0; i < count; i++) {
            if (i < ids.size()) {
                auto id = conv::id(*ids[i]);
                auto senv = env.with_name(std::string{id.name});
                if (i < types.size()) visit(ctx, senv, types[i]);
                if (i < inits.size()) visit(ctx, senv, inits[i]);
            } else {
                auto senv = env.with_name("<error>");
                if (i < types.size()) visit(ctx, senv, types[i]);
                if (i < inits.size()) visit(ctx, senv, inits[i]);
            }
        }
    }

    // just explicit types, no initializers
    else if (data.types) {
        auto ids = data.get_ids().ids;
        auto types = data.get_types().items;
        auto count = std::max({ids.size(), types.size()});
        for (size_t i = 0; i < count; i++) {
            if (i < ids.size()) {
                auto id = conv::id(*ids[i]);
                auto senv = env.with_name(std::string{id.name});
                if (i < types.size()) visit(ctx, senv, types[i]);
            } else {
                auto senv = env.with_name("<error>");
                if (i < types.size()) visit(ctx, senv, types[i]);
            }
        }
    }

    // no explicit types, just the initializers
    else if (data.inits) {
        auto ids = data.get_ids().ids;
        auto inits = data.get_inits().items;
        auto count = std::max({ids.size(), inits.size()});
        for (size_t i = 0; i < count; i++) {
            if (i < ids.size()) {
                auto id = conv::id(*ids[i]);
                auto senv = env.with_name(std::string{id.name});
                if (i < inits.size()) visit(ctx, senv, inits[i]);
            } else {
                auto senv = env.with_name("<error>");
                if (i < inits.size()) visit(ctx, senv, inits[i]);
            }
        }
    }

    // then difine the ids at the current level
    for (auto const& id : data.get_ids().ids) {
        auto name = conv::id(*id);
        if (name.name != "_") {
            auto d = env.define(ds, name.name, node, {}, flags.build());
            id->set_decl(d);
        }
    }
}

void visit_node(Ast& ast, Node* node, Context& ctx, Env& env) {
    if (node == nullptr) return;

    auto visit = [&](Context& ctx, Env& env, Node* node) {
        visit_node(ast, node, ctx, env);
    };

    auto visit_children = [&](Context& ctx, Env& env, Node* node) {
        ast::visit_children(
            ast, node,
            [](Ast& ast, Node* node, auto&&, Context& ctx, Env& env) {
                visit_node(ast, node, ctx, env);
            },
            ctx, env);
    };

    auto& ds = *ast.get_decl_store();

    if (node->is_oneof(ast::NodeKind::Module, ast::NodeKind::SourceFile)) {
        PANIC("found invalid node in `visit_node`", node->get_kind());
    }

    if (node->is_oneof(ast::NodeKind::ImportStmt)) {
        ctx.er->report_bug(node->get_loc(),
                           "import statements have not been implemented");
        return;
    }

    if (node->is_oneof(ast::NodeKind::FuncDecl,
                       ast::NodeKind::FuncDeclWithCVarArgs)) {
        visit_func_decl(ast, node, ctx, env);
        return;
    }

    if (node->is_oneof(ast::NodeKind::FuncParam)) {
        auto data = conv::func_param(*node);

        auto d = env.define(ds, data.name, node, {}, {});
        node->set_decl(d);

        visit(ctx, env, data.type);
        return;
    }

    if (node->is_oneof(ast::NodeKind::NamedRet)) {
        auto data = conv::named_ret(*node);

        auto d = env.define(ds, data.name, node, {}, {});
        node->set_decl(d);

        visit(ctx, env, data.type);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        auto senv = env.with_decl(node);
        visit_children(ctx, senv, node);
        env.update(*ctx.er, senv);

        return;
    }

    if (node->is_oneof(ast::NodeKind::TopVarDecl)) {
        auto senv = env.with_decl(node);
        visit_children(ctx, senv, node);
        env.update(*ctx.er, senv);

        return;
    }

    if (node->is_oneof(ast::NodeKind::DefDecl)) {
        visit_def_decl(ast, node, ctx, env);
        return;
    }

    if (node->is_oneof(ast::NodeKind::VarDecl)) {
        visit_var_decl(ast, node, ctx, env);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Block)) {
        auto senv = env.unnamed();
        visit_children(ctx, senv, node);

        return;
    }

    // if (node->is_oneof(ast::NodeKind::StructType)) {
    //     auto senv = env.with_name();
    //     visit_children(ctx, senv, node);
    //
    //     return;
    // }

    if (node->is_oneof(ast::NodeKind::StructField)) {
        auto data = conv::struct_field(*node);

        auto d = env.define_ghost(ds, data.name, node, {}, {});
        node->set_decl(d);

        visit_children(ctx, env, node);

        return;
    }

    if (node->is_oneof(ast::NodeKind::Assign, ast::NodeKind::AssignAdd,
                       ast::NodeKind::AssignSub, ast::NodeKind::AssignMul,
                       ast::NodeKind::AssignDiv, ast::NodeKind::AssignMod,
                       ast::NodeKind::AssignShiftLeft,
                       ast::NodeKind::AssignShiftRight,
                       ast::NodeKind::AssignBand, ast::NodeKind::AssignBxor,
                       ast::NodeKind::AssignBor)) {
        auto data = conv::assign(*node);

        auto lenv = env.unnamed_lvalue();
        visit(ctx, lenv, data.lhs);
        visit(ctx, env, data.rhs);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        if (env.is_lvalue && data.name == "_") return;

        auto decl = env.lookup(data.name);
        if (decl == nullptr) {
            ctx.er->report_error(node->get_loc(), "undefined identifier: {:?}",
                                 data.name);
            return;
        }

        node->set_decl(decl);
        // ctx.er->report_debug(node->get_loc(), "got decl: {}",
        // decl->full_name); if (decl->node) {
        //     ctx.er->report_debug(decl->node->get_loc(), "defined here:");
        // }

        return;
    }

    // ctx.er->report_debug(node->get_loc(), "visit_node({}, {:?})",
    //                      node->get_kind(), env.gen_full_name(""));

    visit_children(ctx, env, node);
}

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
    //     er.report_debug(n->get_loc(), "should run {}", n->get_kind());
    // }

    auto root_env = Env{};

    {
        auto mkty = [&](types::Type* ty) -> Value {
            return {.type = ts.get_type(), .data = ty};
        };

        auto& ds = *ast.get_decl_store();
        root_env.define(ds, "type", nullptr, mkty(ts.get_type()), {});
        root_env.define(ds, "void", nullptr, mkty(ts.get_void()), {});

        root_env.define(ds, "i64", nullptr, mkty(ts.get_i64()), {});
        root_env.define(ds, "u64", nullptr, mkty(ts.get_u64()), {});
        root_env.define(ds, "i32", nullptr, mkty(ts.get_i32()), {});
        root_env.define(ds, "u32", nullptr, mkty(ts.get_u32()), {});
        root_env.define(ds, "i16", nullptr, mkty(ts.get_i16()), {});
        root_env.define(ds, "u16", nullptr, mkty(ts.get_u16()), {});
        root_env.define(ds, "i8", nullptr, mkty(ts.get_i8()), {});
        root_env.define(ds, "u8", nullptr, mkty(ts.get_u8()), {});

        root_env.define(ds, "isize", nullptr, mkty(ts.get_isize()), {});
        root_env.define(ds, "usize", nullptr, mkty(ts.get_usize()), {});

        root_env.define(ds, "bool", nullptr, mkty(ts.get_bool()), {});

        root_env.define(ds, "f64", nullptr, mkty(ts.get_f64()), {});
        root_env.define(ds, "f32", nullptr, mkty(ts.get_f32()), {});

        root_env.define(ds, "string_view", nullptr, mkty(ts.get_strview()), {});

        root_env.define(ds, "true", nullptr,
                        {.type = ts.get_bool(), .data = true}, {});
        root_env.define(ds, "false", nullptr,
                        {.type = ts.get_bool(), .data = false}, {});

        root_env.define(ds, "nil", nullptr, {.type = ts.get_nil(), .data = {}},
                        {});
    }

    auto m = conv::module(*mod);
    auto top_env = root_env.with_name(std::string{m.name});

    auto ctx = Context{
        .er = &er,
        .current_file = FileId::invalid(),
        .file_envs = {},
    };

    auto decls_to_source_files = make_map_of_decl_to_source_file(mod);

    for (auto ordered : order) {
        auto  source_file = decls_to_source_files[ordered];
        auto& file_env = ctx.for_file(source_file, &top_env);

        ctx.current_file = source_file->get_loc().fileid;
        visit_node(ast, ordered, ctx, file_env);

        top_env.update_preserve_private(er, file_env);
    }

    // auto ns = NameSolver{fs, ts, er, ast};

    // ns.visit_before_module(*mod, conv::module(*mod));
    //
    // for (auto ordered : order) {
    //     auto source_file = decls_to_source_files[ordered];
    //     ns.open_file(source_file);
    //     ns.visit(ordered);
    //     ns.close_file(source_file);
    // }
    //
    // ns.visit_after_module(*mod, conv::module(*mod));

    return mod;
}

}  // namespace yal
