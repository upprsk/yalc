#pragma once

#include <cstdint>
#include <string_view>

#include "arena.hpp"
#include "location.hpp"
#include "node.hpp"

namespace yal::ast {

class File {
    mem::Arena node_arena;
    mem::Arena strings_arena;

    std::string_view   module_name;
    Location           module_name_loc;
    std::vector<Decl*> declarations;

public:
    [[nodiscard]] constexpr auto get_module_name() const -> std::string_view {
        return module_name;
    }

    [[nodiscard]] constexpr auto get_module_name_loc() const -> Location {
        return module_name_loc;
    }

    constexpr void set_module_name(std::string_view name, Location name_loc) {
        module_name = strings_arena.alloc_string_view(name);
        module_name_loc = name_loc;
    }

    constexpr void set_module_name_loc(Location loc) { module_name_loc = loc; }

    constexpr void set_declarations(auto&& d) { declarations = d; }
    constexpr void append_declarations(auto&& d) {
        declarations.append_range(d);
    }

    [[nodiscard]] constexpr auto get_declarations() const
        -> std::vector<Decl*> const& {
        return declarations;
    }

    // Expressions
    // ----------

    auto expr_err(Location loc) -> ErrExpr* {
        return node_arena.create<ErrExpr>(
            Expr{.kind = ExprKind::Err, .loc = loc});
    }

    auto expr_neg(Location loc, Expr* child) -> ArithExpr* {
        return expr_arith(loc, ExprKind::Neg, child, nullptr);
    }

    auto expr_arith(Location loc, ExprKind kind, Expr* lhs, Expr* rhs)
        -> ArithExpr* {
        return node_arena.create<ArithExpr>(Expr{.kind = kind, .loc = loc}, lhs,
                                            rhs);
    }

    auto expr_id(Location loc, std::string_view name) -> IdExpr* {
        return node_arena.create<IdExpr>(Expr{.kind = ExprKind::Id, .loc = loc},
                                         strings_arena.alloc_string_view(name));
    }

    auto expr_kw(Location loc, std::string_view name) -> KwExpr* {
        return node_arena.create<KwExpr>(Expr{.kind = ExprKind::Kw, .loc = loc},
                                         strings_arena.alloc_string_view(name));
    }

    auto expr_int(Location loc, uint64_t value) -> IntExpr* {
        return node_arena.create<IntExpr>(
            Expr{.kind = ExprKind::Int, .loc = loc}, value);
    }

    auto expr_string(Location loc, std::string_view value) -> StringExpr* {
        return node_arena.create<StringExpr>(
            Expr{.kind = ExprKind::String, .loc = loc},
            strings_arena.alloc_string_view(value));
    }

    // Statements
    // ----------

    auto stmt_err(Location loc) -> ErrStmt* {
        return node_arena.create<ErrStmt>(
            Stmt{.kind = StmtKind::Err, .loc = loc});
    }

    auto stmt_block(Location loc, std::span<Stmt* const> children)
        -> BlockStmt* {
        return node_arena.create<BlockStmt>(
            Stmt{.kind = StmtKind::Block, .loc = loc},
            node_arena.alloc<Stmt*>(children));
    }

    auto stmt_return(Location loc, std::span<Expr* const> children)
        -> ReturnStmt* {
        return node_arena.create<ReturnStmt>(
            Stmt{.kind = StmtKind::Return, .loc = loc}, dupe_exprs(children));
    }

    auto stmt_expr(Location loc, Expr* child) -> ExprStmt* {
        return node_arena.create<ExprStmt>(
            Stmt{.kind = StmtKind::Expr, .loc = loc}, child);
    }

    auto stmt_var(Location loc, Location name_loc, std::string_view name,
                  Expr* type, Expr* init) -> VarStmt* {
        return node_arena.create<VarStmt>(
            Stmt{.kind = StmtKind::Var, .loc = loc}, name, name_loc, type,
            init);
    }

    auto stmt_multi_var(Location loc, std::span<MultiVarName const> names,
                        std::span<Expr* const> types,
                        std::span<Expr* const> inits) -> MultiVarStmt* {
        auto anames = alloc_multi_var_names(names);

        return node_arena.create<MultiVarStmt>(
            Stmt{.kind = StmtKind::MultiVar, .loc = loc}, anames,
            dupe_exprs(types), dupe_exprs(inits));
    }

    auto stmt_def(Location loc, Location name_loc, std::string_view name,
                  Expr* type, Expr* init) -> VarStmt* {
        return node_arena.create<VarStmt>(
            Stmt{.kind = StmtKind::Def, .loc = loc}, name, name_loc, type,
            init);
    }

    auto stmt_multi_def(Location loc, std::span<MultiVarName const> names,
                        std::span<Expr* const> types,
                        std::span<Expr* const> inits) -> MultiVarStmt* {
        auto anames = alloc_multi_var_names(names);

        return node_arena.create<MultiVarStmt>(
            Stmt{.kind = StmtKind::MultiDef, .loc = loc}, anames,
            dupe_exprs(types), dupe_exprs(inits));
    }

    // Declarations
    // ------------

    auto decl_err(Location loc) -> ErrDecl* {
        return node_arena.create<ErrDecl>(
            Decl{.kind = DeclKind::Err, .loc = loc});
    }

    // NOTE: attributes should already have been allocated (with
    // `alloc_decl_attributes`).
    // NOTE: params should already have been allocated (with
    // `alloc_func_params`).
    auto decl_func(Location loc, Location name_loc, std::string_view name,
                   std::string_view         attached_type,
                   std::span<DeclAttribute> attributes,
                   std::span<FuncParam> params, std::span<FuncRet const> rets,
                   Stmt* body, bool is_c_varargs) -> FuncDecl* {
        auto arets = node_arena.alloc<FuncRet>(rets);
        for (auto& r : arets) {
            r.name = strings_arena.alloc_string_view(r.name);
        }

        return node_arena.create<FuncDecl>(
            Decl{.kind = DeclKind::Func, .loc = loc},
            strings_arena.alloc_string_view(name),
            strings_arena.alloc_string_view(attached_type), attributes, params,
            arets, body, nullptr, name_loc, is_c_varargs);
    }

    // NOTE: attributes should already have been allocated (with
    // `alloc_decl_attributes`).
    auto decl_var(Location loc, Location name_loc, std::string_view name,
                  std::span<DeclAttribute> attributes, Expr* type_expr,
                  Expr* init) -> VarDecl* {
        return node_arena.create<VarDecl>(
            Decl{.kind = DeclKind::Var, .loc = loc},
            strings_arena.alloc_string_view(name), name_loc, attributes,
            type_expr, init);
    }

    // NOTE: attributes should already have been allocated (with
    // `alloc_decl_attributes`).
    auto decl_def(Location loc, Location name_loc, std::string_view name,
                  std::span<DeclAttribute> attributes, Expr* type_expr,
                  Expr* init) -> VarDecl* {
        return node_arena.create<VarDecl>(
            Decl{.kind = DeclKind::Def, .loc = loc},
            strings_arena.alloc_string_view(name), name_loc, attributes,
            type_expr, init);
    }

    // NOTE: attributes should already have been allocated (with
    // `alloc_decl_attributes`).
    // NOTE: names, types and inits will be duped, should not be preallocated.
    auto decl_multi_var(Location loc, std::span<DeclAttribute> attributes,
                        std::span<MultiVarName const> names,
                        std::span<Expr* const>        types,
                        std::span<Expr* const>        inits) -> MultiVarDecl* {
        auto anames = alloc_multi_var_names(names);

        return node_arena.create<MultiVarDecl>(
            Decl{.kind = DeclKind::MultiVar, .loc = loc}, attributes, anames,
            dupe_exprs(types), dupe_exprs(inits));
    }

    // NOTE: attributes should already have been allocated (with
    // `alloc_decl_attributes`).
    // NOTE: names, types and inits will be duped, should not be preallocated.
    auto decl_multi_def(Location loc, std::span<DeclAttribute> attributes,
                        std::span<MultiVarName const> names,
                        std::span<Expr* const>        types,
                        std::span<Expr* const>        inits) -> MultiVarDecl* {
        auto anames = alloc_multi_var_names(names);

        return node_arena.create<MultiVarDecl>(
            Decl{.kind = DeclKind::MultiDef, .loc = loc}, attributes, anames,
            dupe_exprs(types), dupe_exprs(inits));
    }

    // NOTE: the inner args and kwargs arrays are assumed to already be
    // allocated correctly. Only the strings of the attributes themselves will
    // be duped, not args and kwargs.
    auto alloc_decl_attributes(std::span<DeclAttribute const> attributes)
        -> std::span<DeclAttribute> {
        auto aattributes = node_arena.alloc<DeclAttribute>(attributes);
        for (auto& a : aattributes) {
            // make sure that we own the strings and slices!
            a.qualified_name =
                strings_arena.alloc_string_view(a.qualified_name);
            a.name = strings_arena.alloc_string_view(a.name);
        }

        return aattributes;
    }

    auto alloc_func_params(std::span<FuncParam const> params)
        -> std::span<FuncParam> {
        auto aparams = node_arena.alloc<FuncParam>(params);
        for (auto& p : aparams) {
            p.name = strings_arena.alloc_string_view(p.name);
        }

        return aparams;
    }

    // NOTE: does not dupe internal data
    auto dupe_exprs(std::span<Expr* const> exprs) -> std::span<Expr*> {
        return node_arena.alloc<Expr*>(exprs);
    }

    // NOTE: does also dupe the internal string references
    auto dupe_attribute_kvs(std::span<DeclAttributeKV const> items)
        -> std::span<DeclAttributeKV> {
        auto kvs = node_arena.alloc<DeclAttributeKV>(items);
        for (auto& kv : kvs) {
            kv.name = strings_arena.alloc_string_view(kv.name);
        }

        return kvs;
    }

private:
    auto alloc_multi_var_names(std::span<MultiVarName const> names)
        -> std::span<MultiVarName> {
        auto anames = node_arena.alloc<MultiVarName>(names);
        for (auto& n : anames) {
            // make sure that we own the strings!
            n.name = strings_arena.alloc_string_view(n.name);
        }

        return anames;
    }
};

/// Just the `module <name>;` part of a file.
struct ModuleDecl {
    std::string name;
    Location    name_loc;

    FileId file;
};

/// An entire module, maybe composed of many files.
struct Module {
    std::string       name;
    std::vector<File> files;
};

void to_json(nlohmann::json& j, File const& n);
void to_json(nlohmann::json& j, Module const& n);

}  // namespace yal::ast
