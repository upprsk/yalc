#pragma once

#include <array>
#include <cstdint>
#include <libassert/assert.hpp>
#include <span>
#include <string_view>

#include "location.hpp"
#include "macros.hpp"
#include "symbol.hpp"

namespace yal::ast {

/// All of the variants of expressions.
enum struct ExprKind : uint8_t {
    Err,

    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Field,

    Id,
    Kw,
    Int,
    String,
};

struct ArithExpr;
struct FieldExpr;
struct IdExpr;
struct KwExpr;
struct IntExpr;
struct StringExpr;

/// Base class/interface for expressions. All expressions inherit from this,
/// adding specific fields. There should not be any virtual methods.
///
/// Expressions support the union-find data-structure to allow non-destructive
/// rewriting of the AST. This is done by the forward field and the find and
/// make_equal_to methods.
///
/// > The union-find structure does not allow cycles, so nodes should be cloned
/// > if needed in multiple places.
struct Expr {
    ExprKind kind;
    Location loc;

    Expr* forward = nullptr;

    [[nodiscard]] auto as_arith() const -> ArithExpr const&;
    [[nodiscard]] auto as_field() const -> FieldExpr const&;
    [[nodiscard]] auto as_id() const -> IdExpr const&;
    [[nodiscard]] auto as_kw() const -> KwExpr const&;
    [[nodiscard]] auto as_int() const -> IntExpr const&;
    [[nodiscard]] auto as_string() const -> StringExpr const&;

    [[nodiscard]] auto as_arith() -> ArithExpr&;
    [[nodiscard]] auto as_field() -> FieldExpr&;
    [[nodiscard]] auto as_id() -> IdExpr&;
    [[nodiscard]] auto as_kw() -> KwExpr&;
    [[nodiscard]] auto as_int() -> IntExpr&;
    [[nodiscard]] auto as_string() -> StringExpr&;

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() const -> Expr const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() -> Expr* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The union operation of union find.
    constexpr void make_equal_to(Expr* other) {
        auto n = find();
        if (n != other) n->forward = other;
    }
};

struct ErrExpr : Expr {};

struct FieldExpr : Expr {
    Expr*            obj;
    std::string_view name;
};

struct ArithExpr : public Expr {
    Expr* lhs = nullptr;
    Expr* rhs = nullptr;
};

struct IdExpr : public Expr {
    std::string_view value;

    Symbol* sym = nullptr;
};

struct KwExpr : public Expr {
    std::string_view value;
};

struct IntExpr : public Expr {
    uint64_t value = 0;
};

struct StringExpr : public Expr {
    std::string_view value;
};

// ----------------------------------------------------------------------------

/// All of the variants of statements.
enum struct StmtKind : uint8_t {
    Err,

    Block,
    Return,
    Expr,

    Var,
    Def,
    MultiVar,
    MultiDef,
};

struct BlockStmt;
struct ReturnStmt;
struct ExprStmt;
struct VarStmt;
struct MultiVarStmt;

/// Base class/interface for statements. All statements inherit from this,
/// adding specific fields. There should not be any virtual methods.
///
/// Statements support the union-find data-structure to allow non-destructive
/// rewriting of the AST. This is done by the forward field and the find and
/// make_equal_to methods.
///
/// > The union-find structure does not allow cycles, so nodes should be cloned
/// > if needed in multiple places.
struct Stmt {
    StmtKind kind;
    Location loc;

    Stmt* forward = nullptr;

    [[nodiscard]] auto as_block() const -> BlockStmt const&;
    [[nodiscard]] auto as_return() const -> ReturnStmt const&;
    [[nodiscard]] auto as_expr() const -> ExprStmt const&;
    [[nodiscard]] auto as_var() const -> VarStmt const&;
    [[nodiscard]] auto as_multi_var() const -> MultiVarStmt const&;
    [[nodiscard]] auto as_def() const -> VarStmt const&;
    [[nodiscard]] auto as_multi_def() const -> MultiVarStmt const&;

    [[nodiscard]] auto as_block() -> BlockStmt&;
    [[nodiscard]] auto as_return() -> ReturnStmt&;
    [[nodiscard]] auto as_expr() -> ExprStmt&;
    [[nodiscard]] auto as_var() -> VarStmt&;
    [[nodiscard]] auto as_multi_var() -> MultiVarStmt&;
    [[nodiscard]] auto as_def() -> VarStmt&;
    [[nodiscard]] auto as_multi_def() -> MultiVarStmt&;

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() const -> Stmt const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() -> Stmt* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The union operation of union find.
    constexpr void make_equal_to(Stmt* other) {
        auto n = find();
        if (n != other) n->forward = other;
    }
};

struct ErrStmt : Stmt {};

struct BlockStmt : public Stmt {
    std::span<Stmt*> children;
};

struct ReturnStmt : public Stmt {
    std::span<Expr*> children;
};

struct ExprStmt : public Stmt {
    Expr* child;
};

struct VarStmt : public Stmt {
    std::string_view name;
    Location         name_loc;

    Expr* type_expr = nullptr;
    Expr* init = nullptr;

    Symbol* sym = nullptr;
};

struct MultiVarName {
    std::string_view name;
    Location         loc;

    Symbol* sym = nullptr;
};

struct MultiVarStmt : public Stmt {
    std::span<MultiVarName> names;
    std::span<Expr*>        types;
    std::span<Expr*>        inits;
};

// ----------------------------------------------------------------------------

/// All of the variants of global declarations.
enum struct DeclKind : uint8_t {
    Err,

    Import,
    Func,
    Var,
    Def,

    MultiVar,
    MultiDef,
};

struct ImportDecl;
struct FuncDecl;
struct VarDecl;
struct MultiVarDecl;

struct Decl {
    DeclKind kind;
    Location loc;

    Decl* forward = nullptr;

    [[nodiscard]] auto as_import() const -> ImportDecl const&;
    [[nodiscard]] auto as_func() const -> FuncDecl const&;
    [[nodiscard]] auto as_var() const -> VarDecl const&;
    [[nodiscard]] auto as_def() const -> VarDecl const&;
    [[nodiscard]] auto as_multi_var() const -> MultiVarDecl const&;
    [[nodiscard]] auto as_multi_def() const -> MultiVarDecl const&;

    [[nodiscard]] auto as_import() -> ImportDecl&;
    [[nodiscard]] auto as_func() -> FuncDecl&;
    [[nodiscard]] auto as_var() -> VarDecl&;
    [[nodiscard]] auto as_def() -> VarDecl&;
    [[nodiscard]] auto as_multi_var() -> MultiVarDecl&;
    [[nodiscard]] auto as_multi_def() -> MultiVarDecl&;

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() const -> Decl const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() -> Decl* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The union operation of union find.
    constexpr void make_equal_to(Decl* other) {
        auto n = find();
        if (n != other) n->forward = other;
    }
};

struct ErrDecl : Decl {};

struct DeclAttributeKV {
    Location         loc;
    std::string_view name;
    Expr*            value = nullptr;
};

struct DeclAttribute {
    Location         loc;
    std::string_view qualified_name;
    std::string_view name;

    std::span<Expr*>           args;
    std::span<DeclAttributeKV> kwargs;
};

struct FuncParam {
    std::string_view name;
    Location         loc;

    Expr* type_expr = nullptr;
    bool  is_comptime = false;
};

struct FuncRet {
    std::string_view name;  // only for named returns
    Location         loc;
    Expr*            type_expr = nullptr;
};

struct ImportDecl : Decl {
    std::span<DeclAttribute>    attributes;
    std::string_view            name;
    std::span<std::string_view> path;

    Symbol* sym = nullptr;

    Location name_loc;
};

struct FuncDecl : Decl {
    std::string_view name;
    std::string_view attached_type;  // TODO: better name for this

    std::span<DeclAttribute> attributes;
    std::span<FuncParam>     params;
    std::span<FuncRet>       rets;

    Stmt*   body = nullptr;
    Symbol* sym = nullptr;

    Location name_loc;

    bool is_c_varargs = false;
};

struct VarDecl : Decl {
    std::string_view name;
    Location         name_loc;

    std::span<DeclAttribute> attributes;

    Expr* type_expr = nullptr;
    Expr* init = nullptr;

    Symbol* sym = nullptr;
};

struct MultiVarDecl : Decl {
    std::span<DeclAttribute> attributes;
    std::span<MultiVarName>  names;
    std::span<Expr*>         types;
    std::span<Expr*>         inits;
};

// ----------------------------------------------------------------------------

void to_json(nlohmann::json& j, ExprKind const& n);
void to_json(nlohmann::json& j, StmtKind const& n);
void to_json(nlohmann::json& j, DeclKind const& n);

void to_json(nlohmann::json& j, Expr const& n);
void to_json(nlohmann::json& j, Stmt const& n);
void to_json(nlohmann::json& j, Decl const& n);

void to_json(nlohmann::json& j, MultiVarName const& n);
void to_json(nlohmann::json& j, DeclAttributeKV const& n);
void to_json(nlohmann::json& j, DeclAttribute const& n);
void to_json(nlohmann::json& j, FuncParam const& n);
void to_json(nlohmann::json& j, FuncRet const& n);

void to_lisp(fmt::format_context& ctx, Expr const* expr, int depth = 0);
void to_lisp(fmt::format_context& ctx, Expr const& expr, int depth = 0);
void to_lisp(fmt::format_context& ctx, Stmt const* stmt, int depth = 0);
void to_lisp(fmt::format_context& ctx, Stmt const& stmt, int depth = 0);
void to_lisp(fmt::format_context& ctx, Decl const* decl, int depth = 0);
void to_lisp(fmt::format_context& ctx, Decl const& decl, int depth = 0);

void to_lisp(fmt::format_context& ctx, MultiVarName const& n, int depth = 0);
void to_lisp(fmt::format_context& ctx, DeclAttributeKV const& n, int depth = 0);
void to_lisp(fmt::format_context& ctx, DeclAttribute const& n, int depth = 0);
void to_lisp(fmt::format_context& ctx, FuncParam const& n, int depth = 0);
void to_lisp(fmt::format_context& ctx, FuncRet const& n, int depth = 0);

}  // namespace yal::ast

define_formatter_from_string_view(yal::ast::ExprKind);
define_formatter_from_string_view(yal::ast::StmtKind);
define_formatter_from_string_view(yal::ast::DeclKind);

define_formatter_from_string_view(yal::ast::Expr);
define_formatter_from_string_view(yal::ast::Stmt);
define_formatter_from_string_view(yal::ast::Decl);
