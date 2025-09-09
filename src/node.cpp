#include "node.hpp"

#include <fmt/ranges.h>

#include <libassert/assert.hpp>
#include <nlohmann/json.hpp>
#include <ranges>
#include <string_view>

#include "fmt/format.h"

namespace rv = std::ranges::views;

namespace yal::ast {
using nlohmann::json;

// clang-format off
auto Expr::as_arith() const -> ArithExpr const& { return static_cast<ArithExpr const&>(*this); }
auto Expr::as_field() const -> FieldExpr const& { return static_cast<FieldExpr const&>(*this); }
auto Expr::as_call() const -> CallExpr const& { return static_cast<CallExpr const&>(*this); }
auto Expr::as_ptr() const -> PtrExpr const& { return static_cast<PtrExpr const&>(*this); }
auto Expr::as_array() const -> ArrayExpr const& { return static_cast<ArrayExpr const&>(*this); }
auto Expr::as_id() const -> IdExpr const& { return static_cast<IdExpr const&>(*this); }
auto Expr::as_kw() const -> KwExpr const& { return static_cast<KwExpr const&>(*this); }
auto Expr::as_int() const -> IntExpr const& { return static_cast<IntExpr const&>(*this); }
auto Expr::as_string() const -> StringExpr const& { return static_cast<StringExpr const&>(*this); }

auto Expr::as_arith() -> ArithExpr& { return static_cast<ArithExpr&>(*this); }
auto Expr::as_field() -> FieldExpr& { return static_cast<FieldExpr&>(*this); }
auto Expr::as_call() -> CallExpr& { return static_cast<CallExpr&>(*this); }
auto Expr::as_ptr() -> PtrExpr& { return static_cast<PtrExpr&>(*this); }
auto Expr::as_array() -> ArrayExpr& { return static_cast<ArrayExpr&>(*this); }
auto Expr::as_id() -> IdExpr& { return static_cast<IdExpr&>(*this); }
auto Expr::as_kw() -> KwExpr& { return static_cast<KwExpr&>(*this); }
auto Expr::as_int() -> IntExpr& { return static_cast<IntExpr&>(*this); }
auto Expr::as_string() -> StringExpr& { return static_cast<StringExpr&>(*this); }

auto Stmt::as_block() const -> BlockStmt const& { return static_cast<BlockStmt const&>(*this); }
auto Stmt::as_return() const -> ReturnStmt const& { return static_cast<ReturnStmt const&>(*this); }
auto Stmt::as_expr() const -> ExprStmt const& { return static_cast<ExprStmt const&>(*this); }
auto Stmt::as_var() const -> VarStmt const& { return static_cast<VarStmt const&>(*this); }
auto Stmt::as_multi_var() const -> MultiVarStmt const& { return static_cast<MultiVarStmt const&>(*this); }
auto Stmt::as_def() const -> VarStmt const& { return static_cast<VarStmt const&>(*this); }
auto Stmt::as_multi_def() const -> MultiVarStmt const& { return static_cast<MultiVarStmt const&>(*this); }
auto Stmt::as_assign() const -> AssignStmt const& { return static_cast<AssignStmt const&>(*this); }
auto Stmt::as_multi_assign() const -> MultiAssignStmt const& { return static_cast<MultiAssignStmt const&>(*this); }

auto Stmt::as_block() -> BlockStmt& { return static_cast<BlockStmt&>(*this); }
auto Stmt::as_return() -> ReturnStmt& { return static_cast<ReturnStmt&>(*this); }
auto Stmt::as_expr() -> ExprStmt& { return static_cast<ExprStmt&>(*this); }
auto Stmt::as_var() -> VarStmt& { return static_cast<VarStmt&>(*this); }
auto Stmt::as_multi_var() -> MultiVarStmt& { return static_cast<MultiVarStmt&>(*this); }
auto Stmt::as_def() -> VarStmt& { return static_cast<VarStmt&>(*this); }
auto Stmt::as_multi_def() -> MultiVarStmt& { return static_cast<MultiVarStmt&>(*this); }
auto Stmt::as_assign() -> AssignStmt& { return static_cast<AssignStmt&>(*this); }
auto Stmt::as_multi_assign() -> MultiAssignStmt& { return static_cast<MultiAssignStmt&>(*this); }

auto Decl::as_import() const -> ImportDecl const& { return static_cast<ImportDecl const&>(*this); }
auto Decl::as_func() const -> FuncDecl const& { return static_cast<FuncDecl const&>(*this); }
auto Decl::as_var() const -> VarDecl const& { return static_cast<VarDecl const&>(*this); }
auto Decl::as_def() const -> VarDecl const& { return static_cast<VarDecl const&>(*this); }
auto Decl::as_multi_var() const -> MultiVarDecl const& { return static_cast<MultiVarDecl const&>(*this); }
auto Decl::as_multi_def() const -> MultiVarDecl const& { return static_cast<MultiVarDecl const&>(*this); }

auto Decl::as_import() -> ImportDecl& { return static_cast<ImportDecl&>(*this); }
auto Decl::as_func() -> FuncDecl& { return static_cast<FuncDecl&>(*this); }
auto Decl::as_var() -> VarDecl& { return static_cast<VarDecl&>(*this); }
auto Decl::as_def() -> VarDecl& { return static_cast<VarDecl&>(*this); }
auto Decl::as_multi_var() -> MultiVarDecl& { return static_cast<MultiVarDecl&>(*this); }
auto Decl::as_multi_def() -> MultiVarDecl& { return static_cast<MultiVarDecl&>(*this); }
// clang-format on

void to_json(nlohmann::json& j, ExprKind const& n) { j = fmt::to_string(n); }
void to_json(nlohmann::json& j, StmtKind const& n) { j = fmt::to_string(n); }
void to_json(nlohmann::json& j, DeclKind const& n) { j = fmt::to_string(n); }

template <typename T>
requires std::is_pointer_v<T>
void to_json_arr(json& j, std::span<T> const& items) {
    j = json::array();
    for (auto const& c : items) {
        if (c)
            j.push_back(*c);
        else
            j.push_back(json{});
    }
}

template <typename T>
void to_json_arr(json& j, std::span<T> const& items) {
    j = json::array();
    for (auto const& c : items) {
        j.push_back(c);
    }
}

void to_json(nlohmann::json& j, Expr const& n) {
    j = json{
        {"kind",                n.kind},
        { "loc", fmt::to_string(n.loc)},
    };

    switch (n.kind) {
        case ExprKind::Err: break;

        case ExprKind::Neg:
        case ExprKind::Add:
        case ExprKind::Sub:
        case ExprKind::Mul:
        case ExprKind::Div:
        case ExprKind::Mod: {
            auto& arith = n.as_arith();
            j["lhs"] = arith.lhs ? *arith.lhs : json{};
            j["rhs"] = arith.rhs ? *arith.rhs : json{};
        } break;

        case ExprKind::Field: {
            auto& field = n.as_field();
            j["obj"] = field.obj ? *field.obj : json{};
            j["field"] = field.name;
        } break;

        case ExprKind::Call: {
            auto& call = n.as_call();
            j["callee"] = call.callee ? *call.callee : json{};
            to_json_arr(j["args"], call.args);
        } break;

        case ExprKind::Ptr:
        case ExprKind::MultiPtr:
        case ExprKind::Slice: {
            auto& ptr = n.as_ptr();
            j["inner"] = ptr.inner ? *ptr.inner : json{};
            j["is_const"] = ptr.is_const;
        } break;

        case ExprKind::Array: {
            auto& arr = n.as_array();
            j["count"] = arr.count ? *arr.count : json{};
            j["inner"] = arr.inner ? *arr.inner : json{};
            j["is_const"] = arr.is_const;
        } break;

        case ExprKind::Id: {
            auto& id = n.as_id();
            j["value"] = id.value;
            j["sym"] = id.sym ? json(fmt::to_string(*id.sym)) : json{};
        } break;

        case ExprKind::Kw: {
            auto& kw = n.as_kw();
            j["value"] = kw.value;
        } break;

        case ExprKind::Int: {
            auto& integer = n.as_int();
            j["value"] = integer.value;
        } break;

        case ExprKind::String: {
            auto& string = n.as_string();
            j["value"] = string.value;
        } break;
    }
}

void to_json(nlohmann::json& j, Stmt const& n) {
    j = json{
        {"kind",                n.kind},
        { "loc", fmt::to_string(n.loc)},
    };

    switch (n.kind) {
        case StmtKind::Err: break;

        case StmtKind::Block: {
            auto& block = n.as_block();
            to_json_arr(j["children"], block.children);
        } break;

        case StmtKind::Return: {
            auto& ret = n.as_return();
            to_json_arr(j["children"], ret.children);
        } break;

        case StmtKind::Expr: {
            auto& expr = n.as_expr();
            j["child"] = *expr.child;
        } break;

        case StmtKind::Var:
        case StmtKind::Def: {
            auto& var = n.as_var();
            j["name"] = var.name;
            j["name_loc"] = fmt::to_string(var.name_loc);
            j["sym"] = var.sym ? json(fmt::to_string(*var.sym)) : json{};
            j["type"] = var.type_expr ? *var.type_expr : json{};
            j["init"] = var.init ? *var.init : json{};
        } break;

        case StmtKind::MultiVar:
        case StmtKind::MultiDef: {
            auto& multi_var = n.as_multi_var();

            to_json_arr(j["names"], multi_var.names);
            to_json_arr(j["types"], multi_var.types);
            to_json_arr(j["inits"], multi_var.inits);
        } break;

        case StmtKind::Assign: {
            auto& assign = n.as_assign();
            j["lhs"] = assign.lhs ? *assign.lhs : json{};
            j["rhs"] = assign.rhs ? *assign.rhs : json{};
        } break;

        case StmtKind::MultiAssign: {
            auto& assign = n.as_multi_assign();
            to_json_arr(j["lhs"], assign.lhs);
            to_json_arr(j["rhs"], assign.rhs);
        } break;
    }
}

void to_json(nlohmann::json& j, MultiVarName const& n) {
    j = json{
        {"name",                                        n.name},
        { "loc",                         fmt::to_string(n.loc)},
        { "sym", n.sym ? json(fmt::to_string(*n.sym)) : json{}},
    };
}

void to_json(nlohmann::json& j, DeclAttributeKV const& n) {
    j = json{
        { "name",                      n.name},
        {"value", n.value ? *n.value : json{}},
    };
}

void to_json(nlohmann::json& j, DeclAttribute const& n) {
    j = json{
        {          "name",           n.name},
        {"qualified_name", n.qualified_name},
    };

    to_json_arr(j["args"], n.args);
    to_json_arr(j["kwargs"], n.kwargs);
}

void to_json(nlohmann::json& j, FuncParam const& n) {
    j = json{
        {       "name",                              n.name},
        {  "type_expr", n.type_expr ? *n.type_expr : json{}},
        {"is_comptime",                       n.is_comptime},
    };
}

void to_json(nlohmann::json& j, FuncRet const& n) {
    j = json{
        {     "name",                              n.name},
        {"type_expr", n.type_expr ? *n.type_expr : json{}},
    };
}

void to_json(nlohmann::json& j, Decl const& n) {
    j = json{
        {"kind",                n.kind},
        { "loc", fmt::to_string(n.loc)},
    };

    switch (n.kind) {
        case DeclKind::Err: break;

        case DeclKind::Import: {
            auto& imp = n.as_import();

            j["name"] = imp.name;
            j["path"] = imp.path;
            to_json_arr(j["attributes"], imp.attributes);
        } break;

        case DeclKind::Func: {
            auto& func = n.as_func();

            j["name"] = func.name;
            j["attached_type"] = func.attached_type;
            j["name_loc"] = fmt::to_string(func.name_loc);
            to_json_arr(j["attributes"], func.attributes);
            to_json_arr(j["params"], func.params);
            to_json_arr(j["rets"], func.rets);
            j["is_c_varargs"] = func.is_c_varargs;

            j["body"] = func.body ? *func.body : json{};
        } break;

        case DeclKind::Var:
        case DeclKind::Def: {
            auto& var = n.as_var();

            j["name"] = var.name;
            j["name_loc"] = fmt::to_string(var.name_loc);
            to_json_arr(j["attributes"], var.attributes);

            j["type_expr"] = var.type_expr ? *var.type_expr : json{};
            j["init"] = var.init ? *var.init : json{};
        } break;

        case DeclKind::MultiVar:
        case DeclKind::MultiDef: {
            auto& multi_var = n.as_multi_var();

            to_json_arr(j["attributes"], multi_var.attributes);
            to_json_arr(j["names"], multi_var.names);
            to_json_arr(j["types"], multi_var.types);
            to_json_arr(j["inits"], multi_var.inits);
        } break;
    }
}

void indent_by(fmt::format_context& ctx, int depth) {
    while (depth--) {
        fmt::format_to(ctx.out(), "  ");
    }
}

// same as indent_by, but moves to the next line before indenting
void indent_by_wln(fmt::format_context& ctx, int depth) {
    fmt::format_to(ctx.out(), "\n");
    while (depth--) {
        fmt::format_to(ctx.out(), "  ");
    }
}

template <typename T>
void to_lisp_arr(fmt::format_context& ctx, std::span<T> const& items,
                 int depth) {
    for (auto const& c : items) {
        indent_by_wln(ctx, depth);
        to_lisp(ctx, c, depth);
    }
}

void to_lisp(fmt::format_context& ctx, Expr const* expr, int depth) {
    if (expr) {
        to_lisp(ctx, *expr, depth);
    } else {
        fmt::format_to(ctx.out(), "#nullptr#");
    }
}

void to_lisp(fmt::format_context& ctx, Expr const& expr, int depth) {
    // TODO: when we get types, we add them right here after the node kind
    // TODO: show forward somehow?
    fmt::format_to(ctx.out(), "({}Expr", expr.kind);

    switch (expr.kind) {
        case ExprKind::Err: break;

        case ExprKind::Neg:
        case ExprKind::Add:
        case ExprKind::Sub:
        case ExprKind::Mul:
        case ExprKind::Div:
        case ExprKind::Mod: {
            auto& arith = expr.as_arith();

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, arith.lhs, depth + 1);

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, arith.rhs, depth + 1);
        } break;

        case ExprKind::Field: {
            auto& field = expr.as_field();

            fmt::format_to(ctx.out(), " {:?}", field.name);
            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, field.obj, depth + 1);
        } break;

        case ExprKind::Call: {
            auto& call = expr.as_call();

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, call.callee, depth + 1);
            to_lisp_arr(ctx, call.args, depth + 1);
        } break;

        case ExprKind::Ptr:
        case ExprKind::MultiPtr:
        case ExprKind::Slice: {
            auto& ptr = expr.as_ptr();

            if (ptr.is_const) fmt::format_to(ctx.out(), " const");

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, ptr.inner, depth + 1);
        } break;

        case ExprKind::Array: {
            auto& arr = expr.as_array();

            if (arr.is_const) fmt::format_to(ctx.out(), " const");

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, arr.count, depth + 1);

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, arr.inner, depth + 1);
        } break;

        case ExprKind::Id: {
            auto& id = expr.as_id();

            fmt::format_to(ctx.out(), " {:?}", id.value);
            if (id.sym) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "sym: {}", *id.sym);
            }
        } break;

        case ExprKind::Kw: {
            auto& kw = expr.as_kw();

            fmt::format_to(ctx.out(), " {:?}", kw.value);
        } break;

        case ExprKind::Int: {
            auto& integer = expr.as_int();
            fmt::format_to(ctx.out(), " {}", integer.value);
        } break;

        case ExprKind::String: {
            auto& string = expr.as_string();
            fmt::format_to(ctx.out(), " {:?}", string.value);
        } break;
    }

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, Stmt const* stmt, int depth) {
    if (stmt) {
        to_lisp(ctx, *stmt, depth);
    } else {
        fmt::format_to(ctx.out(), "#nullptr#");
    }
}

void to_lisp(fmt::format_context& ctx, MultiVarName const& n, int depth) {
    fmt::format_to(ctx.out(), "({}", n.name);

    if (n.sym) {
        indent_by_wln(ctx, depth + 1);
        fmt::format_to(ctx.out(), "sym: {}", *n.sym);
    }

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, Stmt const& stmt, int depth) {
    fmt::format_to(ctx.out(), "({}Stmt", stmt.kind);

    switch (stmt.kind) {
        case StmtKind::Err: break;

        case StmtKind::Block: {
            auto& block = stmt.as_block();
            to_lisp_arr(ctx, block.children, depth + 1);
        } break;

        case StmtKind::Return: {
            auto& ret = stmt.as_return();
            to_lisp_arr(ctx, ret.children, depth + 1);
        } break;

        case StmtKind::Expr: {
            auto& expr = stmt.as_expr();
            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, expr.child, depth + 1);
        } break;

        case StmtKind::Var:
        case StmtKind::Def: {
            auto& var = stmt.as_var();

            fmt::format_to(ctx.out(), " {:?}", var.name);
            if (var.sym) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "sym: {}", *var.sym);
            }

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, var.type_expr, depth + 1);

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, var.init, depth + 1);
        } break;

        case StmtKind::MultiVar:
        case StmtKind::MultiDef: {
            auto& multi_var = stmt.as_multi_var();

            if (!multi_var.names.empty()) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "names:");
                to_lisp_arr(ctx, multi_var.names, depth + 1);
            }

            if (!multi_var.types.empty()) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "types:");
                to_lisp_arr(ctx, multi_var.types, depth + 1);
            }

            if (!multi_var.inits.empty()) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "inits:");
                to_lisp_arr(ctx, multi_var.inits, depth + 1);
            }
        } break;

        case StmtKind::Assign: {
            auto& assign = stmt.as_assign();

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, assign.lhs, depth + 1);

            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, assign.rhs, depth + 1);
        } break;

        case StmtKind::MultiAssign: {
            auto& assign = stmt.as_multi_assign();

            to_lisp_arr(ctx, assign.lhs, depth + 1);
            to_lisp_arr(ctx, assign.rhs, depth + 1);
        } break;
    }

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, Decl const* decl, int depth) {
    if (decl) {
        to_lisp(ctx, *decl, depth);
    } else {
        fmt::format_to(ctx.out(), "#nullptr#");
    }
}

void to_lisp(fmt::format_context& ctx, DeclAttributeKV const& n, int depth) {
    fmt::format_to(ctx.out(), "(KV {:?}", n.name);
    indent_by_wln(ctx, depth + 1);
    to_lisp(ctx, n.value, depth + 1);
    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, DeclAttribute const& n, int depth) {
    fmt::format_to(ctx.out(), "(Attribute");

    if (!n.qualified_name.empty())
        fmt::format_to(ctx.out(), " {:?}.", n.qualified_name);
    fmt::format_to(ctx.out(), " {:?}", n.name);

    to_lisp_arr(ctx, n.args, depth + 1);
    to_lisp_arr(ctx, n.kwargs, depth + 1);

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, FuncParam const& n, int depth) {
    fmt::format_to(ctx.out(), "(FuncParam {}{:?}", n.is_comptime ? "$" : "",
                   n.name);

    indent_by_wln(ctx, depth + 1);
    to_lisp(ctx, n.type_expr, depth + 1);

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, FuncRet const& n, int depth) {
    fmt::format_to(ctx.out(), "(FuncRet {:?}", n.name);

    indent_by_wln(ctx, depth + 1);
    to_lisp(ctx, n.type_expr, depth + 1);

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, Decl const& decl, int depth) {
    fmt::format_to(ctx.out(), "({}Decl", decl.kind);

    switch (decl.kind) {
        case DeclKind::Err: break;

        case DeclKind::Import: {
            auto& imp = decl.as_import();

            fmt::format_to(ctx.out(), " {}", fmt::join(imp.path, "/"));
            fmt::format_to(ctx.out(), " {}", imp.name);
            to_lisp_arr(ctx, imp.attributes, depth + 1);
        } break;

        case DeclKind::Func: {
            auto& func = decl.as_func();

            if (!func.attached_type.empty())
                fmt::format_to(ctx.out(), " {:?}.", func.attached_type);
            fmt::format_to(ctx.out(), " {:?}", func.name);

            // TODO: show that a function has c style varargs
            to_lisp_arr(ctx, func.attributes, depth + 1);
            to_lisp_arr(ctx, func.params, depth + 1);
            to_lisp_arr(ctx, func.rets, depth + 1);

            to_lisp(ctx, func.body, depth + 1);
        } break;

        case DeclKind::Var:
        case DeclKind::Def: {
            auto& var = decl.as_var();

            fmt::format_to(ctx.out(), " {:?}", var.name);
            if (var.sym) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "sym: {}", *var.sym);
            }

            to_lisp_arr(ctx, var.attributes, depth + 1);
            to_lisp(ctx, var.type_expr, depth + 1);
            to_lisp(ctx, var.init, depth + 1);
        } break;

        case DeclKind::MultiVar:
        case DeclKind::MultiDef: {
            auto& multi_var = decl.as_multi_var();

            to_lisp_arr(ctx, multi_var.attributes, depth + 1);

            if (!multi_var.names.empty()) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "names:");
                to_lisp_arr(ctx, multi_var.names, depth + 1);
            }

            if (!multi_var.types.empty()) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "types:");
                to_lisp_arr(ctx, multi_var.types, depth + 1);
            }

            if (!multi_var.inits.empty()) {
                indent_by_wln(ctx, depth + 1);
                fmt::format_to(ctx.out(), "inits:");
                to_lisp_arr(ctx, multi_var.inits, depth + 1);
            }
        } break;
    }

    fmt::format_to(ctx.out(), ")");
}

}  // namespace yal::ast

auto fmt::formatter<yal::ast::ExprKind>::format(yal::ast::ExprKind const& p,
                                                format_context& ctx) const
    -> format_context ::iterator {
    std::string_view name = "???";
    switch (p) {
        case yal::ast::ExprKind::Err: name = "Err"; break;
        case yal::ast::ExprKind::Neg: name = "Neg"; break;
        case yal::ast::ExprKind::Add: name = "Add"; break;
        case yal::ast::ExprKind::Sub: name = "Sub"; break;
        case yal::ast::ExprKind::Mul: name = "Mul"; break;
        case yal::ast::ExprKind::Div: name = "Div"; break;
        case yal::ast::ExprKind::Mod: name = "Mod"; break;
        case yal::ast::ExprKind::Field: name = "Field"; break;
        case yal::ast::ExprKind::Call: name = "Call"; break;
        case yal::ast::ExprKind::Ptr: name = "Ptr"; break;
        case yal::ast::ExprKind::MultiPtr: name = "MultiPtr"; break;
        case yal::ast::ExprKind::Slice: name = "Slice"; break;
        case yal::ast::ExprKind::Array: name = "Array"; break;
        case yal::ast::ExprKind::Id: name = "Id"; break;
        case yal::ast::ExprKind::Kw: name = "Kw"; break;
        case yal::ast::ExprKind::Int: name = "Int"; break;
        case yal::ast::ExprKind::String: name = "String"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::ast::StmtKind>::format(yal::ast::StmtKind const& p,
                                                format_context& ctx) const
    -> format_context ::iterator {
    std::string_view name = "???";
    switch (p) {
        case yal::ast::StmtKind::Err: name = "Err"; break;
        case yal::ast::StmtKind::Block: name = "Block"; break;
        case yal::ast::StmtKind::Return: name = "Return"; break;
        case yal::ast::StmtKind::Expr: name = "Expr"; break;
        case yal::ast::StmtKind::Var: name = "Var"; break;
        case yal::ast::StmtKind::Def: name = "Def"; break;
        case yal::ast::StmtKind::MultiVar: name = "MultiVar"; break;
        case yal::ast::StmtKind::MultiDef: name = "MultiDef"; break;
        case yal::ast::StmtKind::Assign: name = "Assign"; break;
        case yal::ast::StmtKind::MultiAssign: name = "MultiAssign"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::ast::DeclKind>::format(yal::ast::DeclKind const& p,
                                                format_context& ctx) const
    -> format_context ::iterator {
    std::string_view name = "???";
    switch (p) {
        case yal::ast::DeclKind::Err: name = "Err"; break;
        case yal::ast::DeclKind::Import: name = "Import"; break;
        case yal::ast::DeclKind::Func: name = "Func"; break;
        case yal::ast::DeclKind::Var: name = "Var"; break;
        case yal::ast::DeclKind::Def: name = "Def"; break;
        case yal::ast::DeclKind::MultiVar: name = "MultiVar"; break;
        case yal::ast::DeclKind::MultiDef: name = "MultiDef"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::ast::Expr>::format(yal::ast::Expr const& p,
                                            format_context&       ctx) const
    -> format_context::iterator {
    yal::ast::to_lisp(ctx, p);
    return ctx.out();
}

auto fmt::formatter<yal::ast::Stmt>::format(yal::ast::Stmt const& p,
                                            format_context&       ctx) const
    -> format_context::iterator {
    yal::ast::to_lisp(ctx, p);
    return ctx.out();
}

auto fmt::formatter<yal::ast::Decl>::format(yal::ast::Decl const& p,
                                            format_context&       ctx) const
    -> format_context::iterator {
    yal::ast::to_lisp(ctx, p);
    return ctx.out();
}
