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

void to_json(nlohmann::json& j, ExprKind const& n) { j = fmt::to_string(n); }
void to_json(nlohmann::json& j, StmtKind const& n) { j = fmt::to_string(n); }

void to_json(nlohmann::json& j, Expr const& n) {
    j = json{
        {        "kind",                          n.kind},
        {         "loc",           fmt::to_string(n.loc)},
        {        "decl", n.decl ? json{*n.decl} : json{}},
        {"string_value",                  n.string_value},
        {   "int_value",                     n.int_value},
    };

    auto children = json::array();
    for (auto const& child : n.children()) {
        if (child) {
            children.push_back(*child);
        } else
            children.push_back(json{});
    }

    j["children"] = children;
}

void to_json(nlohmann::json& j, Stmt const& n) {
    j = json{
        {        "kind",                          n.kind},
        {         "loc",           fmt::to_string(n.loc)},
        {   "value_loc",     fmt::to_string(n.value_loc)},
        {        "decl", n.decl ? json{*n.decl} : json{}},
        {"string_value",                  n.string_value},
    };

    if (n.kind == StmtKind::Block) {
        auto children = json::array();
        for (auto const& child : n.stmt_children()) {
            if (child) {
                children.push_back(*child);
            } else
                children.push_back(json{});
        }

        j["children"] = children;
    }

    else if (n.kind == StmtKind::Return || n.kind == StmtKind::Expr ||
             n.kind == StmtKind::Var) {
        auto children = json::array();
        for (auto const& child : n.expr_children()) {
            if (child) {
                children.push_back(*child);
            } else
                children.push_back(json{});
        }

        j["children"] = children;
    }

    else if (n.kind == StmtKind::MultiVar) {
        auto children = json::array();
        for (auto const& name : n.multi_var_stmt()->names) {
            if (name)
                children.push_back(*name);
            else
                children.push_back(json{});
        }

        j["names"] = children;

        children.clear();
        for (auto const& type : n.multi_var_stmt()->types) {
            if (type)
                children.push_back(*type);
            else
                children.push_back(json{});
        }

        j["types"] = children;

        children.clear();
        for (auto const& init : n.multi_var_stmt()->inits) {
            if (init)
                children.push_back(*init);
            else
                children.push_back(json{});
        }

        j["inits"] = children;
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

void to_lisp(fmt::format_context& ctx, Expr const& expr, int depth) {
    // TODO: when we get types, we add them right here after the node kind
    fmt::format_to(ctx.out(), "({}Expr", expr.kind);

    if (expr.kind == ExprKind::Id || expr.kind == ExprKind::String) {
        fmt::format_to(ctx.out(), " {:?}", expr.string_value);
    } else if (expr.kind == ExprKind::Int) {
        fmt::format_to(ctx.out(), " {}", expr.int_value);
    }

    if (expr.decl) {
        indent_by_wln(ctx, depth + 1);
        fmt::format_to(ctx.out(), "decl: {}", *expr.decl);
    }

    for (auto const& child : expr.children()) {
        indent_by_wln(ctx, depth + 1);
        if (child)
            to_lisp(ctx, *child, depth + 1);
        else
            fmt::format_to(ctx.out(), "#nullptr#");
    }

    fmt::format_to(ctx.out(), ")");
}

void to_lisp(fmt::format_context& ctx, Stmt const& stmt, int depth) {
    fmt::format_to(ctx.out(), "({}Stmt", stmt.kind);

    if (stmt.decl) {
        indent_by_wln(ctx, depth + 1);
        fmt::format_to(ctx.out(), "decl: {}", *stmt.decl);
    }

    if (stmt.kind == StmtKind::Block) {
        for (auto const& child : stmt.stmt_children()) {
            indent_by_wln(ctx, depth + 1);
            to_lisp(ctx, *child, depth + 1);
        }
    }

    else if (stmt.kind == StmtKind::Return || stmt.kind == StmtKind::Expr ||
             stmt.kind == StmtKind::Var) {
        for (auto const& child : stmt.expr_children()) {
            indent_by_wln(ctx, depth + 1);
            if (child)
                to_lisp(ctx, *child, depth + 1);
            else
                fmt::format_to(ctx.out(), "#nullptr#");
        }
    }

    else if (stmt.kind == StmtKind::MultiVar) {
        auto types = stmt.multi_var_stmt();

        if (!types->names.empty()) {
            indent_by_wln(ctx, depth + 1);
            fmt::format_to(ctx.out(), "names:");
            for (auto const& name : types->names) {
                indent_by_wln(ctx, depth + 2);
                to_lisp(ctx, *name, depth + 2);
            }
        }

        if (!types->types.empty()) {
            indent_by_wln(ctx, depth + 1);
            fmt::format_to(ctx.out(), "types:");
            for (auto const& type : types->types) {
                indent_by_wln(ctx, depth + 2);
                to_lisp(ctx, *type, depth + 2);
            }
        }

        if (!types->inits.empty()) {
            indent_by_wln(ctx, depth + 1);
            fmt::format_to(ctx.out(), "inits:");
            for (auto const& init : types->inits) {
                indent_by_wln(ctx, depth + 2);
                to_lisp(ctx, *init, depth + 2);
            }
        }
    }

    fmt::format_to(ctx.out(), ")");
}

// ============================================================================
// OLD IDEAS
// ============================================================================

auto to_json_array(std::span<Node* const> nodes) -> json {
    auto array = json::array();
    for (auto child : nodes) {
        json cj;
        if (child) child->to_json(cj);

        array.push_back(std::move(cj));
    }

    return array;
}

// ============================================================================

auto Node::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(
        ctx.out(), "Node({:?}, {}, {})", kind, loc,
        fmt::join(get_children() | rv::filter([](Node* n) {
                      return n != nullptr;
                  }) | rv::transform([](Node* n) -> Node const& { return *n; }),
                  ", "));
}

void Node::to_json(nlohmann::json& j) const {
    to_json_common_values(j);
    to_json_children(j);
}

void Node::to_json_common_values(nlohmann::json& j) const {
    j = json{
        {"kind",  fmt::to_string(get_kind())},
        { "loc",   fmt::to_string(get_loc())},
        {"decl", decl ? json(*decl) : json{}},
    };
}

void Node::to_json_children(nlohmann::json& j) const {
    if (auto children = get_children(); !children.empty()) {
        j["children"] = to_json_array(children);
    }
}

// ============================================================================

#define define_as_node_kind(name, kind)                             \
    auto as_node_##name(Node* n) -> Node##kind* {                   \
        if (!n) return nullptr;                                     \
        ASSERT(n->get_kind() == NodeKind::kind, "expected " #kind); \
        return static_cast<Node##kind*>(n);                         \
    }

auto as_node_pack(Node* n) -> NodePack* {
    if (!n || n->is_err()) return nullptr;

    ASSERT(n->get_kind() == NodeKind::NodePack, "expected NodePack");
    return static_cast<NodePack*>(n);
}

define_as_node_kind(block, Block);

#undef define_as_node_kind

// ============================================================================

auto NodeFuncArg::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "NodeFuncArg({}, {}: {})", get_loc(),
                          get_name(),
                          get_type() ? *get_type() : NodeErr{get_loc()});
}

void NodeFuncArg::to_json(nlohmann::json& j) const {
    Node::to_json_common_values(j);
    j["name"] = get_name();
    if (auto ty = get_type()) ty->to_json(j["type"]);
}

auto NodeFuncNamedRet::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "NodeFuncNamedRet({}, {}: {})", get_loc(),
                          get_name(),
                          get_type() ? *get_type() : NodeErr{get_loc()});
}

void NodeFuncNamedRet::to_json(nlohmann::json& j) const {
    Node::to_json_common_values(j);
    j["name"] = get_name();
    if (auto ty = get_type()) ty->to_json(j["type"]);
}

// ============================================================================

auto NodeFlatModule::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(
        ctx.out(), "NodeFlatModule({}, {:?}, {})", get_loc(), module_name,
        fmt::join(get_children() | rv::filter([](Node* n) {
                      return n != nullptr;
                  }) | rv::transform([](Node* n) -> Node const& { return *n; }),
                  ", "));
}

void NodeFlatModule::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["module_name"] = module_name;
}

auto NodeFile::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(
        ctx.out(), "NodeFile({}, {:?}, {})", get_loc(), module_name,
        fmt::join(get_children() | rv::filter([](Node* n) {
                      return n != nullptr;
                  }) | rv::transform([](Node* n) -> Node const& { return *n; }),
                  ", "));
}

void NodeFile::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["module_name"] = module_name;
}

// ============================================================================

auto NodeAttribute::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(
        ctx.out(), "NodeAttribute({}, {:?}, {:?}, {})", get_loc(),
        get_qualified_name(), get_name(),
        fmt::join(get_children() | rv::filter([](Node* n) {
                      return n != nullptr;
                  }) | rv::transform([](Node* n) -> Node const& { return *n; }),
                  ", "));
}

void NodeAttribute::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["name"] = get_name();
    if (auto n = get_qualified_name(); !n.empty()) j["qualified_name"] = n;
}

auto NodeAttributeKV::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "NodeAttributeKV({}, {}={})", get_loc(),
                          get_key(),
                          get_value() ? *get_value() : NodeErr{get_loc()});
}

void NodeAttributeKV::to_json(nlohmann::json& j) const {
    Node::to_json_common_values(j);

    j["key"] = get_key();
    if (auto v = get_value()) {
        j["value"] = *v;
    }
}

// ============================================================================

auto NodeVar::get_attributes() const -> NodePack* {
    return as_node_pack(child_at(3));
}

auto NodeVar::get_names() const -> NodePack* {
    return as_node_pack(child_at(0));
}

auto NodeVar::get_types() const -> NodePack* {
    return as_node_pack(child_at(1));
}

auto NodeVar::get_inits() const -> NodePack* {
    return as_node_pack(child_at(2));
}

void NodeVar::to_json(nlohmann::json& j) const {
    Node::to_json_common_values(j);

    if (auto attributes = get_attributes()) {
        attributes->to_json(j["attributes"]);
    } else {
        j["attributes"] = json();
    }

    if (auto names = get_names()) {
        names->to_json(j["names"]);
    } else {
        j["names"] = json();
    }

    if (auto types = get_types()) {
        types->to_json(j["types"]);
    } else {
        j["types"] = json();
    }

    if (auto inits = get_inits()) {
        inits->to_json(j["inits"]);
    } else {
        j["inits"] = json();
    }
}

auto NodeDef::get_attributes() const -> NodePack* {
    return as_node_pack(child_at(3));
}

auto NodeDef::get_gargs() const -> NodePack* {
    return as_node_pack(child_at(4));
}

auto NodeDef::get_names() const -> NodePack* {
    return as_node_pack(child_at(0));
}

auto NodeDef::get_types() const -> NodePack* {
    return as_node_pack(child_at(1));
}

auto NodeDef::get_inits() const -> NodePack* {
    return as_node_pack(child_at(2));
}

void NodeDef::to_json(nlohmann::json& j) const {
    Node::to_json_common_values(j);

    if (auto attributes = get_attributes()) {
        attributes->to_json(j["attributes"]);
    } else {
        j["attributes"] = json();
    }

    if (auto gargs = get_gargs()) {
        gargs->to_json(j["gargs"]);
    } else {
        j["gargs"] = json();
    }

    if (auto names = get_names()) {
        names->to_json(j["names"]);
    } else {
        j["names"] = json();
    }

    if (auto types = get_types()) {
        types->to_json(j["types"]);
    } else {
        j["types"] = json();
    }

    if (auto inits = get_inits()) {
        inits->to_json(j["inits"]);
    } else {
        j["inits"] = json();
    }
}

// ============================================================================

auto NodeFunc::get_attributes() const -> NodePack* {
    return as_node_pack(child_at(0));
}

auto NodeFunc::get_gargs() const -> NodePack* {
    return as_node_pack(child_at(1));
}

auto NodeFunc::get_args() const -> NodePack* {
    return as_node_pack(child_at(2));
}

auto NodeFunc::get_ret() const -> NodePack* {
    return as_node_pack(child_at(3));
}

auto NodeFunc::get_body() const -> NodeBlock* {
    return as_node_block(child_at(4));
}

auto NodeFunc::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    fmt::format_to(ctx.out(), "NodeFunc({}, {:?}.{:?}", get_loc(),
                   get_attached_type(), get_name());

    if (auto attributes = get_attributes())
        fmt::format_to(ctx.out(), ", attributes={}", *attributes);
    if (auto gargs = get_gargs())
        fmt::format_to(ctx.out(), ", gargs={}", *gargs);
    if (auto args = get_args()) fmt::format_to(ctx.out(), ", args={}", *args);
    if (auto ret = get_ret()) fmt::format_to(ctx.out(), ", ret={}", *ret);
    if (auto body = get_body()) fmt::format_to(ctx.out(), ", body={}", *body);

    return fmt::format_to(ctx.out(), ")");
}

void NodeFunc::to_json(nlohmann::json& j) const {
    Node::to_json_common_values(j);
    j["name"] = get_name();
    j["attached_type"] = get_attached_type();
    j["is_c_varargs"] = get_is_c_varargs();

    if (auto attributes = get_attributes()) {
        attributes->to_json(j["attributes"]);
    } else {
        j["attributes"] = json();
    }

    if (auto args = get_args()) {
        args->to_json(j["args"]);
    } else {
        j["args"] = json();
    }

    if (auto gargs = get_gargs()) {
        gargs->to_json(j["gargs"]);
    } else {
        j["gargs"] = json();
    }

    if (auto ret = get_ret()) {
        ret->to_json(j["ret"]);
    } else {
        j["ret"] = json();
    }

    if (auto body = get_body()) {
        body->to_json(j["body"]);
    }
}

// ============================================================================

void NodeId::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["name"] = get_value();
}

auto NodeId::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "NodeId({}, {})", get_loc(), get_value());
}

// ============================================================================

void NodeInt::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["value"] = get_value();
}

auto NodeInt::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "NodeInt({}, {})", get_loc(), get_value());
}

// ============================================================================

auto NodeString::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(ctx.out(), "NodeString({}, {:?})", get_loc(),
                          get_value());
}

void NodeString::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["value"] = get_value();
}

// ============================================================================

void to_json(nlohmann::json& j, NodeKind const& n) { j = fmt::to_string(n); }

void to_json(nlohmann::json& j, Node const& t) { t.to_json(j); }

}  // namespace yal::ast

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
        case yal::ast::StmtKind::MultiVar: name = "MultiVar"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

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
        case yal::ast::ExprKind::Id: name = "Id"; break;
        case yal::ast::ExprKind::Int: name = "Int"; break;
        case yal::ast::ExprKind::String: name = "String"; break;
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

// ============================================================================
// OLD stuff
// ============================================================================

auto fmt::formatter<yal::ast::NodeKind>::format(yal::ast::NodeKind const& p,
                                                format_context& ctx) const
    -> format_context ::iterator {
    std::string_view name = "???";
    switch (p) {
        case yal::ast::NodeKind::Err: name = "Err"; break;
        case yal::ast::NodeKind::FlatModule: name = "FlatModule"; break;
        case yal::ast::NodeKind::File: name = "File"; break;
        case yal::ast::NodeKind::Attribute: name = "Attribute"; break;
        case yal::ast::NodeKind::AttributeKV: name = "AttributeKV"; break;
        case yal::ast::NodeKind::Var: name = "Var"; break;
        case yal::ast::NodeKind::Def: name = "Def"; break;
        case yal::ast::NodeKind::Func: name = "Func"; break;
        case yal::ast::NodeKind::Block: name = "Block"; break;
        case yal::ast::NodeKind::Return: name = "Return"; break;
        case yal::ast::NodeKind::ExprStmt: name = "ExprStmt"; break;
        case yal::ast::NodeKind::Neg: name = "Neg"; break;
        case yal::ast::NodeKind::Add: name = "Add"; break;
        case yal::ast::NodeKind::Sub: name = "Sub"; break;
        case yal::ast::NodeKind::Mul: name = "Mul"; break;
        case yal::ast::NodeKind::Div: name = "Div"; break;
        case yal::ast::NodeKind::Mod: name = "Mod"; break;
        case yal::ast::NodeKind::Id: name = "Id"; break;
        case yal::ast::NodeKind::Int: name = "Int"; break;
        case yal::ast::NodeKind::String: name = "String"; break;
        case yal::ast::NodeKind::NodePack: name = "NodePack"; break;
        case yal::ast::NodeKind::FuncArg: name = "FuncArg"; break;
        case yal::ast::NodeKind::FuncNamedRet: name = "FuncNamedRet"; break;
    }

    return formatter<string_view>::format(name, ctx);
}
