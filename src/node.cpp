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
        {"kind", fmt::to_string(get_kind())},
        { "loc",  fmt::to_string(get_loc())},
    };
}

void Node::to_json_children(nlohmann::json& j) const {
    if (auto children = get_children(); !children.empty()) {
        j["children"] = to_json_array(children);
    }
}

// ============================================================================

#define define_as_node_kind(name, kind)                             \
    auto as_node_##name(Node* n)->Node##kind* {                     \
        if (!n) return nullptr;                                     \
        ASSERT(n->get_kind() == NodeKind::kind, "expected " #kind); \
        return static_cast<Node##kind*>(n);                         \
    }

auto as_node_pack(Node* n) -> NodePack* {
    if (!n || n->is_err()) return nullptr;

    ASSERT(n->is_node_pack(), "expected NodePack", n->get_kind(),
           fmt::to_string(n->get_kind()));
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

// ============================================================================

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
        ctx.out(), "NodeAttribute({}, {:?}, {})", get_loc(), get_name(),
        fmt::join(get_children() | rv::filter([](Node* n) {
                      return n != nullptr;
                  }) | rv::transform([](Node* n) -> Node const& { return *n; }),
                  ", "));
}

void NodeAttribute::to_json(nlohmann::json& j) const {
    Node::to_json(j);
    j["name"] = get_name();
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

auto NodeTopVar::get_attributes() const -> NodePack* {
    return as_node_pack(child_at(3));
}

auto NodeTopVar::get_names() const -> NodePack* {
    return as_node_pack(child_at(0));
}

auto NodeTopVar::get_types() const -> NodePack* {
    return as_node_pack(child_at(1));
}

auto NodeTopVar::get_inits() const -> NodePack* {
    return as_node_pack(child_at(2));
}

void NodeTopVar::to_json(nlohmann::json& j) const {
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

auto NodeTopDef::get_attributes() const -> NodePack* {
    return as_node_pack(child_at(3));
}

auto NodeTopDef::get_names() const -> NodePack* {
    return as_node_pack(child_at(0));
}

auto NodeTopDef::get_types() const -> NodePack* {
    return as_node_pack(child_at(1));
}

auto NodeTopDef::get_inits() const -> NodePack* {
    return as_node_pack(child_at(2));
}

void NodeTopDef::to_json(nlohmann::json& j) const {
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

auto fmt::formatter<yal::ast::NodeKind>::format(yal::ast::NodeKind const& p,
                                                format_context& ctx) const
    -> format_context ::iterator {
    std::string_view name = "???";
    switch (p) {
        case yal::ast::NodeKind::Err: name = "Err"; break;
        case yal::ast::NodeKind::File: name = "File"; break;
        case yal::ast::NodeKind::Attribute: name = "Attribute"; break;
        case yal::ast::NodeKind::AttributeKV: name = "AttributeKV"; break;
        case yal::ast::NodeKind::TopVar: name = "TopVar"; break;
        case yal::ast::NodeKind::TopDef: name = "TopDef"; break;
        case yal::ast::NodeKind::Func: name = "Func"; break;
        case yal::ast::NodeKind::Block: name = "Block"; break;
        case yal::ast::NodeKind::Return: name = "Return"; break;
        case yal::ast::NodeKind::ExprStmt: name = "ExprStmt"; break;
        case yal::ast::NodeKind::Id: name = "Id"; break;
        case yal::ast::NodeKind::Int: name = "Int"; break;
        case yal::ast::NodeKind::String: name = "String"; break;
        case yal::ast::NodeKind::NodePack: name = "NodePack"; break;
        case yal::ast::NodeKind::FuncArg: name = "FuncArg"; break;
    }

    return formatter<string_view>::format(name, ctx);
}
