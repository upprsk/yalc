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

auto Node::format_to(fmt::format_context& ctx) const
    -> fmt::format_context::iterator {
    return fmt::format_to(
        ctx.out(), "Node({}, {}, {})", kind, loc,
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
    auto children = json::array();
    for (auto child : get_children()) {
        json cj;
        if (child) child->to_json(cj);

        children.push_back(cj);
    }

    if (!children.empty()) j["children"] = std::move(children);
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

auto as_node_pack(Node* n) -> NodePack* {
    ASSERT(n->is_node_pack(), "expected NodePack", n->get_kind());
    return static_cast<NodePack*>(n);
}

auto NodeTopVar::get_attributes() const -> NodePack* {
    return child_at(3) ? as_node_pack(child_at(3)) : nullptr;
}

auto NodeTopVar::get_names() const -> NodePack* {
    return child_at(0) ? as_node_pack(child_at(0)) : nullptr;
}

auto NodeTopVar::get_types() const -> NodePack* {
    return child_at(1) ? as_node_pack(child_at(1)) : nullptr;
}

auto NodeTopVar::get_inits() const -> NodePack* {
    return child_at(2) ? as_node_pack(child_at(2)) : nullptr;
}

auto NodeTopDef::get_attributes() const -> NodePack* {
    return child_at(3) ? as_node_pack(child_at(3)) : nullptr;
}

auto NodeTopDef::get_names() const -> NodePack* {
    return child_at(0) ? as_node_pack(child_at(0)) : nullptr;
}

auto NodeTopDef::get_types() const -> NodePack* {
    return child_at(1) ? as_node_pack(child_at(1)) : nullptr;
}

auto NodeTopDef::get_inits() const -> NodePack* {
    return child_at(2) ? as_node_pack(child_at(2)) : nullptr;
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
        case yal::ast::NodeKind::Id: name = "Id"; break;
        case yal::ast::NodeKind::Int: name = "Int"; break;
        case yal::ast::NodeKind::NodePack: name = "NodePack"; break;
    }

    return formatter<string_view>::format(name, ctx);
}
