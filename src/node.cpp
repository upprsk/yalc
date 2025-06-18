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
    auto children = json::array();
    for (auto child : get_children()) {
        json cj;
        if (child) child->to_json(cj);

        children.push_back(cj);
    }

    j = json{
        {    "kind", fmt::to_string(get_kind())},
        {     "loc",  fmt::to_string(get_loc())},
        {"children",                   children},
    };
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

auto as_node_pack(Node* n) -> NodePack* {
    auto p = dynamic_cast<NodePack*>(n);
    ASSERT(p != nullptr, "expected NodePack", n->get_kind());
    return p;
}

auto NodeTopVar::get_names() const -> NodePack* {
    return as_node_pack(child_at(0));
}

auto NodeTopVar::get_types() const -> NodePack* {
    return child_at(1) ? as_node_pack(child_at(1)) : nullptr;
}

auto NodeTopVar::get_inits() const -> NodePack* {
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
        case yal::ast::NodeKind::TopVar: name = "TopVar"; break;
        case yal::ast::NodeKind::TopDef: name = "TopDef"; break;
        case yal::ast::NodeKind::Id: name = "Id"; break;
        case yal::ast::NodeKind::Int: name = "Int"; break;
        case yal::ast::NodeKind::NodePack: name = "NodePack"; break;
    }

    return formatter<string_view>::format(name, ctx);
}
