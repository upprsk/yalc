#include "ast-node.hpp"

#include <string_view>

#include "decl-store.hpp"
#include "file-store.hpp"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"

namespace yal::ast {

void Node::transmute_to_unscoped_group(
    std::span<Node*> allocated_unscoped_items) {
    kind = NodeKind::UnscopedGroup;
    children = allocated_unscoped_items;

    ASSERT(decl == nullptr);
    ASSERT(type != nullptr);
    ASSERT(type->is_void());
    ASSERT(std::holds_alternative<std::monostate>(data));
}

void to_json(json& j, Node const& n) {
    j = json{
        {"kind", fmt::to_string(n.get_kind())},

        // NOTE: converting the location to a string. This is not the ideal, but
        // very convenient for debugging
        // { "loc",  fmt::to_string(n.get_loc())},
        { "loc",                  n.get_loc()},
    };

    if (n.get_decl()) {
        j["decl"] = n.get_decl()->uid;
    }

    if (n.get_type()) {
        j["type"] = fmt::to_string(*n.get_type());
    }

    if (std::holds_alternative<std::monostate>(n.get_data())) {
        j["data"] = {};
    } else if (std::holds_alternative<float>(n.get_data())) {
        j["data"] = n.get_data_f32();
    } else if (std::holds_alternative<double>(n.get_data())) {
        j["data"] = n.get_data_f64();
    } else if (std::holds_alternative<uint64_t>(n.get_data())) {
        j["data"] = n.get_data_u64();
    } else if (std::holds_alternative<std::string_view>(n.get_data())) {
        j["data"] = n.get_data_str();
    } else if (std::holds_alternative<yal::types::Type*>(n.get_data())) {
        ASSERT(n.get_data_type() != nullptr);
        j["data"] = fmt::to_string(*n.get_data_type());
        // j["data"] = *n.get_data_type();
    } else {
        PANIC("invalid data in AST node");
    }

    auto arr = json::array();
    for (auto child : n.get_children()) {
        if (child)
            arr.push_back(*child);
        else
            arr.push_back({});
    }

    j["children"] = arr;
}

}  // namespace yal::ast

auto fmt::formatter<yal::ast::Node>::format(yal::ast::Node const& n,
                                            format_context&       ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "Node({}, {}, ", n.get_kind(), n.get_loc());

    if (std::holds_alternative<std::monostate>(n.get_data())) {
        fmt::format_to(ctx.out(), "<empty>");
    } else if (std::holds_alternative<float>(n.get_data())) {
        fmt::format_to(ctx.out(), "{}", n.get_data_f32());
    } else if (std::holds_alternative<double>(n.get_data())) {
        fmt::format_to(ctx.out(), "{}", n.get_data_f64());
    } else if (std::holds_alternative<uint64_t>(n.get_data())) {
        fmt::format_to(ctx.out(), "{}", n.get_data_u64());
    } else if (std::holds_alternative<std::string_view>(n.get_data())) {
        fmt::format_to(ctx.out(), "{:?}", n.get_data_str());
    } else if (std::holds_alternative<yal::types::Type*>(n.get_data())) {
        ASSERT(n.get_data_type() != nullptr);
        fmt::format_to(ctx.out(), "{}", *n.get_data_type());
    } else {
        fmt::format_to(ctx.out(), "<invalid>");
    }

    fmt::format_to(ctx.out(), ", [");

    for (size_t i{}; auto c : n.get_children()) {
        if (i != 0) fmt::format_to(ctx.out(), ", ");
        if (c != nullptr)
            fmt::format_to(ctx.out(), "{}", *c);
        else
            fmt::format_to(ctx.out(), "null");

        i++;
    }

    return fmt::format_to(ctx.out(), "])");
}
