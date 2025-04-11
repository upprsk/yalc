#include "ast-node.hpp"

#include <string_view>

#include "file-store.hpp"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "nlohmann/json.hpp"

namespace yal::ast {

void to_json(json& j, Node const& n) {
    j = json{
        {"kind", fmt::to_string(n.get_kind())},

        // NOTE: converting the location to a string. This is not the ideal, but
        // very convenient for debugging
        // { "loc",  fmt::to_string(n.get_loc())},
        { "loc",                  n.get_loc()},
    };

    if (n.get_declid().is_valid()) {
        j["declid"] = n.get_declid();
    }

    if (std::holds_alternative<std::monostate>(n.get_data()))
        j["data"] = {};
    else if (std::holds_alternative<float>(n.get_data()))
        j["data"] = std::get<float>(n.get_data());
    else if (std::holds_alternative<double>(n.get_data()))
        j["data"] = std::get<double>(n.get_data());
    else if (std::holds_alternative<uint64_t>(n.get_data()))
        j["data"] = std::get<uint64_t>(n.get_data());
    else if (std::holds_alternative<std::string_view>(n.get_data()))
        j["data"] = std::get<std::string_view>(n.get_data());
    else
        PANIC("invalid data in AST node");

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

    if (std::holds_alternative<std::monostate>(n.get_data()))
        fmt::format_to(ctx.out(), "<empty>");
    else if (std::holds_alternative<float>(n.get_data()))
        fmt::format_to(ctx.out(), "{}", n.get_data_f32());
    else if (std::holds_alternative<double>(n.get_data()))
        fmt::format_to(ctx.out(), "{}", n.get_data_f64());
    else if (std::holds_alternative<uint64_t>(n.get_data()))
        fmt::format_to(ctx.out(), "{}", n.get_data_u64());
    else if (std::holds_alternative<std::string_view>(n.get_data()))
        fmt::format_to(ctx.out(), "{:?}", n.get_data_str());
    else
        fmt::format_to(ctx.out(), "<invalid>");

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
