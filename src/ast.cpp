#include "ast.hpp"

#include <nlohmann/json.hpp>

#include "node.hpp"

namespace yal::ast {

using nlohmann::json;

void to_json(nlohmann::json& j, File const& n) {
    j = json{
        {    "name",     n.get_module_name()},
        {"name_loc", n.get_module_name_loc()},
    };

    auto arr = json::array();
    for (auto const& c : n.get_declarations()) {
        if (c)
            arr.push_back(*c);
        else
            arr.push_back(json{});
    }

    j["decls"] = arr;
}

void to_json(nlohmann::json& j, Module const& n) {
    j = json{
        {"name", n.name}
    };

    auto arr = json::array();
    for (auto const& f : n.files) {
        arr.push_back(f);
    }

    j["files"] = arr;
}

void to_json(nlohmann::json& j, FlatModule const& n) {
    j = json{
        {"name", n.name},
    };

    auto arr = json::array();
    for (auto const& c : n.declarations) {
        if (c)
            arr.push_back(*c);
        else
            arr.push_back(json{});
    }

    j["decls"] = arr;
}

// NOTE: in node.cpp
void indent_by(fmt::format_context& ctx, int depth);
void indent_by_wln(fmt::format_context& ctx, int depth);

}  // namespace yal::ast

auto fmt::formatter<yal::ast::FlatModule>::format(yal::ast::FlatModule const& p,
                                                  format_context& ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "(FlatModule {:?}", p.name);

    for (auto const& d : p.declarations) {
        yal::ast::indent_by_wln(ctx, 1);
        yal::ast::to_lisp(ctx, d, 1);
    }

    return fmt::format_to(ctx.out(), ")");
}
