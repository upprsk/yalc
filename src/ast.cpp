#include "ast.hpp"

#include <nlohmann/json.hpp>

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
}  // namespace yal::ast
