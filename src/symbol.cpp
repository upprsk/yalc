#include "symbol.hpp"

#include <nlohmann/json.hpp>

#include "fmt/format.h"

namespace yal {
using nlohmann::json;

auto SymbolStore::new_sym(Location loc, std::string_view link_name,
                          std::string_view local_name) -> Symbol* {
    auto d = sym_arena.create<Symbol>(loc, dupe_string(link_name),
                                      dupe_string(local_name));
    syms[link_name] = d;

    return d;
}

auto SymbolStore::dupe_string(std::string_view s) -> std::string_view {
    return string_arena.alloc_string_view(s);
}

auto SymbolStore::get_by_link_name(std::string_view link_name) const
    -> Symbol* {
    auto it = syms.find(link_name);
    return it != syms.end() ? it->second : nullptr;
}

void to_json(nlohmann::json& j, Symbol const& d) {
    j = json{
        {       "loc", fmt::to_string(d.get_loc())},
        { "link_name",           d.get_link_name()},
        {"local_name",          d.get_local_name()},
    };
}

}  // namespace yal

auto fmt::formatter<yal::Symbol>::format(yal ::Symbol const& p,
                                         format_context&     ctx) const
    -> format_context ::iterator {
    return fmt::format_to(ctx.out(), "{}", nlohmann::json{p}.dump());
}
