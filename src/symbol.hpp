#pragma once

#include <ankerl/unordered_dense.h>

#include <flat_map>
#include <string_view>

#include "arena.hpp"
#include "location.hpp"
#include "macros.hpp"

namespace yal {

class Symbol {
    Location         loc;
    std::string_view link_name;
    std::string_view local_name;

public:
    constexpr Symbol(Location loc, std::string_view link_name,
                     std::string_view local_name)
        : loc{loc}, link_name{link_name}, local_name{local_name} {}

    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }

    [[nodiscard]] constexpr auto get_link_name() const -> std::string_view {
        return link_name;
    }

    [[nodiscard]] constexpr auto get_local_name() const -> std::string_view {
        return local_name;
    }
};

class SymbolStore {
    using map = ankerl::unordered_dense::map<std::string_view, Symbol*>;

    map syms;

    mem::Arena string_arena;
    mem::Arena sym_arena;

public:
    SymbolStore() = default;

    auto new_sym(Location loc, std::string_view link_name,
                 std::string_view local_name) -> Symbol*;

    [[nodiscard]] auto get_by_link_name(std::string_view link_name) const
        -> Symbol*;

    // ========================================================================

    struct Iter {
        map::const_iterator begin;
        map::const_iterator end;
    };

    auto iter() -> Iter { return {.begin = syms.cbegin(), .end = syms.cend()}; }

private:
    auto dupe_string(std::string_view s) -> std::string_view;
};

void to_json(nlohmann::json& j, Symbol const& d);

}  // namespace yal

define_formatter_from_string_view(yal::Symbol);
