#pragma once

#include <ankerl/unordered_dense.h>

#include <flat_map>
#include <string_view>

#include "arena.hpp"
#include "location.hpp"
#include "macros.hpp"

namespace yal {

class Decl {
    Location         loc;
    std::string_view link_name;
    std::string_view local_name;

public:
    constexpr Decl(Location loc, std::string_view link_name,
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

class DeclStore {
    using map = ankerl::unordered_dense::map<std::string_view, Decl*>;

    map decls;

    mem::Arena string_arena;
    mem::Arena decl_arena;

public:
    DeclStore() = default;

    auto new_decl(Location loc, std::string_view link_name,
                  std::string_view local_name) -> Decl*;

    [[nodiscard]] auto get_by_link_name(std::string_view link_name) const
        -> Decl*;

    // ========================================================================

    struct Iter {
        map::const_iterator begin;
        map::const_iterator end;
    };

    auto iter() -> Iter {
        return {.begin = decls.cbegin(), .end = decls.cend()};
    }

private:
    auto dupe_string(std::string_view s) -> std::string_view;
};

void to_json(nlohmann::json& j, Decl const& d);

}  // namespace yal

define_formatter_from_string_view(yal::Decl);
