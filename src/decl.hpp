#pragma once

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
};

class DeclStore {
    std::flat_map<std::string_view, Decl*> decls;

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
        std::flat_map<std::string_view, Decl*>::const_iterator begin;
        std::flat_map<std::string_view, Decl*>::const_iterator end;
    };

    auto iter() -> Iter {
        return {.begin = decls.cbegin(), .end = decls.cend()};
    }

private:
    auto dupe_string(std::string_view s) -> std::string_view;
};

}  // namespace yal

define_formatter_from_string_view(yal::Decl);
