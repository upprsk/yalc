#include "decl.hpp"

namespace yal {

auto DeclStore::new_decl(Location loc, std::string_view link_name,
                         std::string_view local_name) -> Decl* {
    auto d = decl_arena.create<Decl>(loc, dupe_string(link_name),
                                     dupe_string(local_name));
    decls[link_name] = d;

    return d;
}

auto DeclStore::dupe_string(std::string_view s) -> std::string_view {
    return string_arena.alloc_string_view(s);
}

auto DeclStore::get_by_link_name(std::string_view link_name) const -> Decl* {
    auto it = decls.find(link_name);
    return it != decls.end() ? it->second : nullptr;
}

}  // namespace yal
