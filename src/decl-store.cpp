#include "decl-store.hpp"

#include "nlohmann/json.hpp"

namespace yal {

void to_json(json& j, DeclFlags const& n) {
    j = json::array();

    if (n.flags & DeclFlags::Extern) j.push_back("extern");
    if (n.flags & DeclFlags::Private) j.push_back("private");
    if (n.flags & DeclFlags::PrivateFile) j.push_back("private-file");
}

void to_json(json& j, Decl const& n) {
    j = json{
        {        "id",         n.id},
        {      "name",       n.name},
        {"local_name", n.local_name},
        {      "node",       n.node},
        {     "flags",      n.flags},
    };
}

void to_json(json& j, DeclStore const& n) { j["decls"] = n.get_all_decls(); }

}  // namespace yal

auto fmt::formatter<yal::DeclId>::format(yal::DeclId     did,
                                         format_context& ctx) const
    -> format_context::iterator {
    if (!did.is_valid()) return fmt::format_to(ctx.out(), "DeclId(<invalid>)");
    return fmt::format_to(ctx.out(), "DeclId({})", did.value());
}
