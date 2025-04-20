#include "decl-store.hpp"

#include <ranges>

#include "nlohmann/json.hpp"

namespace yal {

auto DeclStore::new_decl(std::string_view full_name,
                         std::string_view local_name,
                         std::string_view link_name, ast::Node* declarator,
                         DeclFlags flags, Value value) -> Decl* {
    auto d =
        decls.create<DeclItem>(head, Decl{.uid = next_uid++,
                                          .full_name = new_string(full_name),
                                          .local_name = new_string(local_name),
                                          .link_name = new_string(link_name),
                                          .node = declarator,
                                          .flags = flags,
                                          .value = value});

    // fmt::println(stderr,
    //              "new_decl(full_name={:?}, local_name={:?}, link_name={:?}, "
    //              "flags={})",
    //              d->decl.full_name, d->decl.local_name, d->decl.link_name,
    //              d->decl.flags);

    head = d;
    return &d->decl;
}

void to_json(json& j, DeclFlags const& n) {
    auto arr = json::array();

    if (n.has_extern()) arr.push_back("extern");
    if (n.has_private()) arr.push_back("private");
    if (n.has_private_file()) arr.push_back("private-file");

    j = arr;
}

void to_json(json& j, Decl const& n) {
    j = json{
        {       "uid",        n.uid},
        {      "name",  n.full_name},
        {"local_name", n.local_name},
        { "link_name",  n.link_name},
        // {      "node",       n.node},
        {     "flags",      n.flags},
    };
}

void to_json(json& j, DeclStore const& n) {
    auto arr = json::array();
    for (auto const& d : n) {
        arr.push_back(d);
    }

    j["decls"] = arr | std::ranges::views::reverse;
}

}  // namespace yal

auto fmt::formatter<yal::Decl>::format(yal::Decl const& d,
                                       format_context&  ctx) const
    -> format_context::iterator {
    // FIXME: add value
    return fmt::format_to(ctx.out(), "Decl({:?}, {:?}, {:?}, {})", d.full_name,
                          d.local_name, d.link_name, d.flags);
}

auto fmt::formatter<yal::DeclFlags>::format(yal::DeclFlags  flags,
                                            format_context& ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "DeclFlags(");
    if (flags.has_extern()) fmt::format_to(ctx.out(), "extern,");
    if (flags.has_private()) fmt::format_to(ctx.out(), "private,");
    if (flags.has_private_file()) fmt::format_to(ctx.out(), "private_file");

    return fmt::format_to(ctx.out(), ")");
}
