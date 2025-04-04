#include "ast-node-id.hpp"

#include <cstdint>

#include "nlohmann/json.hpp"

namespace yal::ast {

void to_json(json &j, NodeId const &n) {
    if (n.is_valid())
        j = n.value();
    else
        j = {};  // null
}

void from_json(json const &j, NodeId &n) {
    n = NodeId::from_raw_data(j.get<uint32_t>());
}

}  // namespace yal::ast

auto fmt::formatter<yal::ast::NodeId>::format(yal::ast::NodeId nid,
                                              format_context  &ctx) const
    -> format_context::iterator {
    if (!nid.is_valid()) return fmt::format_to(ctx.out(), "NodeId(<invalid>)");
    return fmt::format_to(ctx.out(), "NodeId({})", nid.value());
}
