#include "type-id.hpp"

#include "nlohmann/json.hpp"

namespace yal::types {

void to_json(json &j, TypeId const &n) {
    if (n.is_valid())
        j = n.value();
    else
        j = {};  // null
}

void from_json(json const &j, TypeId &n) {
    n = TypeId::from_raw_data(j.get<uint32_t>());
}

}  // namespace yal::types

auto fmt::formatter<yal::types::TypeId>::format(yal::types::TypeId nid,
                                                format_context    &ctx) const
    -> format_context::iterator {
    if (!nid.is_valid()) return fmt::format_to(ctx.out(), "TypeId(<invalid>)");
    return fmt::format_to(ctx.out(), "TypeId({})", nid.value());
}
