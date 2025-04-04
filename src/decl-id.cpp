#include "decl-id.hpp"

#include "nlohmann/json.hpp"

namespace yal {

void to_json(json &j, DeclId const &n) {
    if (n.is_valid())
        j = n.value();
    else
        j = {};  // null
}

void from_json(json const &j, DeclId &n) {
    n = DeclId::from_raw_data(j.get<uint32_t>());
}

}  // namespace yal
