#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "nlohmann/json_fwd.hpp"
#include "types.hpp"

namespace yal {
using json = nlohmann::json;

struct Value {
    types::Type* type{};
    std::variant<std::monostate, types::Type*, bool, uint64_t, std::string_view>
        data = std::monostate{};

    [[nodiscard]] constexpr auto has_data() const -> bool {
        return !std::holds_alternative<std::monostate>(data);
    }

    [[nodiscard]] constexpr auto get_data_type() const -> types::Type* {
        return std::get<types::Type*>(data);
    }

    [[nodiscard]] constexpr auto get_data_bool() const -> bool {
        return std::get<bool>(data);
    }

    [[nodiscard]] constexpr auto get_data_uint() const -> uint64_t {
        return std::get<uint64_t>(data);
    }

    [[nodiscard]] constexpr auto get_data_strv() const -> std::string_view {
        return std::get<std::string_view>(data);
    }
};

void to_json(json& j, Value const& v);

}  // namespace yal

template <>
struct fmt::formatter<yal::Value> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::Value const& v, format_context& ctx) const
        -> format_context::iterator;
};
