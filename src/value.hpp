#pragma once

#include <cstdint>
#include <variant>

#include "type-id.hpp"

namespace yal {

struct Value {
    types::TypeId                                         type;
    std::variant<std::monostate, types::TypeId, uint64_t> data;

    [[nodiscard]] constexpr auto has_data() const -> bool {
        return std::holds_alternative<std::monostate>(data);
    }

    [[nodiscard]] constexpr auto get_data_type() const -> types::TypeId {
        return std::get<types::TypeId>(data);
    }

    [[nodiscard]] constexpr auto get_data_uint() const -> uint64_t {
        return std::get<uint64_t>(data);
    }
};

}  // namespace yal
