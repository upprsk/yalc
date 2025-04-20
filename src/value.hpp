#pragma once

#include <cstdint>
#include <variant>

#include "types.hpp"

namespace yal {

struct Value {
    types::Type*                                         type;
    std::variant<std::monostate, types::Type*, uint64_t> data;

    [[nodiscard]] constexpr auto has_data() const -> bool {
        return std::holds_alternative<std::monostate>(data);
    }

    [[nodiscard]] constexpr auto get_data_type() const -> types::Type* {
        return std::get<types::Type*>(data);
    }

    [[nodiscard]] constexpr auto get_data_uint() const -> uint64_t {
        return std::get<uint64_t>(data);
    }
};

}  // namespace yal
