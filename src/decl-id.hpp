#pragma once

#include <cstdint>

#include "nlohmann/json_fwd.hpp"

namespace yal {
using json = nlohmann::json;

class DeclId {
    static constexpr auto const INVALID_DATA = 0xFFFF'FFFF;

    constexpr explicit DeclId(uint32_t data) : data{data} {}

public:
    /// Default initialize to invalid.
    constexpr DeclId() = default;
    constexpr DeclId(DeclId const &) = default;
    constexpr DeclId(DeclId &&) = default;
    constexpr auto operator=(DeclId const &) -> DeclId & = default;
    constexpr auto operator=(DeclId &&) -> DeclId & = default;

    // create a new invalid handle
    static constexpr auto invalid() -> DeclId { return DeclId{}; }

    // create a handle with the given value
    static constexpr auto from_raw_data(uint32_t raw_data) -> DeclId {
        return DeclId{raw_data};
    }

    constexpr auto operator==(DeclId const &o) const -> bool = default;

    // ------------

    /// Get the internal data. Make sure to use it correctly.
    [[nodiscard]] constexpr auto value() const -> uint32_t { return data; }

    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return data != INVALID_DATA;
    }

private:
    uint32_t data{INVALID_DATA};
};

void to_json(json &j, DeclId const &n);
void from_json(json const &j, DeclId &n);

}  // namespace yal
