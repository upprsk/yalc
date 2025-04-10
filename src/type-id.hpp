#pragma once

#include <cstdint>

#include "fmt/base.h"
#include "nlohmann/json_fwd.hpp"

namespace yal::types {
using json = nlohmann::json;

struct TypeIdOfRef {
    uint32_t value;
};

struct TypeIdOfArray {
    uint32_t value;
};

struct TypeIdOfCount {
    uint32_t value;

    // the array points to key-values (so it needs to be doubled in size).
    [[nodiscard]] constexpr auto of_kv() const -> TypeIdOfCount {
        return {value * 2};
    }
};

struct TypeIdOfId {
    uint32_t value;
};

/// A handle to a type. It is a "stable pointer" that is also half the size on
/// 64bits.
///
/// A TypeId may point to many things:
///
/// - Another type directly.
/// - An array of types (in the `refs` field in the type store).
/// - A count (for how many types in the array for example).
/// - An identifier/name (mainly for structs).
///
/// The special value of `0xFFFF_FFFF` is used for marking invalid (or
/// uninitialized) handles.
///
/// TODO: introduce some sort of "generation" to the data to allow handle reuse
/// with safety.
class TypeId {
    static constexpr auto const INVALID_DATA = 0xFFFF'FFFF;

    constexpr explicit TypeId(uint32_t data) : data{data} {}

public:
    // ------------
    // Constructors
    // ------------

    /// Default initialize to invalid.
    constexpr TypeId() = default;
    constexpr TypeId(TypeId const &) = default;
    constexpr TypeId(TypeId &&) = default;
    constexpr auto operator=(TypeId const &) -> TypeId & = default;
    constexpr auto operator=(TypeId &&) -> TypeId & = default;

    // create a new invalid handle
    static constexpr auto invalid() -> TypeId { return TypeId{}; }

    // create a handle with the given value
    static constexpr auto from_raw_data(uint32_t raw_data) -> TypeId {
        return TypeId{raw_data};
    }

    constexpr auto operator==(TypeId const &o) const -> bool = default;

    // ------------

    // get a typed representation of the handle.

    // clang-format off
    [[nodiscard]] constexpr auto as_ref() const -> TypeIdOfRef { return {data}; }
    [[nodiscard]] constexpr auto as_array() const -> TypeIdOfArray { return {data}; }
    [[nodiscard]] constexpr auto as_count() const -> TypeIdOfCount { return {data}; }
    [[nodiscard]] constexpr auto as_id() const -> TypeIdOfId { return {data}; }
    // clang-format on

    // ------------

    /// Get the internal data. Make sure to use it correctly.
    [[nodiscard]] constexpr auto value() const -> uint32_t { return data; }

    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return data != INVALID_DATA;
    }

private:
    uint32_t data{INVALID_DATA};
};

void to_json(json &j, TypeId const &n);
void from_json(json const &j, TypeId &n);

}  // namespace yal::types

template <>
struct fmt::formatter<yal::types::TypeId> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::types::TypeId nid, format_context &ctx) const
        -> format_context::iterator;
};

template <>
struct std::hash<yal::types::TypeId> {
    auto operator()(yal::types::TypeId const &k) const -> std::size_t {
        return std::hash<uint32_t>{}(k.value());
    }
};
