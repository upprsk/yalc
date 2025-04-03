#pragma once

#include <cstdint>

#include "file-store.hpp"
#include "nlohmann/json_fwd.hpp"

namespace yal::ast {
using json = nlohmann::json;

struct NodeIdOfRef {
    uint32_t value;
};

struct NodeIdOfArray {
    uint32_t value;
};

struct NodeIdOfCount {
    uint32_t value;

    // the array points to key-values (so it needs to be doubled in size).
    [[nodiscard]] constexpr auto of_kv() const -> NodeIdOfCount {
        return {value * 2};
    }
};

struct NodeIdOfId {
    uint32_t value;
};

struct NodeIdOfBytes {
    uint32_t value;
};

struct NodeIdOfFile {
    FileId value;
};

/// A handle to an AST node. It is a "stable pointer" that is also half the size
/// on 64bits.
///
/// A AstNodeHandle may point to many things:
///
/// - Another AST node directly.
/// - An array of AST nodes (in the `refs` field in the AST).
/// - A count (for how many nodes in the array for example).
/// - An identifier.
/// - A file id in the file store.
/// - Raw bytes (for strings and such).
/// - An arbitrary 64bit integer by joining 2 handles.
/// - An arbitrary 64bit floating point number by joining 2 handles.
/// - An arbitrary 32bit floating point number.
///
/// The special value of `0xFFFF_FFFF` is used for marking invalid (or
/// uninitialized) handles.
///
/// TODO: introduce some sort of "generation" to the data to allow handle reuse
/// with safety.
class NodeId {
    static constexpr auto const INVALID_DATA = 0xFFFF'FFFF;

    constexpr explicit NodeId(uint32_t data) : data{data} {}

public:
    // ------------
    // Constructors
    // ------------

    /// Default initialize to invalid.
    constexpr NodeId() = default;
    constexpr NodeId(NodeId const &) = default;
    constexpr NodeId(NodeId &&) = default;
    constexpr auto operator=(NodeId const &) -> NodeId & = default;
    constexpr auto operator=(NodeId &&) -> NodeId & = default;

    // create a new invalid handle
    static constexpr auto invalid() -> NodeId { return NodeId{}; }

    // create a handle with the given value
    static constexpr auto from_raw_data(uint32_t raw_data) -> NodeId {
        return NodeId{raw_data};
    }

    constexpr auto operator==(NodeId const &o) const -> bool = default;

    // ------------

    // get a typed representation of the handle.

    // clang-format off
    [[nodiscard]] constexpr auto as_ref() const -> NodeIdOfRef { return {data}; }
    [[nodiscard]] constexpr auto as_array() const -> NodeIdOfArray { return {data}; }
    [[nodiscard]] constexpr auto as_count() const -> NodeIdOfCount { return {data}; }
    [[nodiscard]] constexpr auto as_id() const -> NodeIdOfId { return {data}; }
    [[nodiscard]] constexpr auto as_file() const -> NodeIdOfFile { return {FileId::from_raw_data(data)}; }
    [[nodiscard]] constexpr auto as_bytes() const -> NodeIdOfBytes { return {data}; }
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

void to_json(json &j, NodeId const &n);
void from_json(json const &j, NodeId &n);

}  // namespace yal::ast

template <>
struct fmt::formatter<yal::ast::NodeId> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ast::NodeId nid, format_context &ctx) const
        -> format_context::iterator;
};

template <>
struct std::hash<yal::ast::NodeId> {
    auto operator()(yal::ast::NodeId const &k) const -> std::size_t {
        return std::hash<uint32_t>{}(k.value());
    }
};
