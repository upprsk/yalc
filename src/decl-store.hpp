#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "ast-node-id.hpp"

namespace yal {

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

struct DeclFlags {
    enum Flags {
        None = 0,
        Extern = 1 << 0,
        Private = 2 << 0,
        PrivateFile = 3 << 0,
    };
};

class Decl {
public:
    /// This is the name that the declaration has globally. This prepends the
    /// names of all scopes above it an joins them with '.'.
    std::string name;

    /// This is the name that the declaration has locally. This is what is used
    /// by lexical scoping.
    std::string local_name;

    /// The node that declared this. It may be an invalid id if the value is a
    /// buitin or something that does not come from the AST.
    ast::NodeId node;

    /// Flags about the declaration. These further describe properties of the
    /// declaration, like if it is external, private, private(file), etc...
    DeclFlags flags;

private:
};

class DeclStore {
public:
    auto gen_decl(Decl decl) -> DeclId {
        auto sz = decls.size();
        decls.push_back(decl);
        return DeclId::from_raw_data(sz);
    }

    [[nodiscard]] auto get_decl(DeclId id) const -> Decl const & {
        return decls.at(id.value());
    }

private:
    std::vector<Decl> decls;
};

}  // namespace yal

template <>
struct fmt::formatter<yal::DeclId> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::DeclId did, format_context &ctx) const
        -> format_context::iterator;
};

template <>
struct std::hash<yal::DeclId> {
    auto operator()(yal::DeclId const &k) const -> std::size_t {
        return std::hash<uint32_t>{}(k.value());
    }
};
