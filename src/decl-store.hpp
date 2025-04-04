#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "ast-node-id.hpp"
#include "decl-id.hpp"

namespace yal {

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
