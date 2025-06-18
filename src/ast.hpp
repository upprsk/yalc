#pragma once

#include <cstdint>
#include <string_view>

#include "arena.hpp"
#include "location.hpp"
#include "node.hpp"

namespace yal::ast {

class Ast {
    mem::Arena node_arena;
    mem::Arena strings_arena;

public:
    auto new_node_err(Location loc) -> NodeErr* {
        return new_node<NodeErr>(loc);
    }

    auto new_node_file(Location loc, std::span<Node* const> children,
                       std::string_view module_name) -> NodeFile* {
        return new_node<NodeFile>(loc, dupe_span(children),
                                  dupe_string(module_name));
    }

    auto new_node_top_def(Location loc, Node* names, Node* types, Node* inits)
        -> NodeTopDef* {
        return new_node<NodeTopDef>(loc, names, types, inits);
    }

    auto new_node_top_var(Location loc, Node* names, Node* types, Node* inits)
        -> NodeTopVar* {
        return new_node<NodeTopVar>(loc, names, types, inits);
    }

    auto new_node_id(Location loc, std::string_view value) -> NodeId* {
        return new_node<NodeId>(loc, dupe_string(value));
    }

    auto new_node_int(Location loc, uint64_t value) -> NodeInt* {
        return new_node<NodeInt>(loc, value);
    }

    auto new_node_pack(Location loc, std::span<Node* const> children)
        -> NodePack* {
        return new_node<NodePack>(loc, dupe_span(children));
    }

    // ========================================================================

    template <typename T, typename... Args>
    auto new_node(Args&&... args) -> T* {
        return node_arena.create<T>(std::forward<Args>(args)...);
    }

    template <typename T>
    auto dupe_node(T const& n) -> T* {
        // invoke copy-constuctor
        return new_node<T>(n);
    }

    template <typename T>
    auto dupe_node(T const* n) -> T* {
        return dupe_node(*n);
    }

    auto dupe_string(std::string_view source) -> std::string_view {
        return strings_arena.alloc_string_view(source);
    }

    auto dupe_span(std::span<Node* const> nodes) -> std::span<Node*> {
        // NOTE: using the nodes arena, should use something else?
        return node_arena.alloc<Node*>(nodes);
    }
};

}  // namespace yal::ast
