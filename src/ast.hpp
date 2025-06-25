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

    auto new_attribute(Location loc, std::string_view name,
                       std::span<Node* const> args) -> NodeAttribute* {
        return new_node<NodeAttribute>(loc, dupe_string(name), dupe_span(args));
    }

    auto new_attributekv(Location loc, std::string_view key, Node* value)
        -> NodeAttributeKV* {
        return new_node<NodeAttributeKV>(loc, dupe_string(key), value);
    }

    auto new_node_top_def(Location loc, Node* attributes, Node* names,
                          Node* types, Node* inits) -> NodeTopDef* {
        return new_node<NodeTopDef>(loc, attributes, names, types, inits);
    }

    auto new_node_top_var(Location loc, Node* attributes, Node* names,
                          Node* types, Node* inits) -> NodeTopVar* {
        return new_node<NodeTopVar>(loc, attributes, names, types, inits);
    }

    auto new_node_func(Location loc, Node* attributes, std::string_view name,
                       std::string_view attached_type, Node* gargs, Node* args,
                       Node* ret, Node* body, bool is_c_varargs) -> NodeFunc* {
        return new_node<NodeFunc>(loc, attributes, name, attached_type, gargs,
                                  args, ret, body, is_c_varargs);
    }

    auto new_node_block(Location loc, std::span<Node* const> children)
        -> NodeBlock* {
        return new_node<NodeBlock>(loc, dupe_span(children));
    }

    auto new_node_return(Location loc, std::span<Node* const> values)
        -> NodeReturn* {
        return new_node<NodeReturn>(loc, dupe_span(values));
    }

    auto new_node_expr_stmt(Location loc, Node* child) -> NodeExprStmt* {
        return new_node<NodeExprStmt>(loc, child);
    }

    auto new_node_def(Location loc, Node* names, Node* types, Node* inits)
        -> NodeDef* {
        return new_node<NodeDef>(loc, names, types, inits);
    }

    auto new_node_var(Location loc, Node* names, Node* types, Node* inits)
        -> NodeVar* {
        return new_node<NodeVar>(loc, names, types, inits);
    }

    auto new_node_id(Location loc, std::string_view value) -> NodeId* {
        return new_node<NodeId>(loc, dupe_string(value));
    }

    auto new_node_int(Location loc, uint64_t value) -> NodeInt* {
        return new_node<NodeInt>(loc, value);
    }

    auto new_node_string(Location loc, std::string_view value) -> NodeString* {
        return new_node<NodeString>(loc, dupe_string(value));
    }

    auto new_node_func_arg(Location loc, std::string_view name, Node* type)
        -> NodeFuncArg* {
        return new_node<NodeFuncArg>(loc, dupe_string(name), type);
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
