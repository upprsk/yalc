#pragma once

#include <cstddef>
#include <string_view>

#include "ast-node-id.hpp"
#include "ast.hpp"

namespace yal::ast::conv {

struct Module {
    std::string_view        name;
    std::span<NodeId const> children;
};

struct SourceFile {
    NodeId                  mod;
    std::span<NodeId const> children;
};

struct SourceFileMut {
    NodeId            mod;
    std::span<NodeId> children;
};

struct ModuleDecl {
    std::string_view name;
};

struct FuncDecl {
    std::span<NodeId const> decorators;
    std::span<NodeId const> gargs;
    std::span<NodeId const> args;
    NodeId                  name;
    NodeId                  ret;
    NodeId                  body;
    bool                    is_c_varargs;
};

struct TopVarDecl {
    std::span<NodeId const> decorators;
    NodeId                  decl;
};

struct TopDefDecl {
    std::span<NodeId const> decorators;
    NodeId                  decl;
};

struct IdPack {
    // NOTE: each id in `ids` points to an identifier, not an AST node
    std::span<NodeId const> ids;
};

struct FuncParam {
    std::string_view name;
    NodeId           type;
};

struct RetPack {
    // NOTE: each id in `ret` points to either an identifier or an AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs of return value name and type. The name may be an invalid
    // id, for when the return value is not named.
    std::span<NodeId const> ret;
};

struct Decorator {
    // NOTE: each id in `params` points to either an identifier or an AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs. The name may be an invalid id, for when a raw value is
    // passed. The value may be an invalid id, for when no value is passed.
    std::span<NodeId const> params;
    std::string_view        name;
};

struct ImportStmt {
    std::string_view path;
};

struct ExprPack {
    std::span<NodeId const> items;
};

struct Binary {
    NodeId lhs;
    NodeId rhs;
};

struct Unary {
    NodeId child;
};

struct StructType {
    std::span<NodeId const> fields;
};

struct StructField {
    std::string_view name;
    NodeId           type;
    NodeId           init;
};

struct Ptr {
    NodeId inner;
    bool   is_const;
};

struct MultiPtr {
    NodeId inner;
    bool   is_const;
};

struct Slice {
    NodeId inner;
    bool   is_const;
};

struct ArrayType {
    NodeId size;
    NodeId inner;
    bool   is_const;
};

struct Array {
    NodeId                  size;
    NodeId                  inner;
    std::span<NodeId const> items;
};

struct Lit {
    // NOTE: each id in `params` points to either the key or the value AST node.
    // This depends if it is in an even or odd index, as the list contains
    // key-value pairs. The key may be an invalid id, for when a value is given
    // without a key.
    std::span<NodeId const> items;
};

struct Call {
    NodeId                  callee;
    std::span<NodeId const> args;
};

struct Field {
    std::string_view name;
    NodeId           receiver;
};

struct Block {
    std::span<NodeId const> items;
};

struct IfStmt {
    NodeId cond;
    NodeId wt;
    // NOTE: `wf` may be an invalid id in case the if does not have an else
    NodeId wf;
};

struct WhileStmt {
    NodeId cond;
    NodeId body;
};

struct DeferStmt {
    NodeId stmt;
};

struct VarDecl {
    NodeId ids;
    NodeId types;
    NodeId inits;
};

struct DefDecl {
    NodeId ids;
    NodeId types;
    NodeId inits;
};

struct Assign {
    NodeId lhs;
    NodeId rhs;
};

// ------------------------------------------------------------------

[[nodiscard]] constexpr auto module(Ast const& ast, Node const& n) -> Module {
    return {
        .name = ast.get_identifier(n.get_first().as_id()),
        .children = ast.get_array(n.get_second().as_array()),
    };
}

[[nodiscard]] constexpr auto source_file(Ast const& ast, Node const& n)
    -> SourceFile {
    return {
        .mod = n.get_first(),
        .children = ast.get_array(n.get_second().as_array()),
    };
}

[[nodiscard]] constexpr auto source_file_mut(Ast& ast, Node const& n)
    -> SourceFileMut {
    return {
        .mod = n.get_first(),
        .children = ast.get_array_mut(n.get_second().as_array()),
    };
}

[[nodiscard]] constexpr auto module_decl(Ast const& ast, Node const& n)
    -> ModuleDecl {
    return {.name = ast.get_identifier(n.get_first().as_id())};
}

[[nodiscard]] constexpr auto func_decl(Ast const& ast, Node const& n)
    -> FuncDecl {
    auto     second = ast.get_array_unbounded(n.get_second().as_array());
    uint32_t idx{};

    // NOTE: no bounds check because of span missing the `.at`
    // method. :(
    auto dlen = second[idx++].as_count().value;
    auto decorators = second.subspan(idx, dlen);
    idx += dlen;

    auto glen = second[idx++].as_count().value;
    auto gargs = second.subspan(idx, glen);
    idx += glen;

    auto alen = second[idx++].as_count().value;
    auto args = second.subspan(idx, alen);
    idx += alen;

    auto ret = second[idx++];
    auto body = second[idx++];

    return {
        .decorators = decorators,
        .gargs = gargs,
        .args = args,
        .name = n.get_first(),
        .ret = ret,
        .body = body,
        .is_c_varargs = n.get_kind() == NodeKind::FuncDeclWithCVarArgs,
    };
}

[[nodiscard]] constexpr auto top_var_decl(Ast const& ast, Node const& n)
    -> TopVarDecl {
    return {
        .decorators = ast.get_array(n.get_second().as_array()),
        .decl = n.get_first(),
    };
}

[[nodiscard]] constexpr auto top_def_decl(Ast const& ast, Node const& n)
    -> TopDefDecl {
    return {
        .decorators = ast.get_array(n.get_second().as_array()),
        .decl = n.get_first(),
    };
}

[[nodiscard]] constexpr auto id_pack(Ast const& ast, Node const& n) -> IdPack {
    return {.ids = ast.get_array(n.get_first().as_count(),
                                 n.get_second().as_array())};
}

[[nodiscard]] constexpr auto func_param(Ast const& ast, Node const& n)
    -> FuncParam {
    return {.name = ast.get_identifier(n.get_first().as_id()),
            .type = n.get_second()};
}

[[nodiscard]] constexpr auto ret_pack(Ast const& ast, Node const& n)
    -> RetPack {
    return {.ret = ast.get_array(n.get_first().as_count().of_kv(),
                                 n.get_second().as_array())};
}

[[nodiscard]] constexpr auto decorator(Ast const& ast, Node const& n)
    -> Decorator {
    return {
        .params = ast.get_array_of_kv(n.get_second().as_array()),
        .name = ast.get_identifier(n.get_first().as_id()),
    };
}

[[nodiscard]] constexpr auto import_stmt(Ast const& ast, Node const& n)
    -> ImportStmt {
    return {.path = ast.get_bytes_as_string_view(n.get_first().as_bytes())};
}

[[nodiscard]] constexpr auto expr_pack(Ast const& ast, Node const& n)
    -> ExprPack {
    return {.items = ast.get_array(n.get_first().as_count(),
                                   n.get_second().as_array())};
}

[[nodiscard]] constexpr auto binary(Ast const& /*unused*/, Node const& n)
    -> Binary {
    return {.lhs = n.get_first(), .rhs = n.get_second()};
}

[[nodiscard]] constexpr auto unary(Ast const& /*unused*/, Node const& n)
    -> Unary {
    return {.child = n.get_first()};
}

[[nodiscard]] constexpr auto struct_type(Ast const& ast, Node const& n)
    -> StructType {
    return {.fields = ast.get_array(n.get_first().as_count(),
                                    n.get_second().as_array())};
}

[[nodiscard]] constexpr auto struct_field(Ast const& ast, Node const& n)
    -> StructField {
    auto parts = ast.get_array({2}, n.get_second().as_array());
    return {
        .name = ast.get_identifier(n.get_first().as_id()),
        .type = parts[0],
        .init = parts[1],
    };
}

[[nodiscard]] constexpr auto ptr(Ast const& /*unused*/, Node const& n) -> Ptr {
    return {
        .inner = n.get_first(),
        .is_const = n.get_kind() == NodeKind::PtrConst,
    };
}

[[nodiscard]] constexpr auto mptr(Ast const& /*unused*/, Node const& n)
    -> MultiPtr {
    return {
        .inner = n.get_first(),
        .is_const = n.get_kind() == NodeKind::MultiPtrConst,
    };
}

[[nodiscard]] constexpr auto slice(Ast const& /*unused*/, Node const& n)
    -> Slice {
    return {
        .inner = n.get_first(),
        .is_const = n.get_kind() == NodeKind::SliceConst,
    };
}

[[nodiscard]] constexpr auto array_type(Ast const& /*unused*/, Node const& n)
    -> ArrayType {
    return {
        .size = n.get_second(),
        .inner = n.get_first(),
        .is_const = n.get_kind() == NodeKind::ArrayTypeConst,
    };
}

[[nodiscard]] constexpr auto array(Ast const& ast, Node const& n) -> Array {
    auto second = ast.get_array_unbounded(n.get_second().as_array());
    auto size = second[0];
    auto items = second.subspan(2, second[1].as_count().value);

    return {
        .size = size,
        .inner = n.get_first(),
        .items = items,
    };
}

[[nodiscard]] constexpr auto lit(Ast const& ast, Node const& n) -> Lit {
    return {.items = ast.get_array(n.get_first().as_count().of_kv(),
                                   n.get_second().as_array())};
}

[[nodiscard]] constexpr auto call(Ast const& ast, Node const& n) -> Call {
    return {
        .callee = n.get_first(),
        .args = ast.get_array(n.get_second().as_array()),
    };
}

[[nodiscard]] constexpr auto field(Ast const& ast, Node const& n) -> Field {
    return {
        .name = ast.get_identifier(n.get_second().as_id()),
        .receiver = n.get_first(),
    };
}

[[nodiscard]] constexpr auto block(Ast const& ast, Node const& n) -> Block {
    return {.items = ast.get_array(n.get_first().as_count(),
                                   n.get_second().as_array())};
}

[[nodiscard]] constexpr auto if_stmt(Ast const& ast, Node const& n) -> IfStmt {
    if (n.get_kind() == NodeKind::IfStmt)
        return {.cond = n.get_first(),
                .wt = n.get_second(),
                .wf = NodeId::invalid()};

    // NodeKind::IfStmtWithElse
    auto second = ast.get_array({2}, n.get_second().as_array());
    return {.cond = n.get_first(), .wt = second[0], .wf = second[1]};
}

[[nodiscard]] constexpr auto while_stmt(Ast const& /*unused*/, Node const& n)
    -> WhileStmt {
    return {.cond = n.get_first(), .body = n.get_second()};
}

[[nodiscard]] constexpr auto defer_stmt(Ast const& /*unused*/, Node const& n)
    -> DeferStmt {
    return {.stmt = n.get_first()};
}

[[nodiscard]] constexpr auto var_decl(Ast const& ast, Node const& n)
    -> VarDecl {
    auto second = ast.get_array({2}, n.get_second().as_array());
    return {.ids = n.get_first(), .types = second[0], .inits = second[1]};
}

[[nodiscard]] constexpr auto def_decl(Ast const& ast, Node const& n)
    -> DefDecl {
    auto second = ast.get_array({2}, n.get_second().as_array());
    return {.ids = n.get_first(), .types = second[0], .inits = second[1]};
}

[[nodiscard]] constexpr auto assign(Ast const& /*unused*/, Node const& n)
    -> Assign {
    return {.lhs = n.get_first(), .rhs = n.get_second()};
}

// ------------------------------------------------------------------

/// Get the inner `IdPack` of a func-decl
[[nodiscard]] constexpr auto func_decl_id_pack(Node const& n) -> NodeId {
    return n.get_first();
}

/// Get the names of the inner `IdPack` of a func-decl
[[nodiscard]] constexpr auto func_decl_name(Ast const& ast, Node const& n)
    -> IdPack {
    auto ids = ast.get_node(func_decl_id_pack(n).as_ref());
    ASSERT(ids.get_kind() == ast::NodeKind::IdPack);

    return id_pack(ast, ids);
}

/// Get the inner `IdPack` for the given top-var-decl
[[nodiscard]] constexpr auto top_var_decl_id_pack(Ast const& ast, Node const& n)
    -> NodeId {
    auto child = ast.get_node(n.get_first().as_ref());
    ASSERT(child.get_kind() == ast::NodeKind::VarDecl);

    return child.get_first();
}

/// Get the names of the inner `IdPack` for the given top-var-decl
[[nodiscard]] constexpr auto top_var_decl_names(Ast const& ast, Node const& n)
    -> IdPack {
    auto ids = ast.get_node(top_var_decl_id_pack(ast, n).as_ref());
    ASSERT(ids.get_kind() == ast::NodeKind::IdPack);

    return id_pack(ast, ids);
}

/// Get the inner `IdPack` for the given top-def-decl
[[nodiscard]] constexpr auto top_def_decl_id_pack(Ast const& ast, Node const& n)
    -> NodeId {
    auto child = ast.get_node(n.get_first().as_ref());
    ASSERT(child.get_kind() == ast::NodeKind::DefDecl);

    return child.get_first();
}

/// Get the names of the inner `IdPack` for the given top-def-decl
[[nodiscard]] constexpr auto top_def_decl_names(Ast const& ast, Node const& n)
    -> IdPack {
    auto ids = ast.get_node(top_def_decl_id_pack(ast, n).as_ref());
    ASSERT(ids.get_kind() == ast::NodeKind::IdPack);

    return id_pack(ast, ids);
}

}  // namespace yal::ast::conv
