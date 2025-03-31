#pragma once

#include <bit>
#include <cstdint>

#include "ast-node-id.hpp"
#include "file-store.hpp"

namespace yal::ast {

// All of the kinds of AST nodes.
enum class NodeKind : uint16_t {
    /// An error in the AST, most likely due to parsing errors.
    //
    /// - `first` points to a string (in bytes in the store) with an error
    /// message.
    Err = 0,

    /// --------
    /// Literals
    /// --------

    /// An identifier.
    //
    /// - `first` points to the identifier in the store.
    Id,

    /// An integer.
    //
    /// - `first` and `second` form a 64bit unsigned integer.
    Int,

    /// A double/f64.
    //
    /// - `first` and `second` form a 64bit floating point number.
    Double,

    /// A float/f32.
    //
    /// - `first` contains a 32bit floating point number.
    /// NOTE: this might not be needed
    Float,

    /// A string.
    //
    /// - `first` points to the byte contents of the string in the store.
    Str,

    /// A character.
    //
    /// - `first` contains the byte value (or multi-byte for UFT-8) of the
    /// character.
    Char,

    /// ---------
    /// Top-Level
    /// ---------

    /// A module, which may contain multiple source files. This is the root of
    /// the tree.
    //
    /// - `first` points to an identifier with the module name.
    /// - `second` points to an array of `SourceFile`s.
    //
    /// The `second` array is made of `[{length}, ...]`.
    //
    /// The location of this node is that of the first source file found when
    /// discovering the module. Not very exact but should be sufficient.
    Module,

    /// A single source file. Always a child of `Module`.
    //
    /// - `first` points to a ModuleDecl (the `module <name>;` at the start of
    /// every file).
    /// - `second` points to an array of children nodes.
    //
    /// The `second` array is made of `[{length}, ...]`.
    SourceFile,

    /// The `module <name>;` at the start of a source file.
    //
    /// - `first` points to the identifier of `<name>`.
    /// - `second` is not used.
    ModuleDecl,

    /// A function declaration.
    ///
    ///     @decor(stuff) func ns.name[T: any](a: i32, b: T) T {}
    ///     ^~~~~~~~~~~~^      ^~~~~~^ ^~~~~^  ^~~~~~~~~~~^  ^ ^^
    ///     |                  |       |       |             | |
    ///     |                  |       |       |             | \_ body
    ///     |                  |       |       |             |
    ///     |                  |       |       |             \_ return type
    ///     |                  |       |       |
    ///     |                  |       |       \_ arguments
    ///     |                  |       |
    ///     \_ decorators      |       \_ generic arguments
    ///                        |
    ///                        \_ name (with namespace)
    ///
    /// 1. A function may be annotated with a "decorator". This can change
    /// various attributes of the function, like: `extern`, link name,
    /// alignment, etc.
    ///
    /// - `first` points to a `IdPack` node with the function name (and
    /// namespacing).
    /// - `second` points to all other things in an array.
    ///
    /// Structure of `second`:
    ///
    /// - **len(A)**: Number of decorators added to the function.
    /// - **A...**: Pointers to each decorator.
    /// - **len(B)**: Number of generic arguments.
    /// - **B...**: Pointers to each generic argument.
    /// - **len(C)**: Number of arguments.
    /// - **C...**: Pointers to each argument.
    /// - **D?**: Pointer to the return type, if any. The function may have
    /// multiple return values, this is handled by using a FuncRetPack node.
    /// - **E?**: Pointer to the body, if any.
    ///
    /// +--------+--....--+--------+--....--+--------+--....--+--------+--------+
    /// | len(A) |  A...  | len(B) |  B...  | len(C) |  C...  |   D?   |   E? |
    /// +--------+--....--+--------+--....--+--------+--....--+--------+--------+
    FuncDecl,

    /// A top-level (global) variable declaration.
    ///
    /// - `first` points to a VarDecl node.
    /// - `second` points to an array of decorators added to the function.
    ///
    /// The `second` array is made of `[{length}, ...]`.
    TopVarDecl,

    /// A pack of ids. This is used in function definitions for namespacing and
    /// on variable definitions to allow multiple returns.
    ///
    /// - `first`: Number of identifiers.
    /// - `second`: Pointer to array of identifiers (in the identifier list).
    /// The ids are in wrapping order, with the last one having the actual name
    /// of the function and the previous ones containing each of the wrapping
    /// names.
    IdPack,

    /// A function parameter.
    ///
    /// - `first` has a pointer to the parameter name in identifiers.
    /// - `second` has a pointer to the type expression. A parameter may not
    /// have an explicit type, so this field may be an invalid id.
    FuncParam,

    /// Used for functions that return multiple values.
    ///
    /// Function return values may be named, this is why we store key-value
    /// pairs in this node. In case the node is not named, then the key is an
    /// invalid id. This node is used even when the function returns a single
    /// value.
    ///
    /// - `first`: has a count of how many key-value pairs there are.
    /// - `second`: pointer to array of key-value pairs of the name of the
    /// return value and type.
    FuncRetPack,

    /// -----------
    /// Expressions
    /// -----------

    /// Used for multiple returns and contains a list of expressions.
    ///
    /// - `first` has the count of how many children the node has.
    /// - `second` pointer to array of children.
    ExprPack,

    /// ----------
    /// Statements
    /// ----------

    /// A block of statements.
    ///
    /// - `first` has the number of statements.
    /// - `second` points to array of `first` statements.
    Block,

    /// A statement over an expression, like `print("hi!");`.
    ///
    /// - `first` has the child node.
    ExprStmt,

    /// A return statement.
    ///
    /// - `first` has the child node. For multiple return values, child will be
    /// an ExprPack. When it is a bare return without an expression, child is an
    /// invalid id.
    ReturnStmt,

    /// A variable declaration.
    ///
    ///     var a, b: T, U = 1, 2;
    ///         ^~~^  ^~~^   ^~~^
    ///         |     |      |
    ///         |     |      \_ initializers
    ///         |     \_ types
    ///         \_ ids
    ///
    /// - `first` points to an `IdPack` with all of the declared names.
    /// - `second` points to an an array with all explicit types and
    /// initializers as `[{types}, {inits}]`. Each of `types` and `inits` should
    /// be an `ExprPack`.
    ///
    /// Both `types` and `inits` are optional during parsing, so either can be
    /// an invalid id.
    VarDecl,
};

class Node {
    constexpr Node(NodeKind kind, NodeId id, Location loc, NodeId first,
                   NodeId second)
        : kind{kind}, id{id}, loc{loc}, first{first}, second{second} {}

public:
    // ------------
    // Constructors
    // ------------
    constexpr Node(Node const &) = default;
    constexpr Node(Node &&) = default;
    constexpr auto operator=(Node const &) -> Node & = default;
    constexpr auto operator=(Node &&) -> Node & = default;

    // construct an AST node from it's raw parts
    static constexpr auto from_parts(NodeKind kind, NodeId id, Location loc,
                                     NodeId first, NodeId second) -> Node {
        return {kind, id, loc, first, second};
    }

    [[nodiscard]] constexpr auto get_kind() const -> NodeKind { return kind; }
    [[nodiscard]] constexpr auto get_id() const -> NodeId { return id; }
    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }
    [[nodiscard]] constexpr auto get_first() const -> NodeId { return first; }
    [[nodiscard]] constexpr auto get_second() const -> NodeId { return second; }

    [[nodiscard]] constexpr auto cast_u32() const -> uint32_t {
        return first.value();
    }

    [[nodiscard]] constexpr auto cast_u64() const -> uint64_t {
        return static_cast<uint64_t>(first.value()) << 32 |
               static_cast<uint64_t>(second.value());
    }

    [[nodiscard]] constexpr auto cast_double() const -> double {
        return std::bit_cast<double>(cast_u64());
    }

    [[nodiscard]] constexpr auto cast_float() const -> float {
        return std::bit_cast<float>(first.value());
    }

private:
    NodeKind kind;
    NodeId   id;
    Location loc;
    NodeId   first;
    NodeId   second;
};

constexpr auto format_as(NodeKind kind) {
    std::string_view name;

    switch (kind) {
        case NodeKind::Err: name = "Err"; break;
        case NodeKind::Id: name = "Id"; break;
        case NodeKind::Int: name = "Int"; break;
        case NodeKind::Double: name = "Double"; break;
        case NodeKind::Float: name = "Float"; break;
        case NodeKind::Str: name = "Str"; break;
        case NodeKind::Char: name = "Char"; break;
        case NodeKind::Module: name = "Module"; break;
        case NodeKind::SourceFile: name = "SourceFile"; break;
        case NodeKind::ModuleDecl: name = "ModuleDecl"; break;
        case NodeKind::FuncDecl: name = "FuncDecl"; break;
        case NodeKind::TopVarDecl: name = "TopVarDecl"; break;
        case NodeKind::IdPack: name = "IdPack"; break;
        case NodeKind::FuncParam: name = "FuncParam"; break;
        case NodeKind::FuncRetPack: name = "FuncRetPack"; break;
        case NodeKind::ExprPack: name = "ExprPack"; break;
        case NodeKind::Block: name = "Block"; break;
        case NodeKind::ExprStmt: name = "ExprStmt"; break;
        case NodeKind::ReturnStmt: name = "ReturnStmt"; break;
        case NodeKind::VarDecl: name = "VarDecl"; break;
    }

    return name;
}

}  // namespace yal::ast
