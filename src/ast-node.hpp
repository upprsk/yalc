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

    /// A keyword literal (`.id`)
    KwLit,

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
    FuncDeclWithCVarArgs,

    /// A top-level (global) variable declaration.
    ///
    /// - `first` points to a VarDecl node.
    /// - `second` points to an array of decorators added to the declaration.
    ///
    /// The `second` array is made of `[{length}, ...]`.
    TopVarDecl,

    /// A top-level (global) constant declaration.
    ///
    /// - `first` points to a DefDecl node.
    /// - `second` points to an array of decorators added to the declaration.
    ///
    /// The `second` array is made of `[{length}, ...]`.
    TopDefDecl,

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

    /// Decorates a top-level declaration to add additional functionality (like
    /// extern).
    ///
    ///     @extern(name="external.value")
    ///      ^~~~~^ ^~~^ ^~~~~~~~~~~~~~~^
    ///      |      |     \_ value
    ///      |      \_ key
    ///      \_ name
    ///
    ///     @something(a, "b", c=1)
    ///      ^~~~~~~~^ ^  ^~^  ^ ^
    ///      |         |  |    |  \_ value
    ///      |         |  |    \_ key
    ///      |         |  \_ value
    ///      |         \_ key
    ///      \_ name
    ///
    /// - `first`: points to an identifier for the name of the decorator. Node
    /// that the starting `@` is removed.
    /// - `second`: may point to the arguments. In case the decorator is used
    /// without arguments, then this is an invalid id. When there is both key
    /// and value, then we have a pair of `[{key, value}]` in the array. When
    /// there is just a key (an identifier without the `=`), we have `[{key},
    /// <inval>]`. When there is just a value, we have `[<inval>, {value}]`.
    Decorator,

    /// Import statement.
    ///
    /// - `first` points to the library import path string.
    ImportStmt,

    /// -----------
    /// Expressions
    /// -----------

    /// Used for multiple returns and contains a list of expressions.
    ///
    /// - `first` has the count of how many children the node has.
    /// - `second` pointer to array of children.
    ExprPack,

    /// Generic binary operation expressions.
    ///
    /// - `first` has the left side of the operation.
    /// - `second` has the right side of the operation.
    Add,           // +
    Sub,           // -
    Mul,           // *
    Div,           // /
    Mod,           // %
    LeftShift,     // <<
    RightShift,    // >>
    Equal,         // ==
    NotEqual,      // !=
    Less,          // <
    LessEqual,     // <=
    Greater,       // >
    GreaterEqual,  // >=
    Band,          // &
    Bor,           // |
    Bxor,          // ^
    Land,          // and
    Lor,           // or
    Cast,          // as

    /// Generic unary operation expressions.
    ///
    /// - `first` points to the child.
    AddrOf,  // &
    Lnot,    // !
    Bnot,    // ~
    Neg,     // -

    /// A struct type literal
    ///
    ///     struct {
    ///         field_1: type_1,
    ///         field_2: type_2 = init,
    ///     }
    ///
    /// - `first` contains the number of fields.
    /// - `second` points to an array of struct fields.
    StructType,

    /// A struct type field.
    ///
    ///     struct {
    ///         field_1: type_1,
    ///         field_2: type_2 = init,
    ///     }
    ///
    /// - `first` points to an identifier for the name of the field.
    /// - `second` points to an array with `[{type}, {init}]`. `init` is
    /// optional and may be an invalid id.
    StructField,

    /// A pointer type literal.
    ///
    ///     *const i32
    ///     *f32
    ///
    /// - `first` points to the inner type.
    PtrConst,
    Ptr,

    /// A multi-pointer type literal.
    ///
    ///     [*]const u8
    ///     [*]i32
    ///
    /// - `first` points to the inner type.
    MultiPtrConst,
    MultiPtr,

    /// A slice type literal.
    ///
    ///     []const u8
    ///     []i16
    ///
    /// - `first` points to the inner type.
    SliceConst,
    Slice,

    /// An array type literal.
    ///
    ///     [23]const i32
    ///     [6 + 6]u8
    ///
    /// - `first` points to the inner type.
    /// - `second` points to the size.
    ArrayTypeConst,
    ArrayType,

    /// An array literal.
    ///
    ///     [5]i32{0, 1, 2, 3, 4, 5}
    ///     [_]u8{'h', 'i', '!', 0}
    ///
    /// - `first` points to the inner type.
    /// - `second` points to an array with size expression and list of
    /// initializer items in the format: `[{inner}, {length}, ...]`. In case the
    /// size in inferred (`_`), then `size` is an invalid id.
    Array,

    /// A struct or array literal, with inferred/auto type.
    ///
    ///     .{.a = v1, .b = v2}
    ///     .{1, 2, 3}
    ///
    /// - `first` contains the number of initializer item pairs.
    /// - `second` contains a list of key-value pairs. In case there is no key,
    /// then it has an invalid id in it's place.
    ///
    /// For example: `.{.a = v1, .b = v2, .c, 1}`
    ///     +----+----+----+----+----+----+----+----+
    ///     | .a | v1 | .b | v2 | -- | .c | -- |  1 |
    ///     +----+----+----+----+----+----+----+----+
    Lit,

    /// A function call.
    ///
    /// - `first` contains the callee.
    /// - `second` points to an array of arguments.
    ///
    /// The `second` array is made of `[{length}, ...]`.
    Call,

    /// A field access.
    ///
    /// - `first` points to the receiver.
    /// - `second` points to the field name (identifier).
    Field,

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

    /// An if statement without an else.
    ///
    /// - `first` has the condition expression.
    /// - `second` has the _then_ block.
    IfStmt,

    /// An if statement with an else.
    ///
    /// - `first` has the condition expression.
    /// - `second` points to an array with the _then_ and _else_ branches (in
    /// the format `[{then}, {else}]`.
    IfStmtWithElse,

    /// A while statement.
    ///
    /// - `first` has the condition expression.
    /// - `second` has the body.
    WhileStmt,

    /// Break statement.
    Break,

    /// Continue statement.
    Continue,

    /// Defer statement.
    DeferStmt,

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

    /// A constant declaration.
    ///
    /// - `first` points to an `IdPack` with all of the declared names.
    /// - `second` points to an an array with all explicit types and
    /// initializers as `[{types}, {inits}]`. Each of `types` and `inits` should
    /// be an `ExprPack`.
    ///
    /// `types` is optional, so it can be an invalid id.
    DefDecl,

    /// Assigment statements.
    ///
    /// - `first` points to the left hand side of the assigment.
    /// - `second` points to the right hand side of the assgment.
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignShiftLeft,
    AssignShiftRight,
    AssignBand,
    AssignBxor,
    AssignBor,
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
        case NodeKind::KwLit: name = "KwLit"; break;
        case NodeKind::Int: name = "Int"; break;
        case NodeKind::Double: name = "Double"; break;
        case NodeKind::Float: name = "Float"; break;
        case NodeKind::Str: name = "Str"; break;
        case NodeKind::Char: name = "Char"; break;
        case NodeKind::Module: name = "Module"; break;
        case NodeKind::SourceFile: name = "SourceFile"; break;
        case NodeKind::ModuleDecl: name = "ModuleDecl"; break;
        case NodeKind::FuncDecl: name = "FuncDecl"; break;
        case NodeKind::FuncDeclWithCVarArgs:
            name = "FuncDeclWithCVarArgs";
            break;
        case NodeKind::TopVarDecl: name = "TopVarDecl"; break;
        case NodeKind::TopDefDecl: name = "TopDefDecl"; break;
        case NodeKind::IdPack: name = "IdPack"; break;
        case NodeKind::FuncParam: name = "FuncParam"; break;
        case NodeKind::FuncRetPack: name = "FuncRetPack"; break;
        case NodeKind::Decorator: name = "Decorator"; break;
        case NodeKind::ImportStmt: name = "ImportStmt"; break;
        case NodeKind::ExprPack: name = "ExprPack"; break;
        case NodeKind::Add: name = "Add"; break;
        case NodeKind::Sub: name = "Sub"; break;
        case NodeKind::Mul: name = "Mul"; break;
        case NodeKind::Div: name = "Div"; break;
        case NodeKind::Mod: name = "Mod"; break;
        case NodeKind::LeftShift: name = "LeftShift"; break;
        case NodeKind::RightShift: name = "RightShift"; break;
        case NodeKind::Equal: name = "Equal"; break;
        case NodeKind::NotEqual: name = "NotEqual"; break;
        case NodeKind::Less: name = "Less"; break;
        case NodeKind::LessEqual: name = "LessEqual"; break;
        case NodeKind::Greater: name = "Greater"; break;
        case NodeKind::GreaterEqual: name = "GreaterEqual"; break;
        case NodeKind::Band: name = "Band"; break;
        case NodeKind::Bor: name = "Bor"; break;
        case NodeKind::Bxor: name = "Bxor"; break;
        case NodeKind::Land: name = "Land"; break;
        case NodeKind::Lor: name = "Lor"; break;
        case NodeKind::Cast: name = "Cast"; break;
        case NodeKind::AddrOf: name = "AddrOf"; break;
        case NodeKind::Lnot: name = "Lnot"; break;
        case NodeKind::Bnot: name = "Bnot"; break;
        case NodeKind::Neg: name = "Neg"; break;
        case NodeKind::StructType: name = "StructType"; break;
        case NodeKind::StructField: name = "StructField"; break;
        case NodeKind::PtrConst: name = "PtrConst"; break;
        case NodeKind::Ptr: name = "Ptr"; break;
        case NodeKind::MultiPtrConst: name = "MultiPtrConst"; break;
        case NodeKind::MultiPtr: name = "MultiPtr"; break;
        case NodeKind::SliceConst: name = "SliceConst"; break;
        case NodeKind::Slice: name = "Slice"; break;
        case NodeKind::ArrayTypeConst: name = "ArrayTypeConst"; break;
        case NodeKind::ArrayType: name = "ArrayType"; break;
        case NodeKind::Array: name = "Array"; break;
        case NodeKind::Lit: name = "Lit"; break;
        case NodeKind::Call: name = "Call"; break;
        case NodeKind::Field: name = "Field"; break;
        case NodeKind::Block: name = "Block"; break;
        case NodeKind::ExprStmt: name = "ExprStmt"; break;
        case NodeKind::ReturnStmt: name = "ReturnStmt"; break;
        case NodeKind::IfStmt: name = "IfStmt"; break;
        case NodeKind::IfStmtWithElse: name = "IfStmtWithElse"; break;
        case NodeKind::WhileStmt: name = "WhileStmt"; break;
        case NodeKind::Break: name = "Break"; break;
        case NodeKind::Continue: name = "Continue"; break;
        case NodeKind::DeferStmt: name = "DeferStmt"; break;
        case NodeKind::VarDecl: name = "VarDecl"; break;
        case NodeKind::DefDecl: name = "DefDecl"; break;
        case NodeKind::Assign: name = "Assign"; break;
        case NodeKind::AssignAdd: name = "AssignAdd"; break;
        case NodeKind::AssignSub: name = "AssignSub"; break;
        case NodeKind::AssignMul: name = "AssignMul"; break;
        case NodeKind::AssignDiv: name = "AssignDiv"; break;
        case NodeKind::AssignMod: name = "AssignMod"; break;
        case NodeKind::AssignShiftLeft: name = "AssignShiftLeft"; break;
        case NodeKind::AssignShiftRight: name = "AssignShiftRight"; break;
        case NodeKind::AssignBand: name = "AssignBand"; break;
        case NodeKind::AssignBxor: name = "AssignBxor"; break;
        case NodeKind::AssignBor: name = "AssignBor"; break;
    }

    return name;
}

}  // namespace yal::ast
