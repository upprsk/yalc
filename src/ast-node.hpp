#pragma once

#include <bit>
#include <cstdint>
#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "file-store.hpp"
#include "types.hpp"

namespace yal {
struct Decl;
}

namespace yal::ast {

// All of the kinds of AST nodes.
enum class NodeKind : uint16_t {
    /// An error in the AST, most likely due to parsing errors.
    Err = 0,

    /// --------
    /// Literals
    /// --------

    /// An identifier.
    ///
    /// - `data` contains a string with the identifier.
    Id,

    /// A keyword literal (`.id`)
    ///
    /// - `data` contains a string with the identifier.
    KwLit,

    /// An integer.
    ///
    /// - `data` contains a 64bit unsigened integer.
    Int,

    /// A double/f64.
    //
    /// - `data` contains a 64bit double.
    Double,

    /// A float/f32.
    //
    /// - `data` contains a 32bit double.
    Float,

    /// A string.
    //
    /// - `data` contains a string (already escaped).
    Str,

    /// A character.
    //
    /// - `data` contains the character in a 64bit unsigened integer.
    Char,

    /// ---------
    /// Top-Level
    /// ---------

    /// A module, which may contain multiple source files. This is the root of
    /// the tree.
    ///
    /// - `data` contains a string with the module name.
    /// - `children` contains all of `SourceFile`s that are a part of the
    /// module.
    ///
    /// The location of this node is that of the first source file found when
    /// discovering the module. Not very exact but should be sufficient.
    Module,

    /// A single source file. Always a child of `Module`.
    ///
    /// - `children` contains in the first position the `ModuleDecl` ast node
    /// followed by all of the declarations in the file.
    SourceFile,

    /// The `module <name>;` at the start of a source file.
    ///
    /// - `data` has a string with the module name.
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
    /// A function may be annotated with a "decorator". This can change various
    /// attributes of the function, like: `extern`, link name, alignment, etc.
    ///
    /// - `children` points to all parts of the function declaration.
    ///     1. `Decorators` node with all decorators.
    ///     2. `IdPack` node with the function name (maybe namespaced).
    ///     3. `FuncParams` node with all generic arguments.
    ///     4. `FuncParams` node with all arguments.
    ///     5. The return type, or null in case we have no explicit return
    ///     type.
    ///     6. The function body, or null in case the function has no body.
    FuncDecl,
    FuncDeclWithCVarArgs,

    /// The list of arguments or generic arguments to a function.
    ///
    /// - `children` points to either a list of `FuncParam` or `FuncGenParam`,
    /// depending if this is used as arguments or generic arguments.
    FuncParams,

    /// A top-level (global) variable declaration.
    ///
    /// - `children` has 2 elements: The inner `VarDecl` and a `Decorators`.
    TopVarDecl,

    /// A top-level (global) constant declaration.
    ///
    /// - `children` has 2 elements: The inner `DefDecl` and a `Decorators`.
    TopDefDecl,

    /// A pack of ids. This is used in function definitions for namespacing and
    /// on variable definitions to allow multiple returns.
    ///
    /// - `children` points to various `Id` nodes with the identifiers.
    IdPack,

    /// A function parameter.
    ///
    /// - `data` has a string with the name of the parameter.
    /// - `children` has the type of the expression, or null in case it was
    /// not given.
    FuncParam,

    /// Used for functions that return multiple values.
    ///
    /// Function return values may be named, this is why we store either any
    /// expression or a `NamedRet`.
    ///
    /// - `children` list of either arbitrary expressions or `NamedRet`.
    FuncRetPack,

    /// Store a named return.
    ///
    /// - `data` has a string with the name of the return.
    /// - `children` has the type.
    NamedRet,

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
    /// - `data` is a string with the decorator name.
    /// - `children` has the parameters. Each item may be either an expression
    /// for a positional argument or a `DecoratorParam` for keyword arguments.
    Decorator,

    /// A decorator parameter.
    ///
    /// - `data` has a string with the name of the parameter.
    /// - `children` has the value.
    DecoratorParam,

    /// A list of decorators, as we can add many decorators to a single
    /// declaration.
    ///
    /// - `children` has all of the `Decorator` notes.
    Decorators,

    /// Import statement.
    ///
    /// - `data` is a string with the import path.
    ImportStmt,

    /// -----------
    /// Expressions
    /// -----------

    /// Used for multiple returns and assignments and contains a list of
    /// expressions.
    ///
    /// - `children` has all of the expressions.
    ExprPack,

    /// Generic binary operation expressions.
    ///
    /// - `children` has 2 elements: the left an right side of the operation.
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
    /// - `children` has the child.
    AddrOf,  // &
    Deref,   // *
    Lnot,    // !
    Bnot,    // ~
    Neg,     // -

    /// A struct type literal.
    ///
    ///     struct {
    ///         field_1: type_1,
    ///         field_2: type_2 = init,
    ///     }
    ///
    /// - `children` has many `StructField`s.
    StructType,

    /// A struct type field.
    ///
    ///     struct {
    ///         field_1: type_1,
    ///         field_2: type_2 = init,
    ///     }
    ///
    /// - `data` has a string with the name of the field.
    /// - `children` has 2 elements:
    ///     1. The type of the field.
    ///     2. The default initializer for the field. May be null in case the
    ///     field does not have a default initializer.
    StructField,

    /// A pointer type literal.
    ///
    ///     *const i32
    ///     *f32
    ///
    /// - `children` points to the inner type.
    PtrConst,
    Ptr,

    /// A multi-pointer type literal.
    ///
    ///     [*]const u8
    ///     [*]i32
    ///
    /// - `children` points to the inner type.
    MultiPtrConst,
    MultiPtr,

    /// A slice type literal.
    ///
    ///     []const u8
    ///     []i16
    ///
    /// - `children` points to the inner type.
    SliceConst,
    Slice,

    /// An array type literal.
    ///
    ///     [23]const i32
    ///     [6 + 6]u8
    ///
    /// - `children` has 2 elements:
    ///     1. The inner type
    ///     2. The expression for the size of the array.
    ArrayTypeConst,
    ArrayType,

    /// An array literal.
    ///
    ///     [5]i32{0, 1, 2, 3, 4, 5}
    ///     [_]u8{'h', 'i', '!', 0}
    ///
    /// - `children` has at least 2 elements:
    ///     1. The inner type.
    ///     2. The expression for the size of the array. In case the array has
    ///     an inferred size (`_`), then this is null.
    ///     3. All of the remaining elements form the elements of the
    ///     initializer.
    Array,

    /// An optional type.
    ///
    ///     ?*i32
    ///     ?[]string_view
    ///     ?struct { v: i32 }
    ///
    /// - `children` has the inner type.
    Optional,

    /// A struct or array literal, with inferred/auto type.
    ///
    ///     .{.a = v1, .b = v2}
    ///     .{1, 2, 3}
    ///
    /// - `children` has an array where each element is either an arbitrary
    /// expression for a positional item or a `LitParam` when the item is keyed.
    Lit,

    /// A single keyed item in a `Lit`.
    ///
    /// - `data` has a string with the key.
    /// - `children` has the initializer expression.
    LitParam,

    /// A function call.
    ///
    /// - `children` contains the callee and then all of the arguments.
    Call,

    /// A field access.
    ///
    /// - `data` has a string with the field name.
    /// - `children` has the receiver.
    Field,

    /// ----------
    /// Statements
    /// ----------

    /// A block of statements.
    ///
    /// - `children` has all of the statements.
    Block,

    /// A statement over an expression, like `print("hi!");`.
    ///
    /// - `children` has the child node.
    ExprStmt,

    /// A return statement.
    ///
    /// - `children` has the child node. For multiple return values, child will
    /// be an `ExprPack`. When it is a bare return without an expression,
    /// `children` is empty.
    ReturnStmt,

    /// An if statement.
    ///
    /// - `children` has 3 elements: the condition, followed by the _then_
    /// branch, followed by the _else_ branch.
    IfStmt,

    /// A while statement.
    ///
    /// - `children` has 2 elements: the condition and the loop block.
    WhileStmt,

    /// Break statement.
    Break,

    /// Continue statement.
    Continue,

    /// Defer statement.
    ///
    /// - `children` has the deferred statement.
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
    /// - `children` has 3 elements:
    ///     1. `IdPack` with all identifiers beeing declared.
    ///     2. An `ExprPack` with all of the types. This may be null when we
    ///     have no explicit types.
    ///     3. An `ExprPack` with all of the initializers. This may be null when
    ///     we have no initializers.
    ///
    /// Both `types` and `inits` are optional during parsing, so either can be
    /// null.
    VarDecl,

    /// A constant declaration.
    ///
    ///     def a, b: T, U = 1, 2;
    ///         ^~~^  ^~~^   ^~~^
    ///         |     |      |
    ///         |     |      \_ initializers
    ///         |     \_ types
    ///         \_ ids
    ///
    /// - `children` has 3 elements:
    ///     1. `IdPack` with all identifiers beeing declared.
    ///     2. An `ExprPack` with all of the types. This may be null when we
    ///     have no explicit types.
    ///     3. An `ExprPack` with all of the initializers. This may be null when
    ///     we have no initializers.
    ///
    /// Both `types` and `inits` are optional during parsing, so either can be
    /// null, even if `inits` is required.
    DefDecl,

    /// Assigment statements.
    ///
    /// - `children` has 2 elements:
    ///     1. Has an `ExprPack` with all of the l-values for the assignment.
    ///     2. Has an `ExprPack` with all of the r-values for the assignment.
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

    /// ---------
    /// Internals
    /// ---------

    /// After name resolution, all top-level nodes are placed in a flat module
    /// structure, ordered correctly for semantic analysis.
    ///
    /// - `children` contains all of the top-level declarations.
    /// - `data` has a string with the module name.
    FlatModule,

    /// Coerce some type into another. This is inserted whenever an implicit
    /// conversion ocurs.
    ///
    /// - `children` has 1 element, the expression to convert.
    /// - `data` has the type to convert to.
    Coerce,

    /// Discard the result of some expression.
    ///
    ///     _ = 12;
    ///
    /// - `children` has 1 element, the child expression that has the discarded
    /// value.
    Discard,

    /// Used as a placeholder for discarded values.
    ///
    ///     a, _ = fn();
    ///        ^_ this becomes a Discarded node
    Discarded,

    /// A group of statements, like a block, but that does not participate in
    /// name resolution or scope rules. it is used when transforming one
    /// node becomes many. This should be flattened on a later step.
    ///
    /// - `children` contains the child statements.
    UnscopedGroup,

    /// A de-sugared assignment, only has one item in the left hand side and one
    /// element in the right hand side.
    ///
    /// - `children` constains 2 elements, the lhs and the rhs.
    AssignDirect,

    /// A de-sugared assigment from a function call. The left hand side contains
    /// the expressions to assign to and the right hand side contains just one
    /// expression (the function call). Discarded values are substituted with
    /// Discarded nodes.
    ///
    /// - `children` contains 2 elements, the ExprPack for the rhs and a single
    /// expression for rhs.
    AssignDirectPack,

    /// Declare a single local variable.
    ///
    /// - `children` has the initializer for the variable.
    /// - `data` has the variable name.
    DeclLocalVarDirect,

    /// Declare local variables from a function with multible returns.
    ///
    /// - `children` has n+1 elements. The first n elements are `Id`s with the
    /// names beeing declared or `Discarded` when the value is discarded. The
    /// last element is the initializer expression with the function call.
    DeclLocalVarDirectPack
};

class Node {
public:
    // ------------
    // Constructors
    // ------------
    constexpr Node(Node const &o) = default;
    constexpr Node(Node &&) = default;
    constexpr auto operator=(Node const &) -> Node & = default;
    constexpr auto operator=(Node &&) -> Node & = default;

    constexpr Node(NodeKind kind, Location loc, std::span<Node *> children,
                   std::variant<std::monostate, float, double, uint64_t,
                                std::string_view, types::Type *>
                       data)
        : kind{kind}, loc{loc}, children{children}, data{std::move(data)} {}

    // ------------------------------------------------------------------------

    [[nodiscard]] constexpr auto get_kind() const -> NodeKind { return kind; }
    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }
    [[nodiscard]] constexpr auto get_children() const -> std::span<Node *> {
        return children;
    }
    [[nodiscard]] constexpr auto get_child(size_t at) const -> Node * {
        ASSERT(at < children.size());
        return children[at];
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return is_oneof(NodeKind::Err);
    }

    constexpr auto is_oneof(auto &&...kinds) const -> bool {
        return ((get_kind() == kinds) || ...);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] constexpr auto get_data_f64() const -> double {
        return std::get<double>(data);
    }

    [[nodiscard]] constexpr auto get_data_f32() const -> float {
        return std::get<float>(data);
    }

    [[nodiscard]] constexpr auto get_data_str() const -> std::string_view {
        return std::get<std::string_view>(data);
    }

    [[nodiscard]] constexpr auto get_data_u64() const -> uint64_t {
        return std::get<uint64_t>(data);
    }

    [[nodiscard]] constexpr auto get_data_type() const -> types::Type * {
        return std::get<types::Type *>(data);
    }

    [[nodiscard]] constexpr auto get_decl() const -> Decl * { return decl; }
    constexpr void               set_decl(Decl *d) { decl = d; }

    [[nodiscard]] constexpr auto get_type() const -> types::Type * {
        return type;
    }
    constexpr void set_type(types::Type *d) { type = d; }

    [[nodiscard]] constexpr auto get_data() const
        -> std::variant<std::monostate, float, double, uint64_t,
                        std::string_view, types::Type *> {
        return data;
    }

    [[nodiscard]] constexpr auto has_data() const -> bool {
        return std::holds_alternative<std::monostate>(data);
    }

    // ------------------------------------------------------------------------

    constexpr void clear_data() { data = {}; }
    constexpr void set_data(float v) { data = v; }
    constexpr void set_data(double v) { data = v; }
    constexpr void set_data(uint64_t v) { data = v; }
    constexpr void set_data(std::string_view v) { data = v; }

    constexpr void update_with_decl_ref(std::string_view v) { data = v; }

    constexpr void set_child(size_t i, Node *n) {
        ASSERT(i < children.size());
        children[i] = n;
    }

    // ------------------------------------------------------------------------

    void transmute_to_unscoped_group(
        std::span<Node *> allocated_unscoped_items);

private:
    NodeKind          kind{};
    Location          loc{};
    std::span<Node *> children;

    Decl        *decl{};
    types::Type *type{};

    std::variant<std::monostate, float, double, uint64_t, std::string_view,
                 types::Type *>
        data;
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
        case NodeKind::FuncParams: name = "FuncParams"; break;
        case NodeKind::TopVarDecl: name = "TopVarDecl"; break;
        case NodeKind::TopDefDecl: name = "TopDefDecl"; break;
        case NodeKind::IdPack: name = "IdPack"; break;
        case NodeKind::FuncParam: name = "FuncParam"; break;
        case NodeKind::FuncRetPack: name = "FuncRetPack"; break;
        case NodeKind::NamedRet: name = "NamedRet"; break;
        case NodeKind::Decorator: name = "Decorator"; break;
        case NodeKind::DecoratorParam: name = "DecoratorParam"; break;
        case NodeKind::Decorators: name = "Decorators"; break;
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
        case NodeKind::Deref: name = "Deref"; break;
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
        case NodeKind::Optional: name = "Optional"; break;
        case NodeKind::Lit: name = "Lit"; break;
        case NodeKind::LitParam: name = "LitParam"; break;
        case NodeKind::Call: name = "Call"; break;
        case NodeKind::Field: name = "Field"; break;
        case NodeKind::Block: name = "Block"; break;
        case NodeKind::ExprStmt: name = "ExprStmt"; break;
        case NodeKind::ReturnStmt: name = "ReturnStmt"; break;
        case NodeKind::IfStmt: name = "IfStmt"; break;
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
        case NodeKind::FlatModule: name = "FlatModule"; break;
        case NodeKind::Coerce: name = "Coerce"; break;
        case NodeKind::Discard: name = "Discard"; break;
        case NodeKind::Discarded: name = "Discarded"; break;
        case NodeKind::UnscopedGroup: name = "UnscopedGroup"; break;
        case NodeKind::AssignDirect: name = "AssignDirect"; break;
        case NodeKind::AssignDirectPack: name = "AssignDirectPack"; break;
        case NodeKind::DeclLocalVarDirect: name = "DeclLocalVarDirect"; break;
        case NodeKind::DeclLocalVarDirectPack:
            name = "DeclLocalVarDirectPack";
            break;
    }

    return name;
}

void to_json(json &j, Node const &n);

}  // namespace yal::ast

template <>
struct fmt::formatter<yal::ast::Node> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ast::Node const &n, format_context &ctx) const
        -> format_context::iterator;
};
