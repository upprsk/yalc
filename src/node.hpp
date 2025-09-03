#pragma once

#include <array>
#include <cstdint>
#include <libassert/assert.hpp>
#include <span>
#include <string_view>

#include "decl.hpp"
#include "location.hpp"
#include "macros.hpp"

namespace yal::ast {

/// All of the variants of expressions.
enum struct ExprKind : uint8_t {
    Err,

    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Id,
    Int,
    String,
};

/// Flags for controlling layout of data inside of an expression.
struct ExprFlags {
    enum Bits {
        None = 0,
        ChildrenSingle = 1,
        ChildrenPair = 2,
    };

    uint8_t value = 0;

    /// Create flags from the given size.
    ///
    /// - size=1: ChildrenSingle
    /// - size=2: ChildrenPair
    /// - otherwise: no flags set
    static constexpr auto from_size(size_t size) -> ExprFlags {
        auto flags = ExprFlags{};
        if (size == 1) {
            flags |= ExprFlags::ChildrenSingle;
        } else if (size == 2) {
            flags |= ExprFlags::ChildrenPair;
        }

        return flags;
    }

    /// Is ChildrenSingle set?
    [[nodiscard]] constexpr auto is_single() const -> bool {
        return value & ChildrenSingle;
    }

    /// Is ChildrenPair set?
    [[nodiscard]] constexpr auto is_pair() const -> bool {
        return value & ChildrenPair;
    }

    constexpr auto operator|(ExprFlags::Bits const& r) const -> ExprFlags {
        return {static_cast<uint8_t>(value | r)};
    }

    constexpr auto operator|=(ExprFlags::Bits const& r) -> ExprFlags {
        value |= r;
        return *this;
    }
};

/// An expression in the AST.
///
/// The decl is used for expressions that consume identifiers (ExprKind::Id) to
/// reference what is the declaration after name resolution.
///
/// Expressions support the union-find data-structure to allow non-destructive
/// rewriting of the AST. This is done by the forward field and the find and
/// make_equal_to methods.
///
/// > The union-find structure does not allow cycles, so nodes should be cloned
/// > if needed in multiple places.
///
/// Children are optimized for beeing kept inline if there are less than 3. The
/// flags control what layout to use.
///
/// - ChildrenSingle: children_as.single is active.
/// - ChildrenPair: children_as.pair is active.
/// - otherwise: children_as.ref is active.
///
/// What fields mean depends on kind.
///
/// - Neg, Add, Sub, Mul, Div, Mod: just two children for lhs and rhs of
///   operation.
/// - Id: No children, string_value has the identifier.
/// - Int: No children, int_value has the value.
/// - String: No children, string_value has the value.
struct Expr {
    ExprKind  kind;
    ExprFlags flags;
    Location  loc{};

    Decl* decl{};

    union {
        std::array<Expr*, 1> single;
        std::array<Expr*, 2> pair;
        std::span<Expr*>     ref;
    } children_as{};

    Expr* forward{};

    // NOLINTNEXTLINE(readability-redundant-member-init)
    std::string_view string_value{};
    uint64_t         int_value{};

    /// Get the left child (the first one).
    [[nodiscard]] constexpr auto lhs() const -> Expr* { return at(0); }

    /// Get the right child (the second one).
    [[nodiscard]] constexpr auto rhs() const -> Expr* { return at(1); }

    /// Get all of the children.
    [[nodiscard]] constexpr auto children() const -> std::span<Expr* const> {
        if (flags.is_single()) return children_as.single;
        if (flags.is_pair()) return children_as.pair;
        return children_as.ref;
    }

    /// Get the children at a given index.
    [[nodiscard]] constexpr auto at(size_t idx) const -> Expr* {
        return children()[idx];
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() const -> Expr const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() -> Expr* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The union operation of union find.
    constexpr void make_equal_to(Expr* other) {
        auto n = find();
        if (n != other) n->forward = other;
    }
};

/// All of the variants of statements.
enum struct StmtKind : uint8_t {
    Err,

    Block,
    Return,
    Expr,

    Var,
    MultiVar,
};

/// Data specially for a variable declaration statement.
///
/// Contains the list of names beeing declared as an array of Expr of kind
/// ExprKind::Id. We store the full expression node of an Id so that we have an
/// actual loc and decl for each.
///
/// Contains the list of type expressions and initializers. We can only
/// correctly match the lengths of all three after typing.
struct MultiVarStmt {
    // TODO: optimize for the case where we have less than 3 items in the span.
    std::span<Expr*> names;
    std::span<Expr*> types;
    std::span<Expr*> inits;
};

/// Flags for controlling layout of data inside of a statement.
struct StmtFlags {
    enum Bits {
        None = 0,
        ChildrenSingle = 1,
        ChildrenPair = 2,
    };

    uint8_t value = 0;

    /// Create flags from the given size.
    ///
    /// - size=1: ChildrenSingle
    /// - size=2: ChildrenPair
    /// - otherwise: no flags set
    static constexpr auto from_size(size_t size) -> StmtFlags {
        auto flags = StmtFlags{};
        if (size == 1) {
            flags |= StmtFlags::ChildrenSingle;
        } else if (size == 2) {
            flags |= StmtFlags::ChildrenPair;
        }

        return flags;
    }

    /// Is ChildrenSingle set?
    [[nodiscard]] constexpr auto is_single() const -> bool {
        return value & ChildrenSingle;
    }

    /// Is ChildrenPair set?
    [[nodiscard]] constexpr auto is_pair() const -> bool {
        return value & ChildrenPair;
    }

    constexpr auto operator|(StmtFlags::Bits const& r) const -> StmtFlags {
        return {static_cast<uint8_t>(value | r)};
    }

    constexpr auto operator|=(StmtFlags::Bits const& r) -> StmtFlags {
        value |= r;
        return *this;
    }
};

/// A statement in the AST.
///
/// The decl is used for statements that declare identifiers (StmtKind::Var) to
/// reference what was declared after name resolution.
///
/// Statements support the union-find data-structure to allow non-destructive
/// rewriting of the AST. This is done by the forward field and the find and
/// make_equal_to methods.
///
/// > The union-find structure does not allow cycles, so nodes should be cloned
/// > if needed in multiple places.
///
/// Children are optimized for beeing kept inline if there are less than 3. The
/// flags control what layout to use.
///
/// - ChildrenSingle: children_as.single is active.
/// - ChildrenPair: children_as.pair is active.
/// - otherwise: children_as.ref is active.
///
/// Wether stmts or exprs are used depends on kind.
///
/// - Block: uses stmts
/// - Return, Expr: uses exprs
/// - Variable declarations: Var and MultiVar, see below.
///
/// #### Var
///
/// ```yal
/// func f() {
///     var x0: s32 = 10; // (1)
///     var x1: s32;      // (2)
///     var x2      = 10; // (3)
/// }
/// ```
///
/// value_loc is set to the location of the name and string_value to the name
/// itself.
///
/// - (1): Children are the two expressions for s32 and 10.
///
///     (Var "x0"
///       (Id "s32")
///       (Int 10))
///
/// - (2): Children are the expression for s32 and nullptr.
///
///     (Var "x1"
///       (Id "s32")
///       #nullptr#)
///
/// - (3): Children are nullptr and the expression for 10.
///
///     (Var "x2"
///       #nullptr#
///       (Int 10))
///
/// #### MultiVar
///
/// ```yal
/// func f() {
///     var x0, x1: s32 = 10, 11; // (1)
///     var x2, x3, x4: s32;      // (2)
/// }
/// ```
///
/// Use the special field var_stmt in children_as.
///
/// - (1) var_stmt.names has 2 names, one for each variable. var_stmt.types has
///   a single expression for s32. var_stmt.inits has two expressions for 10
///   and 11.
///
///     (MultiVar
///       names:
///         (Id "x0")
///         (Id "x1")
///       types:
///         (Id "s32")
///       inits:
///         (Int 10)
///         (Int 11))
///
/// - (2) var_stmt.names has 3 names, one for each variable. var_stmt.types has
/// a single expression for s32. var_stmt.inits has no expressions.
///
///     (MultiVar
///       names:
///         (Id "x2")
///         (Id "x3")
///         (Id "x4")
///       types:
///         (Id "s32"))
struct Stmt {
    StmtKind  kind;
    StmtFlags flags;
    Location  loc{};
    Location  value_loc{};

    Decl* decl{};

    union {
        struct {
            union {
                std::array<Stmt*, 1> stmts;
                std::array<Expr*, 1> exprs;
            };
        } single;
        struct {
            union {
                std::array<Stmt*, 2> stmts;
                std::array<Expr*, 2> exprs;
            };
        } pair;
        struct {
            union {
                std::span<Stmt*> stmts;
                std::span<Expr*> exprs;
            };
        } ref;
        MultiVarStmt* var_stmt;
    } children_as{};

    // NOLINTNEXTLINE(readability-redundant-member-init)
    std::string_view string_value{};

    Stmt* forward{};

    /// Get the children as expressions.
    [[nodiscard]] constexpr auto expr_children() const
        -> std::span<Expr* const> {
        if (flags.is_single()) return children_as.single.exprs;
        if (flags.is_pair()) return children_as.pair.exprs;
        return children_as.ref.exprs;
    }

    /// Get the children as statements.
    [[nodiscard]] constexpr auto stmt_children() const
        -> std::span<Stmt* const> {
        if (flags.is_single()) return children_as.single.stmts;
        if (flags.is_pair()) return children_as.pair.stmts;
        return children_as.ref.stmts;
    }

    /// Get the expression children at a given index
    [[nodiscard]] constexpr auto expr_at(size_t idx) const -> Expr* {
        return expr_children()[idx];
    }

    /// Get the statement children at a given index
    [[nodiscard]] constexpr auto stmt_at(size_t idx) const -> Stmt* {
        return stmt_children()[idx];
    }

    /// Get the MultiVarStmt data.
    [[nodiscard]] constexpr auto multi_var_stmt() const -> MultiVarStmt* {
        return children_as.var_stmt;
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() const -> Stmt const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The find operation of union find.
    [[nodiscard]] constexpr auto find() -> Stmt* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    /// The union operation of union find.
    constexpr void make_equal_to(Stmt* other) {
        auto n = find();
        if (n != other) n->forward = other;
    }
};

void to_json(nlohmann::json& j, ExprKind const& n);
void to_json(nlohmann::json& j, StmtKind const& n);

void to_json(nlohmann::json& j, Expr const& n);
void to_json(nlohmann::json& j, Stmt const& n);

void to_lisp(fmt::format_context& ctx, Expr const& expr, int depth = 0);
void to_lisp(fmt::format_context& ctx, Stmt const& stmt, int depth = 0);

// ============================================================================
// OLD stuff
// ============================================================================

enum class NodeKind {
    Err,

    FlatModule,

    File,
    Attribute,
    AttributeKV,
    Var,
    Def,
    Func,

    Block,
    Return,
    ExprStmt,

    Neg,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Id,
    Int,
    String,

    NodePack,
    FuncArg,
    FuncNamedRet,
};

// ----------------------------------------------------------------------------

// fwd

class NodeErr;
class NodeFlatModule;
class NodeFile;
class NodeAttribute;
class NodeAttributeKV;
class NodeVar;
class NodeDef;
class NodeFunc;
class NodeBlock;
class NodeExprStmt;
class NodeUnaryExpr;
class NodeBinaryExpr;
class NodeId;
class NodeInt;
class NodePack;
class NodeFuncArg;
class NodeFuncNamedRet;

// ----------------------------------------------------------------------------

// Base interface for all AST nodes.
class Node {
    NodeKind kind{};
    Location loc{};

    // In case this is a defining node (like Func or Var), then this points to
    // the Decl that it defines. In case this is a refering node (Id), then this
    // points to the Decl that originated it.
    Decl* decl{};

    // Used for union-find
    Node* forward{};

public:
    constexpr Node(NodeKind kind, Location loc) : kind{kind}, loc{loc} {}

    [[nodiscard]] constexpr auto get_kind() const -> NodeKind { return kind; }
    [[nodiscard]] constexpr auto get_loc() const -> Location { return loc; }
    [[nodiscard]] constexpr auto get_decl() const -> Decl* { return decl; }

    constexpr void set_decl(Decl* new_decl) { decl = new_decl; }

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == NodeKind::Err;
    }

    // In case this node has a forwarding pointer, i.e was set to another using
    // union-find, process the chain and return the latest. In case the node is
    // not forwarded, returns itself.
    [[nodiscard]] constexpr auto find() const -> Node const* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    [[nodiscard]] constexpr auto find() -> Node* {
        auto n = this;
        while (n->forward) n = n->forward;
        return n;
    }

    // Set the forwarding pointer to the given node.
    //
    // NOTE: this can cause cycles, so duplicate the node when in doubt.
    constexpr void make_equal_to(Node* n) {
        auto found = find();
        if (found != n) found->forward = n;
    }

    // Get all children of a node. The default implementation returns an empty
    // span.
    [[nodiscard]] virtual auto get_children() const -> std::span<Node* const> {
        return {};
    }

    // This is to allow formatting from the base class. There is a default
    // implementation that just prints kind and location.
    virtual auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator;

    // Convert the node to json.
    virtual void to_json(nlohmann::json& j) const;

    // Convert just common values to json (this overwrites the JSON object).
    void to_json_common_values(nlohmann::json& j) const;

    // Convert just children to json (this does not overwrite the JSON object,
    // just adds the 'children' key.
    void to_json_children(nlohmann::json& j) const;
};

// Error node.
class NodeErr : public Node {
public:
    constexpr NodeErr(Location loc) : Node{NodeKind::Err, loc} {}
};

// ----------------------------------------------------------------------------

class NodePack : public Node {
    std::span<Node*> children;

public:
    constexpr NodePack(Location loc, std::span<Node*> children)
        : Node{NodeKind::NodePack, loc}, children{children} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// ----------------------------------------------------------------------------

class NodeFuncArg : public Node {
    std::string_view name;
    Node*            type;

public:
    constexpr NodeFuncArg(Location loc, std::string_view name, Node* type)
        : Node{NodeKind::FuncArg, loc}, name{name}, type{type} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_type() const -> Node* { return type; }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&type, 1};
    }
};

class NodeFuncNamedRet : public Node {
    std::string_view name;
    Node*            type;

public:
    constexpr NodeFuncNamedRet(Location loc, std::string_view name, Node* type)
        : Node{NodeKind::FuncNamedRet, loc}, name{name}, type{type} {}

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_type() const -> Node* { return type; }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&type, 1};
    }
};

// ============================================================================

// The top-level of a parsed file, with all declarations and the module name.
class NodeFlatModule : public Node {
    std::span<Node*> children;
    std::string_view module_name;

public:
    constexpr NodeFlatModule(Location loc, std::span<Node*> children,
                             std::string_view module_name)
        : Node{NodeKind::FlatModule, loc},
          children{children},
          module_name{module_name} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// The top-level of a parsed file, with all declarations and the module name.
class NodeFile : public Node {
    std::span<Node*> children;
    std::string_view module_name;

public:
    constexpr NodeFile(Location loc, std::span<Node*> children,
                       std::string_view module_name)
        : Node{NodeKind::File, loc},
          children{children},
          module_name{module_name} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }

    [[nodiscard]] auto get_module_name() const -> std::string_view {
        return module_name;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// ============================================================================

// An attribute that is attached to some declaration. It supports both styles:
//
//     @test
//     ^^^^\_ name
//
//     @test_module.test
//     ^^^^^^^^^^^\ ^^^\_ name
//                 \_ qualified_name
class NodeAttribute : public Node {
    std::string_view qualified_name;
    std::string_view name;

    // A list of all arguments given in the attribute. Key-Value pairs are
    // represented by `AttributeKV`.
    std::span<Node*> args;

public:
    constexpr NodeAttribute(Location loc, std::string_view qualified_name,
                            std::string_view name, std::span<Node*> args)
        : Node{NodeKind::Attribute, loc},
          qualified_name{qualified_name},
          name{name},
          args{args} {}

    [[nodiscard]] constexpr auto get_qualified_name() const
        -> std::string_view {
        return qualified_name;
    }

    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return args;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

class NodeAttributeKV : public Node {
    std::string_view key;
    Node*            value{};

public:
    constexpr NodeAttributeKV(Location loc, std::string_view key, Node* value)
        : Node{NodeKind::AttributeKV, loc}, key{key}, value{value} {}

    [[nodiscard]] constexpr auto get_value() const -> Node* { return value; }
    [[nodiscard]] constexpr auto get_key() const -> std::string_view {
        return key;
    }

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&value, 1};
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// ============================================================================

// Top-level (global) variable declaration or local variable declaration. Both
// use the exact same node.
class NodeVar : public Node {
    std::array<Node*, 4> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeVar(Location loc, Node* attributes, Node* names, Node* types,
                      Node* inits)
        : Node{NodeKind::Var, loc}, children{names, types, inits, attributes} {}

    // Get attributes attached to the variable. May be null (in case of no
    // attributes).
    //
    //     @attribute var a, b, c: ta, tb, tc = va, vb, vc;
    //     ^^^^^^^^^^
    [[nodiscard]] auto get_attributes() const -> NodePack*;

    // Get the names that are declared. This is a pack of ids, one for each
    // value declared. May be null (in case of parse errors).
    //
    //     var a, b, c: ta, tb, tc = va, vb, vc;
    //         ^^^^^^^
    [[nodiscard]] auto get_names() const -> NodePack*;

    // Get the types of the values declared. This is a pack of exprs, one for
    // each explicit type. May be null.
    //
    //     var a, b, c: ta, tb, tc = va, vb, vc;
    //                  ^^^^^^^^^^
    [[nodiscard]] auto get_types() const -> NodePack*;

    // Get the initializers of the values declared. This is a pack of exprs, one
    // for each initializer. May be null.
    //
    //     var a, b, c: ta, tb, tc = va, vb, vc;
    //                               ^^^^^^^^^^
    [[nodiscard]] auto get_inits() const -> NodePack*;

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// Top-level (global) constant declaration or local constant declaration. Both
// use the exact same node.
class NodeDef : public Node {
    std::array<Node*, 5> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeDef(Location loc, Node* attributes, Node* gargs, Node* names,
                      Node* types, Node* inits)
        : Node{NodeKind::Def, loc},
          children{names, types, inits, attributes, gargs} {}

    // Get attributes attached to the definition. May be null (in case of no
    // attributes).
    //
    //     @attribute def a, b, c: ta, tb, tc = va, vb, vc;
    //     ^^^^^^^^^^
    [[nodiscard]] auto get_attributes() const -> NodePack*;

    // Get attributes attached to the definition. May be null (in case of no
    // attributes).
    //
    //     def S[T]: ta = va;
    //          ^^^
    [[nodiscard]] auto get_gargs() const -> NodePack*;

    // Get the names that are declared. This is a pack of ids, one for each
    // value declared. May be null (in case of parse errors).
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //         ^^^^^^^
    [[nodiscard]] auto get_names() const -> NodePack*;

    // Get the types of the values declared. This is a pack of exprs, one for
    // each explicit type. May be null.
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //                  ^^^^^^^^^^
    [[nodiscard]] auto get_types() const -> NodePack*;

    // Get the initializers of the values declared. This is a pack of exprs, one
    // for each initializer. May be null.
    //
    //     def a, b, c: ta, tb, tc = va, vb, vc;
    //                               ^^^^^^^^^^
    [[nodiscard]] auto get_inits() const -> NodePack*;

    void to_json(nlohmann::json& j) const override;

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// ============================================================================

// A direct function definition (witout namespacing or generics).
//
//     @attribute
//     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
//          ^~~~^~~~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~ ^~~~~~~~~~~~ ^~
//             |          |                          |            |  |
//             |          |                          |            |  \_ body
//             |          |                          |            |
//             |          |                          |            \_ ret
//             |          |                          |
//             |          |                          \_ args
//             |          |
//             |          \_ gargs
//             \_ name
class NodeFunc : public Node {
    std::string_view     name;
    std::string_view     attached_type;
    std::array<Node*, 5> children;

    Span name_span;
    Span attached_type_span;

    bool is_c_varargs;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeFunc(Location loc, Node* attributes, std::string_view name,
                       Span name_span, std::string_view attached_type,
                       Span attached_type_span, Node* gargs, Node* args,
                       Node* ret, Node* body, bool is_c_varargs)
        : Node{NodeKind::Func, loc},
          name{name},
          attached_type{attached_type},
          children{attributes, gargs, args, ret, body},
          name_span{name_span},
          attached_type_span{attached_type_span},
          is_c_varargs{is_c_varargs} {}

    // Get the name of the function.
    //
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //          ^^^^
    [[nodiscard]] constexpr auto get_name() const -> std::string_view {
        return name;
    }

    [[nodiscard]] constexpr auto get_name_span() const -> Span {
        return name_span;
    }

    // Get the namespace name of the function, for when it is attached to a
    // type.
    //
    //     func test.test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //          ^^^^
    [[nodiscard]] constexpr auto get_attached_type() const -> std::string_view {
        return attached_type;
    }

    [[nodiscard]] constexpr auto get_attached_type_span() const -> Span {
        return attached_type_span;
    }

    // Get attributes attached to the definition. May be null (in case of no
    // attributes).
    //
    //     @attribute
    //     ^^^^^^^^^^
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    [[nodiscard]] auto get_attributes() const -> NodePack*;

    // Get generic arguments. May be null (in case of no generic arguments).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //              ^^^^^^^^^^^
    [[nodiscard]] auto get_gargs() const -> NodePack*;

    // Get arguments. May be null (in case of no arguments).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    [[nodiscard]] auto get_args() const -> NodePack*;

    // Get the return values. May be null (in case of no return values).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //                                                     ^^^^^^^^^^^^
    [[nodiscard]] auto get_ret() const -> NodePack*;

    // Get the function body. May be null (in case of no return values).
    //
    //     @attribute
    //     func test[T: any, V](argc: i32, argv: [*][*]u8) (i32, usize) {}
    //                                                     ^^^^^^^^^^^^
    [[nodiscard]] auto get_body() const -> NodeBlock*;

    // If the function has the '...' for c-style variable length arguments.
    [[nodiscard]] constexpr auto get_is_c_varargs() const -> bool {
        return is_c_varargs;
    }

    // ------------------------------------------------------------------------

    constexpr void set_name(std::string_view new_name) { name = new_name; }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// ============================================================================

// A node for a code block.
class NodeBlock : public Node {
    std::span<Node*> children;

public:
    constexpr NodeBlock(Location loc, std::span<Node*> children)
        : Node{NodeKind::Block, loc}, children{children} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// A node for a return statement.
class NodeExprStmt : public Node {
    Node* child;

public:
    constexpr NodeExprStmt(Location loc, Node* child)
        : Node{NodeKind::ExprStmt, loc}, child{child} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&child, 1};
    }
};

// A node for a return statement.
class NodeReturn : public Node {
    std::span<Node*> values;

public:
    constexpr NodeReturn(Location loc, std::span<Node*> values)
        : Node{NodeKind::Return, loc}, values{values} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return values;
    }
};

// ============================================================================

class NodeUnaryExpr : public Node {
    Node* child;

public:
    constexpr NodeUnaryExpr(NodeKind kind, Location loc, Node* child)
        : Node{kind, loc}, child{child} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return {&child, 1};
    }
};

class NodeBinaryExpr : public Node {
    std::array<Node*, 2> children;

    [[nodiscard]] constexpr auto child_at(size_t idx) const -> Node* {
        return children[idx];
    }

public:
    constexpr NodeBinaryExpr(NodeKind kind, Location loc, Node* lhs, Node* rhs)
        : Node{kind, loc}, children{lhs, rhs} {}

    [[nodiscard]] auto get_children() const -> std::span<Node* const> override {
        return children;
    }
};

// ============================================================================

// A node for an identifier.
class NodeId : public Node {
    std::string_view id;

public:
    constexpr NodeId(Location loc, std::string_view id)
        : Node{NodeKind::Id, loc}, id{id} {}

    [[nodiscard]] constexpr auto get_value() const -> std::string_view {
        return id;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// A node for an integer literal of any base.
class NodeInt : public Node {
    uint64_t value{};

public:
    constexpr NodeInt(Location loc, uint64_t value)
        : Node{NodeKind::Int, loc}, value{value} {}

    [[nodiscard]] constexpr auto get_value() const -> uint64_t { return value; }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

// A node for a string literal.
class NodeString : public Node {
    std::string_view value;

public:
    constexpr NodeString(Location loc, std::string_view value)
        : Node{NodeKind::String, loc}, value{value} {}

    [[nodiscard]] constexpr auto get_value() const -> std::string_view {
        return value;
    }

    auto format_to(fmt::format_context& ctx) const
        -> fmt::format_context::iterator override;

    void to_json(nlohmann::json& j) const override;
};

void to_json(nlohmann::json& j, NodeKind const& n);
void to_json(nlohmann::json& j, Node const& t);

}  // namespace yal::ast

define_formatter_from_string_view(yal::ast::ExprKind);
define_formatter_from_string_view(yal::ast::StmtKind);

define_formatter_from_string_view(yal::ast::Expr);
define_formatter_from_string_view(yal::ast::Stmt);

// ============================================================================
// OLD stuff
// ============================================================================

define_formatter_from_string_view(yal::ast::NodeKind);

#define define_formatter_from_string_view_for_virtual(T)   \
                                                           \
    template <>                                            \
    struct fmt ::formatter<T> : formatter<string_view> {   \
        auto format(T const& p, format_context& ctx) const \
            -> format_context ::iterator {                 \
            return p.format_to(ctx);                       \
        }                                                  \
    }

define_formatter_from_string_view_for_virtual(yal::ast::Node);

define_formatter_from_string_view_for_virtual(yal::ast::NodeErr);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFile);
define_formatter_from_string_view_for_virtual(yal::ast::NodeAttribute);
define_formatter_from_string_view_for_virtual(yal::ast::NodeAttributeKV);
define_formatter_from_string_view_for_virtual(yal::ast::NodeVar);
define_formatter_from_string_view_for_virtual(yal::ast::NodeDef);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFunc);
define_formatter_from_string_view_for_virtual(yal::ast::NodeBlock);
define_formatter_from_string_view_for_virtual(yal::ast::NodeId);
define_formatter_from_string_view_for_virtual(yal::ast::NodeInt);
define_formatter_from_string_view_for_virtual(yal::ast::NodePack);
define_formatter_from_string_view_for_virtual(yal::ast::NodeFuncArg);

#undef define_formatter_from_string_view_for_virtual
