#pragma once

#include <cstddef>
#include <cstdint>
#include <string_view>

#include "ast-node.hpp"
#include "ast.hpp"
#include "decl-store.hpp"

namespace yal::ast::conv {

struct Id {
    std::string_view name;
    Decl*            to;
};

struct KwLit {
    std::string_view id;
};

struct Int {
    uint64_t value;
};

struct Double {
    double value;
};

struct Float {
    float value;
};

struct Str {
    std::string_view value;
};

struct Char {
    uint32_t value;
};

struct Module {
    std::string_view name;
    std::span<Node*> children;
};

struct SourceFile {
    Node*            mod{};
    std::span<Node*> children;
};

struct ModuleDecl {
    std::string_view name;
};

struct Decorator {
    std::string_view name;
    std::span<Node*> params;
};

struct DecoratorParam {
    std::string_view key;
    Node*            value{};
};

struct Decorators {
    std::span<Node*> items;
};

struct FuncParams {
    std::span<Node*> params;
};

struct IdPack {
    std::span<Node*> ids;
};

struct FuncRetPack {
    std::span<Node*> ret;
};

struct FuncDecl {
    Node* decorators;
    Node* name;
    Node* gargs;
    Node* args;
    Node* ret;
    Node* body{};
    bool  is_c_varargs;

    [[nodiscard]] constexpr auto get_decorators() const -> Decorators;
    [[nodiscard]] constexpr auto get_name() const -> IdPack;
    [[nodiscard]] constexpr auto get_gargs() const -> FuncParams;
    [[nodiscard]] constexpr auto get_args() const -> FuncParams;
    [[nodiscard]] constexpr auto get_ret() const -> FuncRetPack;
};

struct ExprPack {
    std::span<Node*> items;
};

struct VarDecl {
    Node* ids;
    Node* types;
    Node* inits;

    [[nodiscard]] constexpr auto get_ids() const -> IdPack;
    [[nodiscard]] constexpr auto get_types() const -> ExprPack;
    [[nodiscard]] constexpr auto get_inits() const -> ExprPack;
};

struct DefDecl {
    Node* ids;
    Node* types;
    Node* inits;

    [[nodiscard]] constexpr auto get_ids() const -> IdPack;
    [[nodiscard]] constexpr auto get_types() const -> ExprPack;
    [[nodiscard]] constexpr auto get_inits() const -> ExprPack;
};

struct TopVarDecl {
    Node* decorators;
    Node* decl;

    [[nodiscard]] constexpr auto get_decorators() const -> Decorators;
    [[nodiscard]] constexpr auto get_decl() const -> VarDecl;
};

struct TopDefDecl {
    Node* decorators;
    Node* decl;

    [[nodiscard]] constexpr auto get_decorators() const -> Decorators;
    [[nodiscard]] constexpr auto get_decl() const -> DefDecl;
};

struct FuncParam {
    std::string_view name;
    Node*            type{};
};

struct NamedRet {
    std::string_view name;
    Node*            type{};
};

struct ImportStmt {
    std::string_view path;
};

struct Binary {
    Node* lhs{};
    Node* rhs{};
};

struct Unary {
    Node* child{};
};

struct StructType {
    std::span<Node*> fields;
};

struct StructField {
    std::string_view name;
    Node*            type{};
    Node*            init{};
};

struct Ptr {
    Node* inner{};
    bool  is_const;
};

struct MultiPtr {
    Node* inner{};
    bool  is_const;
};

struct Slice {
    Node* inner{};
    bool  is_const;
};

struct ArrayType {
    Node* size{};
    Node* inner{};
    bool  is_const;
};

struct Array {
    Node*            size{};
    Node*            inner{};
    std::span<Node*> items;
};

struct Lit {
    std::span<Node*> items;
};

struct LitParam {
    std::string_view key;
    Node*            init{};
};

struct Call {
    Node*            callee{};
    std::span<Node*> args;
};

struct Field {
    std::string_view name;
    Node*            receiver{};
};

struct Block {
    std::span<Node*> items;
};

struct ExprStmt {
    Node* child{};
};

struct ReturnStmt {
    Node* child{};
};

struct IfStmt {
    Node* cond{};
    Node* wt{};
    Node* wf{};
};

struct WhileStmt {
    Node* cond{};
    Node* body{};
};

struct DeferStmt {
    Node* stmt{};
};

struct Assign {
    Node* lhs;
    Node* rhs;

    [[nodiscard]] constexpr auto get_lhs() const -> ExprPack;
    [[nodiscard]] constexpr auto get_rhs() const -> ExprPack;
};

struct Coerce {
    Node*        child{};
    types::Type* target{};
};

// ------------------------------------------------------------------

[[nodiscard]] constexpr auto id(Node const& n) -> Id {
    ASSERT(n.get_kind() == NodeKind::Id);
    return {.name = n.get_data_str(), .to = n.get_decl()};
}

[[nodiscard]] constexpr auto kwlit(Node const& n) -> KwLit {
    ASSERT(n.get_kind() == NodeKind::KwLit);
    return {.id = n.get_data_str()};
}

[[nodiscard]] constexpr auto integers(Node const& n) -> Int {
    ASSERT(n.get_kind() == NodeKind::Int);
    return {.value = n.get_data_u64()};
}

[[nodiscard]] constexpr auto float64(Node const& n) -> Double {
    ASSERT(n.get_kind() == NodeKind::Double);
    return {.value = n.get_data_f64()};
}

[[nodiscard]] constexpr auto float32(Node const& n) -> Float {
    ASSERT(n.get_kind() == NodeKind::Float);
    return {.value = n.get_data_f32()};
}

[[nodiscard]] constexpr auto str(Node const& n) -> Str {
    ASSERT(n.get_kind() == NodeKind::Str);
    return {.value = n.get_data_str()};
}

[[nodiscard]] constexpr auto character(Node const& n) -> Char {
    ASSERT(n.get_kind() == NodeKind::Char);
    return {.value = static_cast<uint32_t>(n.get_data_u64())};
}

[[nodiscard]] constexpr auto module(Node const& n) -> Module {
    ASSERT(n.get_kind() == NodeKind::Module);
    return {.name = n.get_data_str(), .children = n.get_children()};
}

[[nodiscard]] constexpr auto source_file(Node const& n) -> SourceFile {
    ASSERT(n.get_kind() == NodeKind::SourceFile);
    return {
        .mod = n.get_child(0),
        .children = n.get_children().subspan(1),
    };
}

[[nodiscard]] constexpr auto module_decl(Node const& n) -> ModuleDecl {
    ASSERT(n.get_kind() == NodeKind::ModuleDecl);
    return {.name = n.get_data_str()};
}

[[nodiscard]] constexpr auto decorator(Node const& n) -> Decorator {
    ASSERT(n.get_kind() == NodeKind::Decorator);
    return {.name = n.get_data_str(), .params = n.get_children()};
}

[[nodiscard]] constexpr auto decorator_param(Node const& n) -> DecoratorParam {
    ASSERT(n.get_kind() == NodeKind::DecoratorParam);
    return {.key = n.get_data_str(), .value = n.get_child(0)};
}

[[nodiscard]] constexpr auto decorators(Node const& n) -> Decorators {
    ASSERT(n.get_kind() == NodeKind::Decorators);
    return {.items = n.get_children()};
}

[[nodiscard]] constexpr auto func_params(Node const& n) -> FuncParams {
    ASSERT(n.get_kind() == NodeKind::FuncParams);
    return {.params = n.get_children()};
}

[[nodiscard]] constexpr auto id_pack(Node const& n) -> IdPack {
    ASSERT(n.get_kind() == NodeKind::IdPack);
    return {.ids = n.get_children()};
}

[[nodiscard]] constexpr auto func_ret_pack(Node const& n) -> FuncRetPack {
    ASSERT(n.get_kind() == NodeKind::FuncRetPack);
    return {.ret = n.get_children()};
}

[[nodiscard]] constexpr auto func_decl(Node const& n) -> FuncDecl {
    ASSERT(n.is_oneof(NodeKind::FuncDecl, NodeKind::FuncDeclWithCVarArgs));
    return {
        .decorators = n.get_child(0),
        .name = n.get_child(1),
        .gargs = n.get_child(2),
        .args = n.get_child(3),
        .ret = n.get_child(4),
        .body = n.get_child(5),
        .is_c_varargs = n.get_kind() == NodeKind::FuncDeclWithCVarArgs,
    };
}

[[nodiscard]] constexpr auto var_decl(Node const& n) -> VarDecl {
    ASSERT(n.get_kind() == NodeKind::VarDecl);
    return {.ids = n.get_child(0),
            .types = n.get_child(1),
            .inits = n.get_child(2)};
}

[[nodiscard]] constexpr auto def_decl(Node const& n) -> DefDecl {
    ASSERT(n.get_kind() == NodeKind::DefDecl);
    return {.ids = n.get_child(0),
            .types = n.get_child(1),
            .inits = n.get_child(2)};
}

[[nodiscard]] constexpr auto top_var_decl(Node const& n) -> TopVarDecl {
    ASSERT(n.get_kind() == NodeKind::TopVarDecl);
    return {
        .decorators = n.get_child(0),
        .decl = n.get_child(1),
    };
}

[[nodiscard]] constexpr auto top_def_decl(Node const& n) -> TopDefDecl {
    ASSERT(n.get_kind() == NodeKind::TopDefDecl);
    return {
        .decorators = n.get_child(0),
        .decl = n.get_child(1),
    };
}

[[nodiscard]] constexpr auto func_param(Node const& n) -> FuncParam {
    ASSERT(n.get_kind() == NodeKind::FuncParam);
    return {.name = n.get_data_str(), .type = n.get_child(0)};
}

[[nodiscard]] constexpr auto named_ret(Node const& n) -> NamedRet {
    ASSERT(n.get_kind() == NodeKind::NamedRet);
    return {.name = n.get_data_str(), .type = n.get_child(0)};
}

[[nodiscard]] constexpr auto import_stmt(Node const& n) -> ImportStmt {
    ASSERT(n.get_kind() == NodeKind::ImportStmt);
    return {.path = n.get_data_str()};
}

[[nodiscard]] constexpr auto expr_pack(Node const& n) -> ExprPack {
    ASSERT(n.get_kind() == NodeKind::ExprPack);
    return {.items = n.get_children()};
}

[[nodiscard]] constexpr auto binary(Node const& n) -> Binary {
    ASSERT(n.is_oneof(
        ast::NodeKind::Add, ast::NodeKind::Sub, ast::NodeKind::Mul,
        ast::NodeKind::Div, ast::NodeKind::Mod, ast::NodeKind::LeftShift,
        ast::NodeKind::RightShift, ast::NodeKind::Equal,
        ast::NodeKind::NotEqual, ast::NodeKind::Less, ast::NodeKind::LessEqual,
        ast::NodeKind::Greater, ast::NodeKind::GreaterEqual,
        ast::NodeKind::Band, ast::NodeKind::Bor, ast::NodeKind::Bxor,
        ast::NodeKind::Land, ast::NodeKind::Lor, ast::NodeKind::Cast));
    return {.lhs = n.get_child(0), .rhs = n.get_child(1)};
}

[[nodiscard]] constexpr auto unary(Node const& n) -> Unary {
    ASSERT(n.is_oneof(NodeKind::AddrOf, NodeKind::Lnot, NodeKind::Bnot,
                      NodeKind::Neg, NodeKind::Optional, NodeKind::ExprStmt,
                      NodeKind::ReturnStmt));
    return {.child = n.get_child(0)};
}

[[nodiscard]] constexpr auto struct_type(Node const& n) -> StructType {
    ASSERT(n.get_kind() == NodeKind::StructType);
    return {.fields = n.get_children()};
}

[[nodiscard]] constexpr auto struct_field(Node const& n) -> StructField {
    ASSERT(n.get_kind() == NodeKind::StructField);
    return {
        .name = n.get_data_str(),
        .type = n.get_child(0),
        .init = n.get_child(1),
    };
}

[[nodiscard]] constexpr auto ptr(Node const& n) -> Ptr {
    ASSERT(n.is_oneof(NodeKind::Ptr, NodeKind::PtrConst));
    return {
        .inner = n.get_child(0),
        .is_const = n.get_kind() == NodeKind::PtrConst,
    };
}

[[nodiscard]] constexpr auto mptr(Node const& n) -> MultiPtr {
    ASSERT(n.is_oneof(NodeKind::MultiPtr, NodeKind::MultiPtrConst));
    return {
        .inner = n.get_child(0),
        .is_const = n.get_kind() == NodeKind::MultiPtrConst,
    };
}

[[nodiscard]] constexpr auto slice(Node const& n) -> Slice {
    ASSERT(n.is_oneof(NodeKind::Slice, NodeKind::SliceConst));
    return {
        .inner = n.get_child(0),
        .is_const = n.get_kind() == NodeKind::SliceConst,
    };
}

[[nodiscard]] constexpr auto array_type(Node const& n) -> ArrayType {
    ASSERT(n.is_oneof(NodeKind::ArrayType, NodeKind::ArrayTypeConst));
    return {
        .size = n.get_child(1),
        .inner = n.get_child(0),
        .is_const = n.get_kind() == NodeKind::ArrayTypeConst,
    };
}

[[nodiscard]] constexpr auto array(Node const& n) -> Array {
    ASSERT(n.get_kind() == NodeKind::Array);
    return {
        .size = n.get_child(1),
        .inner = n.get_child(0),
        .items = n.get_children().subspan(2),
    };
}

[[nodiscard]] constexpr auto lit(Node const& n) -> Lit {
    ASSERT(n.get_kind() == NodeKind::Lit);
    return {.items = n.get_children()};
}

[[nodiscard]] constexpr auto lit_param(Node const& n) -> LitParam {
    ASSERT(n.get_kind() == NodeKind::LitParam);
    return {.key = n.get_data_str(), .init = n.get_child(0)};
}

[[nodiscard]] constexpr auto call(Node const& n) -> Call {
    ASSERT(n.get_kind() == NodeKind::Call);
    return {.callee = n.get_child(0), .args = n.get_children().subspan(1)};
}

[[nodiscard]] constexpr auto field(Node const& n) -> Field {
    ASSERT(n.get_kind() == NodeKind::Field);
    return {.name = n.get_data_str(), .receiver = n.get_child(0)};
}

[[nodiscard]] constexpr auto block(Node const& n) -> Block {
    ASSERT(n.get_kind() == NodeKind::Block);
    return {.items = n.get_children()};
}

[[nodiscard]] constexpr auto if_stmt(Node const& n) -> IfStmt {
    ASSERT(n.get_kind() == NodeKind::IfStmt);
    return {.cond = n.get_child(0), .wt = n.get_child(1), .wf = n.get_child(2)};
}

[[nodiscard]] constexpr auto while_stmt(Node const& n) -> WhileStmt {
    ASSERT(n.get_kind() == NodeKind::WhileStmt);
    return {.cond = n.get_child(0), .body = n.get_child(1)};
}

[[nodiscard]] constexpr auto defer_stmt(Node const& n) -> DeferStmt {
    ASSERT(n.get_kind() == NodeKind::DeferStmt);
    return {.stmt = n.get_child(0)};
}

[[nodiscard]] constexpr auto assign(Node const& n) -> Assign {
    ASSERT(n.is_oneof(ast::NodeKind::Assign, ast::NodeKind::AssignAdd,
                      ast::NodeKind::AssignSub, ast::NodeKind::AssignMul,
                      ast::NodeKind::AssignDiv, ast::NodeKind::AssignMod,
                      ast::NodeKind::AssignShiftLeft,
                      ast::NodeKind::AssignShiftRight,
                      ast::NodeKind::AssignBand, ast::NodeKind::AssignBxor,
                      ast::NodeKind::AssignBor));
    return {
        .lhs = n.get_child(0),
        .rhs = n.get_child(1),
    };
}

[[nodiscard]] constexpr auto coerce(Node const& n) -> Coerce {
    ASSERT(n.is_oneof(ast::NodeKind::Coerce));
    return {
        .child = n.get_child(0),
        .target = n.get_data_type(),
    };
}

// ==================================================================

enum class PrivateKind { Public, Module, File };
[[nodiscard]] constexpr auto decorators_private_kind(Decorators decorators)
    -> PrivateKind {
    for (auto decl_ptr : decorators.items) {
        auto deco = conv::decorator(*decl_ptr);
        if (deco.name == "private") {
            for (auto param : deco.params) {
                if (param->is_oneof(NodeKind::KwLit)) {
                    auto p = conv::kwlit(*param);
                    if (p.id == "file") return PrivateKind::File;
                }
            }

            return PrivateKind::Module;
        }
    }

    return PrivateKind::Public;
}

// ==================================================================

constexpr auto FuncDecl::get_decorators() const -> Decorators {
    return conv::decorators(*decorators);
}

constexpr auto FuncDecl::get_name() const -> IdPack {
    return conv::id_pack(*name);
}

constexpr auto FuncDecl::get_gargs() const -> FuncParams {
    return conv::func_params(*gargs);
}

constexpr auto FuncDecl::get_args() const -> FuncParams {
    return conv::func_params(*args);
}

constexpr auto FuncDecl::get_ret() const -> FuncRetPack {
    return ret ? conv::func_ret_pack(*ret) : FuncRetPack{};
}

constexpr auto VarDecl::get_ids() const -> IdPack {
    return conv::id_pack(*ids);
}

constexpr auto VarDecl::get_types() const -> ExprPack {
    return conv::expr_pack(*types);
}

constexpr auto VarDecl::get_inits() const -> ExprPack {
    return conv::expr_pack(*inits);
}

constexpr auto DefDecl::get_ids() const -> IdPack {
    return conv::id_pack(*ids);
}

constexpr auto DefDecl::get_types() const -> ExprPack {
    return conv::expr_pack(*types);
}

constexpr auto DefDecl::get_inits() const -> ExprPack {
    return conv::expr_pack(*inits);
}

constexpr auto TopVarDecl::get_decorators() const -> Decorators {
    return conv::decorators(*decorators);
}

constexpr auto TopVarDecl::get_decl() const -> VarDecl {
    return conv::var_decl(*decl);
}

constexpr auto TopDefDecl::get_decorators() const -> Decorators {
    return conv::decorators(*decorators);
}

constexpr auto TopDefDecl::get_decl() const -> DefDecl {
    return conv::def_decl(*decl);
}

constexpr auto Assign::get_lhs() const -> ExprPack {
    return conv::expr_pack(*lhs);
}
constexpr auto Assign::get_rhs() const -> ExprPack {
    return conv::expr_pack(*rhs);
}

}  // namespace yal::ast::conv
