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
    DeclId           to;
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
    Decorators  decorators;
    IdPack      name;
    FuncParams  gargs;
    FuncParams  args;
    FuncRetPack ret;
    Node*       body{};
    bool        is_c_varargs;

    struct {
        Node* decorators;
        Node* name;
        Node* gargs;
        Node* args;
        Node* ret;
    } detail;
};

struct VarDecl {
    Node* ids;
    Node* types;
    Node* inits;
};

struct DefDecl {
    Node* ids;
    Node* types;
    Node* inits;
};

struct TopVarDecl {
    Decorators decorators;
    VarDecl    decl;

    struct {
        Node* decorators;
        Node* decl;
    } detail;
};

struct TopDefDecl {
    Decorators decorators;
    DefDecl    decl;

    struct {
        Node* decorators;
        Node* decl;
    } detail;
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

struct ExprPack {
    std::span<Node*> items;
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
    ExprPack lhs;
    ExprPack rhs;

    struct {
        Node* lhs{};
        Node* rhs{};
    } detail;
};

// ------------------------------------------------------------------

[[nodiscard]] constexpr auto id(Node const& n) -> Id {
    ASSERT(n.get_kind() == NodeKind::Id);
    return {.name = n.get_data_str(), .to = n.get_declid()};
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
        .mod = n.get_children()[0],
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
    return {.key = n.get_data_str(), .value = n.get_children()[0]};
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
    ASSERT(n.get_kind() == NodeKind::FuncDecl);
    auto c = n.get_children();

    return {
        .decorators = conv::decorators(*c[0]),
        .name = conv::id_pack(*c[1]),
        .gargs = conv::func_params(*c[2]),
        .args = conv::func_params(*c[3]),
        .ret = c[4] ? conv::func_ret_pack(*c[4]) : FuncRetPack{},
        .body = c[5],
        .is_c_varargs = n.get_kind() == NodeKind::FuncDeclWithCVarArgs,
        .detail = {.decorators = c[0],  //
                   .name = c[1],        //
                   .gargs = c[2],       //
                   .args = c[3],        //
                   .ret = c[4]}
    };
}

[[nodiscard]] constexpr auto var_decl(Node const& n) -> VarDecl {
    ASSERT(n.get_kind() == NodeKind::VarDecl);
    auto c = n.get_children();
    return {.ids = c[0], .types = c[1], .inits = c[2]};
}

[[nodiscard]] constexpr auto def_decl(Node const& n) -> DefDecl {
    ASSERT(n.get_kind() == NodeKind::DefDecl);
    auto c = n.get_children();
    return {.ids = c[0], .types = c[1], .inits = c[2]};
}

[[nodiscard]] constexpr auto top_var_decl(Node const& n) -> TopVarDecl {
    ASSERT(n.get_kind() == NodeKind::TopVarDecl);
    auto c = n.get_children();

    return {
        .decorators = conv::decorators(*c[0]),
        .decl = conv::var_decl(*c[1]),
        .detail = {.decorators = c[0], .decl = c[1]},
    };
}

[[nodiscard]] constexpr auto top_def_decl(Node const& n) -> TopDefDecl {
    ASSERT(n.get_kind() == NodeKind::TopDefDecl);
    auto c = n.get_children();

    return {
        .decorators = conv::decorators(*c[0]),
        .decl = conv::def_decl(*c[1]),
        .detail = {.decorators = c[0], .decl = c[1]},
    };
}

[[nodiscard]] constexpr auto func_param(Node const& n) -> FuncParam {
    ASSERT(n.get_kind() == NodeKind::FuncParam);
    return {.name = n.get_data_str(), .type = n.get_children()[0]};
}

[[nodiscard]] constexpr auto named_ret(Node const& n) -> NamedRet {
    ASSERT(n.get_kind() == NodeKind::NamedRet);
    return {.name = n.get_data_str(), .type = n.get_children()[0]};
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
    // TODO: assert that it is a valid binary
    auto c = n.get_children();
    return {.lhs = c[0], .rhs = c[1]};
}

[[nodiscard]] constexpr auto unary(Node const& n) -> Unary {
    // TODO: assert that it is a valid unary
    return {.child = n.get_children()[0]};
}

[[nodiscard]] constexpr auto struct_type(Node const& n) -> StructType {
    ASSERT(n.get_kind() == NodeKind::StructType);
    return {.fields = n.get_children()};
}

[[nodiscard]] constexpr auto struct_field(Node const& n) -> StructField {
    ASSERT(n.get_kind() == NodeKind::StructField);
    auto c = n.get_children();
    return {.name = n.get_data_str(), .type = c[0], .init = c[1]};
}

[[nodiscard]] constexpr auto ptr(Node const& n) -> Ptr {
    ASSERT(n.get_kind() == NodeKind::Ptr || n.get_kind() == NodeKind::PtrConst);
    return {
        .inner = n.get_children()[0],
        .is_const = n.get_kind() == NodeKind::PtrConst,
    };
}

[[nodiscard]] constexpr auto mptr(Node const& n) -> MultiPtr {
    ASSERT(n.get_kind() == NodeKind::MultiPtr ||
           n.get_kind() == NodeKind::MultiPtrConst);
    return {
        .inner = n.get_children()[0],
        .is_const = n.get_kind() == NodeKind::MultiPtrConst,
    };
}

[[nodiscard]] constexpr auto slice(Node const& n) -> Slice {
    ASSERT(n.get_kind() == NodeKind::Slice ||
           n.get_kind() == NodeKind::SliceConst);
    return {
        .inner = n.get_children()[0],
        .is_const = n.get_kind() == NodeKind::SliceConst,
    };
}

[[nodiscard]] constexpr auto array_type(Node const& n) -> ArrayType {
    ASSERT(n.get_kind() == NodeKind::ArrayType ||
           n.get_kind() == NodeKind::ArrayTypeConst);
    auto c = n.get_children();

    return {
        .size = c[1],
        .inner = c[0],
        .is_const = n.get_kind() == NodeKind::ArrayTypeConst,
    };
}

[[nodiscard]] constexpr auto array(Node const& n) -> Array {
    ASSERT(n.get_kind() == NodeKind::Array);
    auto c = n.get_children();
    return {.size = c[1], .inner = c[0], .items = c.subspan(2)};
}

[[nodiscard]] constexpr auto lit(Node const& n) -> Lit {
    ASSERT(n.get_kind() == NodeKind::Lit);
    return {.items = n.get_children()};
}

[[nodiscard]] constexpr auto lit_param(Node const& n) -> LitParam {
    ASSERT(n.get_kind() == NodeKind::LitParam);
    return {.key = n.get_data_str(), .init = n.get_children()[0]};
}

[[nodiscard]] constexpr auto call(Node const& n) -> Call {
    ASSERT(n.get_kind() == NodeKind::Call);
    auto c = n.get_children();
    return {.callee = c[0], .args = c.subspan(1)};
}

[[nodiscard]] constexpr auto field(Node const& n) -> Field {
    ASSERT(n.get_kind() == NodeKind::Field);
    return {.name = n.get_data_str(), .receiver = n.get_children()[0]};
}

[[nodiscard]] constexpr auto block(Node const& n) -> Block {
    ASSERT(n.get_kind() == NodeKind::Block);
    return {.items = n.get_children()};
}

[[nodiscard]] constexpr auto if_stmt(Node const& n) -> IfStmt {
    ASSERT(n.get_kind() == NodeKind::IfStmt);
    auto c = n.get_children();
    return {.cond = c[0], .wt = c[1], .wf = c[2]};
}

[[nodiscard]] constexpr auto while_stmt(Node const& n) -> WhileStmt {
    ASSERT(n.get_kind() == NodeKind::WhileStmt);
    auto c = n.get_children();
    return {.cond = c[0], .body = c[1]};
}

[[nodiscard]] constexpr auto defer_stmt(Node const& n) -> DeferStmt {
    ASSERT(n.get_kind() == NodeKind::DeferStmt);
    return {.stmt = n.get_children()[0]};
}

[[nodiscard]] constexpr auto assign(Node const& n) -> Assign {
    // FIXME: check if it is a valid assign
    auto c = n.get_children();
    return {
        .lhs = conv::expr_pack(*c[0]),
        .rhs = conv::expr_pack(*c[1]),
        .detail = {.lhs = c[0], .rhs = c[1]},
    };
}

// ==================================================================

enum class PrivateKind { Public, Module, File };
[[nodiscard]] constexpr auto decorators_private_kind(Decorators decorators)
    -> PrivateKind {
    for (auto decl_ptr : decorators.items) {
        auto decl = conv::decorator(*decl_ptr);
        if (decl.name == "private") {
            for (auto param : decl.params) {
                if (param->get_kind() == NodeKind::Id) {
                    auto p = conv::id(*param);
                    if (p.name == "file") return PrivateKind::File;
                }
            }

            return PrivateKind::Module;
        }
    }

    return PrivateKind::Public;
}

}  // namespace yal::ast::conv
