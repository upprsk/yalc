#pragma once

#include <cstdint>
#include <span>
#include <string_view>

#include "ast-node-conv.hpp"
#include "ast-node.hpp"
#include "ast.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"

namespace yal::ast {

// NOLINTNEXTLINE()
auto visit_children(Ast& ast, Node* node, auto&& visitor, auto&&... args) {
    if (node == nullptr) return;

    switch (node->get_kind()) {
        case NodeKind::Err:
        case NodeKind::Id:
        case NodeKind::KwLit:
        case NodeKind::Int:
        case NodeKind::Double:
        case NodeKind::Float:
        case NodeKind::Str:
        case NodeKind::Char: break;

        case NodeKind::Module: {
            auto data = conv::module(*node);
            for (auto c : data.children) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::SourceFile: {
            auto data = conv::source_file(*node);
            visitor(ast, data.mod, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            for (auto c : data.children) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::ModuleDecl: break;

        case NodeKind::FuncDecl:
        case NodeKind::FuncDeclWithCVarArgs: {
            auto data = conv::func_decl(*node);
            visitor(ast, data.decorators,
                    std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.name, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.gargs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.args, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.ret, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.body, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::FuncParams: {
            auto data = conv::func_params(*node);
            for (auto c : data.params) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::TopVarDecl: {
            auto data = conv::top_var_decl(*node);
            visitor(ast, data.decorators,
                    std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.decl, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::TopDefDecl: {
            auto data = conv::top_def_decl(*node);
            visitor(ast, data.decorators,
                    std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.decl, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::IdPack: {
            auto data = conv::id_pack(*node);
            for (auto c : data.ids) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::FuncParam: {
            auto data = conv::func_param(*node);
            visitor(ast, data.type, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::FuncRetPack: {
            auto data = conv::func_ret_pack(*node);
            for (auto c : data.ret) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::NamedRet: {
            auto data = conv::named_ret(*node);
            visitor(ast, data.type, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Decorator: {
            auto data = conv::decorator(*node);
            for (auto c : data.params) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::DecoratorParam: {
            auto data = conv::decorator_param(*node);
            visitor(ast, data.value, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Decorators: {
            auto data = conv::decorators(*node);
            for (auto c : data.items) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::ImportStmt: break;

        case NodeKind::ExprPack: {
            auto data = conv::expr_pack(*node);
            for (auto c : data.items) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::Add:
        case NodeKind::Sub:
        case NodeKind::Mul:
        case NodeKind::Div:
        case NodeKind::Mod:
        case NodeKind::LeftShift:
        case NodeKind::RightShift:
        case NodeKind::Equal:
        case NodeKind::NotEqual:
        case NodeKind::Less:
        case NodeKind::LessEqual:
        case NodeKind::Greater:
        case NodeKind::GreaterEqual:
        case NodeKind::Band:
        case NodeKind::Bor:
        case NodeKind::Bxor:
        case NodeKind::Land:
        case NodeKind::Lor:
        case NodeKind::Cast: {
            auto data = conv::binary(*node);
            visitor(ast, data.lhs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.rhs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::AddrOf:
        case NodeKind::Lnot:
        case NodeKind::Bnot:
        case NodeKind::Neg: {
            auto data = conv::unary(*node);
            visitor(ast, data.child, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::StructType: {
            auto data = conv::struct_type(*node);
            for (auto c : data.fields) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::StructField: {
            auto data = conv::struct_field(*node);
            visitor(ast, data.type, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.init, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::PtrConst:
        case NodeKind::Ptr: {
            auto data = conv::ptr(*node);
            visitor(ast, data.inner, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::MultiPtrConst:
        case NodeKind::MultiPtr: {
            auto data = conv::mptr(*node);
            visitor(ast, data.inner, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::SliceConst:
        case NodeKind::Slice: {
            auto data = conv::slice(*node);
            visitor(ast, data.inner, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::ArrayTypeConst:
        case NodeKind::ArrayType: {
            auto data = conv::array_type(*node);
            visitor(ast, data.size, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.inner, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Array: {
            auto data = conv::array(*node);
            visitor(ast, data.size, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.inner, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            for (auto c : data.items) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::Optional: {
            auto data = conv::unary(*node);
            visitor(ast, data.child, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Lit: {
            auto data = conv::lit(*node);
            for (auto c : data.items) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::LitParam: {
            auto data = conv::lit_param(*node);
            visitor(ast, data.init, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Call: {
            auto data = conv::call(*node);
            visitor(ast, data.callee, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            for (auto c : data.args) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::Field: {
            auto data = conv::field(*node);
            visitor(ast, data.receiver,
                    std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Block: {
            auto data = conv::block(*node);
            for (auto c : data.items) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::ExprStmt:
        case NodeKind::ReturnStmt: {
            auto data = conv::unary(*node);
            visitor(ast, data.child, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::IfStmt: {
            auto data = conv::if_stmt(*node);
            visitor(ast, data.cond, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.wt, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.wf, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::WhileStmt: {
            auto data = conv::while_stmt(*node);
            visitor(ast, data.cond, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.body, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Break:
        case NodeKind::Continue: break;

        case NodeKind::DeferStmt: {
            auto data = conv::defer_stmt(*node);
            visitor(ast, data.stmt, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::VarDecl: {
            auto data = conv::var_decl(*node);
            visitor(ast, data.ids, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.types, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.inits, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::DefDecl: {
            auto data = conv::def_decl(*node);
            visitor(ast, data.ids, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.types, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.inits, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Assign:
        case NodeKind::AssignAdd:
        case NodeKind::AssignSub:
        case NodeKind::AssignMul:
        case NodeKind::AssignDiv:
        case NodeKind::AssignMod:
        case NodeKind::AssignShiftLeft:
        case NodeKind::AssignShiftRight:
        case NodeKind::AssignBand:
        case NodeKind::AssignBxor:
        case NodeKind::AssignBor: {
            auto data = conv::assign(*node);
            visitor(ast, data.lhs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.rhs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::FlatModule: {
            auto data = conv::flat_module(*node);
            for (auto c : data.children) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::Coerce: {
            auto data = conv::coerce(*node);
            visitor(ast, data.child, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Discard: {
            auto data = conv::discard(*node);
            visitor(ast, data.child, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;

        case NodeKind::Discarded: break;

        case NodeKind::UnscopedGroup: {
            auto data = conv::unscoped_group(*node);
            for (auto c : data.items) {
                visitor(ast, c, std::forward<decltype(visitor)>(visitor),
                        std::forward<decltype(args)>(args)...);
            }
        } break;

        case NodeKind::AssignDirect:
        case NodeKind::AssignDirectPack: {
            auto data = conv::assign(*node);
            visitor(ast, data.lhs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
            visitor(ast, data.rhs, std::forward<decltype(visitor)>(visitor),
                    std::forward<decltype(args)>(args)...);
        } break;
    }
}

}  // namespace yal::ast
