#include "ast.hpp"

#include "fmt/base.h"
#include "fmt/format.h"

namespace yal {

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto Ast::dump(fmt::format_context& ctx, NodeHandle n) const
    -> fmt::format_context::iterator {
    using fmt::format_to;

    auto node = get(n);
    switch (node->kind) {
        case NodeKind::Err: return format_to(ctx.out(), "Err({})", node->span);
        case NodeKind::Nil: return format_to(ctx.out(), "Nil");
        case NodeKind::Break: return format_to(ctx.out(), "Break");

        case NodeKind::Int:
            return format_to(ctx.out(), "Int({})", node->value_uint64());
        case NodeKind::Id:
            return format_to(ctx.out(), "Id({})", node->value_string());
        case NodeKind::Str:
            return format_to(ctx.out(), "Str({:?})", node->value_string());
        case NodeKind::EnumLit:
            return format_to(ctx.out(), "EnumLit(.{})", node->value_string());

        case NodeKind::Ptr:
        case NodeKind::MultiPtr:
        case NodeKind::SlicePtr:
            format_to(ctx.out(), "{}(", node->kind);
            if (node->has_flag_ptr_is_const()) format_to(ctx.out(), "const ");
            dump(ctx, node->first);
            return format_to(ctx.out(), ")");

        case NodeKind::Array: {
            auto a = node->as_array(*this);
            format_to(ctx.out(), "Array(");
            dump(ctx, a.size);
            format_to(ctx.out(), ", ");
            dump(ctx, a.type);
            format_to(ctx.out(), ", [");

            size_t i{};
            for (auto const& chld : a.items) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return format_to(ctx.out(), "])");
        }

        case NodeKind::ArrayType: {
            auto a = node->as_array(*this);
            format_to(ctx.out(), "ArrayType(");
            dump(ctx, a.size);
            format_to(ctx.out(), ", ");
            dump(ctx, a.type);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::ArrayAutoLen: {
            auto a = node->as_array(*this);
            format_to(ctx.out(), "ArrayAutoLen(");
            dump(ctx, a.type);
            format_to(ctx.out(), ", [");

            size_t i{};
            for (auto const& chld : a.items) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return format_to(ctx.out(), "])");
        }

        case NodeKind::Field:
            format_to(ctx.out(), "Field(");
            dump(ctx, node->first);
            return format_to(ctx.out(), ", {})", node->value_string());

        case NodeKind::FuncArg:
            format_to(ctx.out(), "FuncArg({}, ", node->value_string());
            dump(ctx, node->first);
            return format_to(ctx.out(), ")");

        case NodeKind::File:
        case NodeKind::ExprPack:
        case NodeKind::IdPack:
        case NodeKind::FuncRetPack:
        case NodeKind::Block: {
            format_to(ctx.out(), "{}([", node->kind);

            size_t i{};
            for (auto const& chld : get_children(n)) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return format_to(ctx.out(), "])");
        }

        case NodeKind::Func: {
            auto f = node->as_func(*this);
            format_to(ctx.out(), "Func(");

            if (node->has_flag_func_is_extern())
                format_to(ctx.out(), "extern ");

            dump(ctx, f.name);
            format_to(ctx.out(), ", [");

            size_t i{};
            for (auto const& chld : f.args) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            format_to(ctx.out(), "], ");
            dump(ctx, f.ret);
            format_to(ctx.out(), ", ");
            dump(ctx, f.body);

            return format_to(ctx.out(), ")");
        }

        case NodeKind::VarDecl:
        case NodeKind::DefDecl: {
            auto d = node->as_var_decl(*this);
            format_to(ctx.out(), "{}(", node->kind);

            dump(ctx, d.ids);
            format_to(ctx.out(), ", ");
            dump(ctx, d.type);
            format_to(ctx.out(), ", ");
            dump(ctx, d.init);

            return format_to(ctx.out(), ")");
        }

        case NodeKind::Call: {
            auto c = node->as_call(*this);
            format_to(ctx.out(), "Call(");

            dump(ctx, c.callee);
            format_to(ctx.out(), ", [");

            size_t i{};
            for (auto const& chld : c.args) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return format_to(ctx.out(), "])");
        }

        case NodeKind::IfStmt: {
            auto i = node->as_ifstmt(*this);
            format_to(ctx.out(), "IfStmt(");
            dump(ctx, i.cond);
            format_to(ctx.out(), ", ");
            dump(ctx, i.when_true);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmtWithElse: {
            auto i = node->as_ifstmt(*this);
            format_to(ctx.out(), "IfStmt(");
            dump(ctx, i.cond);
            format_to(ctx.out(), ", ");
            dump(ctx, i.when_true);
            format_to(ctx.out(), ", ");
            dump(ctx, i.when_false);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmtWithDecl: {
            auto i = node->as_ifstmt(*this);
            format_to(ctx.out(), "IfStmt(");
            dump(ctx, i.decl);
            format_to(ctx.out(), ", ");
            dump(ctx, i.cond);
            format_to(ctx.out(), ", ");
            dump(ctx, i.when_true);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmtWithDeclAndElse: {
            auto i = node->as_ifstmt(*this);
            format_to(ctx.out(), "IfStmt(");
            dump(ctx, i.decl);
            format_to(ctx.out(), ", ");
            dump(ctx, i.cond);
            format_to(ctx.out(), ", ");
            dump(ctx, i.when_true);
            format_to(ctx.out(), ", ");
            dump(ctx, i.when_false);
            return format_to(ctx.out(), ")");
        }

        // unops
        case NodeKind::AddrOf:
        case NodeKind::LogicNot:
        case NodeKind::BinNot:
        case NodeKind::Plus:
        case NodeKind::Neg:
        case NodeKind::Optional:
        case NodeKind::Deref:
        case NodeKind::OrReturn:
        case NodeKind::ExprStmt:
        case NodeKind::ReturnStmt:
        case NodeKind::Defer:
            format_to(ctx.out(), "{}(", node->kind);
            dump(ctx, node->first);
            return format_to(ctx.out(), ")");

        // binops
        case NodeKind::LogicOr:
        case NodeKind::LogicAnd:
        case NodeKind::BinOr:
        case NodeKind::BinXor:
        case NodeKind::BinAnd:
        case NodeKind::Equal:
        case NodeKind::NotEqual:
        case NodeKind::Greater:
        case NodeKind::GreaterEqual:
        case NodeKind::Smaller:
        case NodeKind::SmallerEqual:
        case NodeKind::ShftLeft:
        case NodeKind::ShftRight:
        case NodeKind::Add:
        case NodeKind::Sub:
        case NodeKind::Mul:
        case NodeKind::Div:
        case NodeKind::Mod:
        case NodeKind::Cast:
        case NodeKind::OrElse:
        case NodeKind::WhileStmt:
        case NodeKind::Assign:
            format_to(ctx.out(), "{}(", node->kind);
            dump(ctx, node->first);
            format_to(ctx.out(), ", ");
            dump(ctx, node->second);
            return format_to(ctx.out(), ")");
    }

    return format_to(ctx.out(), "unknown");
}

}  // namespace yal

auto fmt::formatter<yal::NodeHandle>::format(yal::NodeHandle n,
                                             format_context& ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "{{");

    if (!n.is_valid()) {
        fmt::format_to(ctx.out(), "!");
    }

    if (n.is_array()) {
        fmt::format_to(ctx.out(), "[{:x}]", n.as_idx());
    } else {
        fmt::format_to(ctx.out(), "{:x}", n.as_idx());
    }

    return fmt::format_to(ctx.out(), "}}");
}

auto fmt::formatter<yal::NodeKind>::format(yal::NodeKind   n,
                                           format_context& ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yal::NodeKind::Err: name = "Err"; break;
        case yal::NodeKind::Nil: name = "Nil"; break;
        case yal::NodeKind::File: name = "File"; break;
        case yal::NodeKind::Func: name = "Func"; break;
        case yal::NodeKind::FuncArg: name = "FuncArg"; break;
        case yal::NodeKind::FuncRetPack: name = "FuncRetPack"; break;
        case yal::NodeKind::Block: name = "Block"; break;
        case yal::NodeKind::VarDecl: name = "VarDecl"; break;
        case yal::NodeKind::DefDecl: name = "DefDecl"; break;
        case yal::NodeKind::ExprStmt: name = "ExprStmt"; break;
        case yal::NodeKind::ReturnStmt: name = "ReturnStmt"; break;
        case yal::NodeKind::IfStmt: name = "IfStmt"; break;
        case yal::NodeKind::IfStmtWithElse: name = "IfStmtWithElse"; break;
        case yal::NodeKind::IfStmtWithDecl: name = "IfStmtWithDecl"; break;
        case yal::NodeKind::IfStmtWithDeclAndElse:
            name = "IfStmtWithDeclAndElse";
            break;
        case yal::NodeKind::WhileStmt: name = "WhileStmt"; break;
        case yal::NodeKind::Assign: name = "Assign"; break;
        case yal::NodeKind::Break: name = "Break"; break;
        case yal::NodeKind::Defer: name = "Defer"; break;
        case yal::NodeKind::LogicOr: name = "LogicOr"; break;
        case yal::NodeKind::LogicAnd: name = "LogicAnd"; break;
        case yal::NodeKind::BinOr: name = "BinOr"; break;
        case yal::NodeKind::BinXor: name = "BinXor"; break;
        case yal::NodeKind::BinAnd: name = "BinAnd"; break;
        case yal::NodeKind::Equal: name = "Equal"; break;
        case yal::NodeKind::NotEqual: name = "NotEqual"; break;
        case yal::NodeKind::Greater: name = "Greater"; break;
        case yal::NodeKind::GreaterEqual: name = "GreaterEqual"; break;
        case yal::NodeKind::Smaller: name = "Smaller"; break;
        case yal::NodeKind::SmallerEqual: name = "SmallerEqual"; break;
        case yal::NodeKind::ShftLeft: name = "ShftLeft"; break;
        case yal::NodeKind::ShftRight: name = "ShftRight"; break;
        case yal::NodeKind::Add: name = "Add"; break;
        case yal::NodeKind::Sub: name = "Sub"; break;
        case yal::NodeKind::Mul: name = "Mul"; break;
        case yal::NodeKind::Div: name = "Div"; break;
        case yal::NodeKind::Mod: name = "Mod"; break;
        case yal::NodeKind::Cast: name = "Cast"; break;
        case yal::NodeKind::OrElse: name = "OrElse"; break;
        case yal::NodeKind::OrReturn: name = "OrReturn"; break;
        case yal::NodeKind::AddrOf: name = "AddrOf"; break;
        case yal::NodeKind::LogicNot: name = "LogicNot"; break;
        case yal::NodeKind::BinNot: name = "BinNot"; break;
        case yal::NodeKind::Plus: name = "Plus"; break;
        case yal::NodeKind::Neg: name = "Neg"; break;
        case yal::NodeKind::Optional: name = "Optional"; break;
        case yal::NodeKind::Ptr: name = "Ptr"; break;
        case yal::NodeKind::MultiPtr: name = "MultiPtr"; break;
        case yal::NodeKind::SlicePtr: name = "SlicePtr"; break;
        case yal::NodeKind::Array: name = "Array"; break;
        case yal::NodeKind::ArrayType: name = "ArrayType"; break;
        case yal::NodeKind::ArrayAutoLen: name = "ArrayAutoLen"; break;
        case yal::NodeKind::Deref: name = "Deref"; break;
        case yal::NodeKind::Call: name = "Call"; break;
        case yal::NodeKind::Field: name = "Field"; break;
        case yal::NodeKind::ExprPack: name = "ExprPack"; break;
        case yal::NodeKind::EnumLit: name = "EnumLit"; break;
        case yal::NodeKind::IdPack: name = "IdPack"; break;
        case yal::NodeKind::Int: name = "Int"; break;
        case yal::NodeKind::Id: name = "Id"; break;
        case yal::NodeKind::Str: name = "Str"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::Node>::format(yal::Node n, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}, {}}}", n.kind, n.span,
                          n.first, n.second);
}

auto fmt::formatter<yal::FatNodeHandle>::format(yal::FatNodeHandle n,
                                                format_context&    ctx) const
    -> format_context::iterator {
    return n.ast->dump(ctx, n.node);
}
