#include "ast.hpp"

#include "fmt/format.h"

namespace yal {

auto Ast::dump(fmt::format_context& ctx, NodeHandle n) const
    -> fmt::format_context::iterator {
    auto node = get(n);
    switch (node->kind) {
        case NodeKind::Err:
            return fmt::format_to(ctx.out(), "Err({})", node->span);
        case NodeKind::Nil: return fmt::format_to(ctx.out(), "Nil");

        case NodeKind::Int:
            return fmt::format_to(ctx.out(), "Int({})", node->value_uint64());
        case NodeKind::Id:
            return fmt::format_to(ctx.out(), "Id({})", node->value_string());
        case NodeKind::EnumLit:
            return fmt::format_to(ctx.out(), "EnumLit(.{})",
                                  node->value_string());

        case NodeKind::File:
        case NodeKind::ExprPack: {
            fmt::format_to(ctx.out(), "{}([", node->kind);

            size_t i{};
            for (auto const& chld : get_children(n)) {
                if (i++ != 0) fmt::format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return fmt::format_to(ctx.out(), "])");
        }

        case NodeKind::Call: {
            fmt::format_to(ctx.out(), "{}(", node->kind);

            auto children = get_children(n);
            if (children.size() < 1)
                throw fmt::system_error(
                    6, "invalid number of children for call: {} < 1",
                    children.size());

            dump(ctx, children[0]);
            fmt::format_to(ctx.out(), ", [");

            size_t i{};
            for (auto const& chld : children.subspan(1)) {
                if (i++ != 0) fmt::format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return fmt::format_to(ctx.out(), "])");
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
            fmt::format_to(ctx.out(), "{}(", node->kind);
            dump(ctx, node->first);
            return fmt::format_to(ctx.out(), ")");

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
            fmt::format_to(ctx.out(), "{}(", node->kind);
            dump(ctx, node->first);
            fmt::format_to(ctx.out(), ", ");
            dump(ctx, node->second);
            return fmt::format_to(ctx.out(), ")");
    }

    return fmt::format_to(ctx.out(), "unknown");
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
        case yal::NodeKind::Deref: name = "Deref"; break;
        case yal::NodeKind::Call: name = "Call"; break;
        case yal::NodeKind::ExprPack: name = "ExprPack"; break;
        case yal::NodeKind::EnumLit: name = "EnumLit"; break;
        case yal::NodeKind::Int: name = "Int"; break;
        case yal::NodeKind::Id: name = "Id";
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::Node>::format(yal::Node n, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", n.kind, n.span, n.first,
                          n.second);
}

auto fmt::formatter<yal::FatNodeHandle>::format(yal::FatNodeHandle n,
                                                format_context&    ctx) const
    -> format_context::iterator {
    return n.ast->dump(ctx, n.node);
}
