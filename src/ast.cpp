#include "ast.hpp"

#include "fmt/base.h"
#include "fmt/format.h"

namespace yal {

void Ast::assert_is_oneof(std::string_view name, Node const& n,
                          auto&&... kinds) const {
    if (!n.is_oneof(kinds...))
        throw std::runtime_error{
            fmt::format("invalid node for `{}`: {}", name, n.kind)};
}

auto Ast::node_with_children(Node const& n) const -> Node::WithChildren {
    assert_is_oneof("node_with_children", n, NodeKind::Block,
                    NodeKind::ExprPack, NodeKind::File, NodeKind::FuncRetPack,
                    NodeKind::IdPack);

    return {.children = get_array(n.first, n.second.as_count())};
}

auto Ast::node_with_child(Node const& n) const -> Node::WithChild {
    assert_is_oneof("node_with_child", n, NodeKind::AddrOf, NodeKind::BinNot,
                    NodeKind::Defer, NodeKind::Deref, NodeKind::ExprStmt,
                    NodeKind::LogicNot, NodeKind::MultiPtr,
                    NodeKind::MultiPtrConst, NodeKind::Neg, NodeKind::Optional,
                    NodeKind::OrReturn, NodeKind::Plus, NodeKind::Ptr,
                    NodeKind::PtrConst, NodeKind::ReturnStmt,
                    NodeKind::SlicePtr, NodeKind::SlicePtrConst);

    return {.child = n.first};
}

auto Ast::node_with_child_pair(Node const& n) const -> Node::WithChildPair {
    assert_is_oneof("node_with_child_pair", n, NodeKind::WhileStmt,
                    NodeKind::Assign, NodeKind::Add, NodeKind::BinAnd,
                    NodeKind::BinOr, NodeKind::BinXor, NodeKind::Div,
                    NodeKind::Equal, NodeKind::Greater, NodeKind::GreaterEqual,
                    NodeKind::LogicAnd, NodeKind::LogicOr, NodeKind::Mod,
                    NodeKind::Mul, NodeKind::NotEqual, NodeKind::OrElse,
                    NodeKind::ShftLeft, NodeKind::ShftRight, NodeKind::Smaller,
                    NodeKind::SmallerEqual, NodeKind::Sub, NodeKind::Cast);

    return {.first = n.first, .second = n.second};
}

auto Ast::node_func(Node const& n) const -> Node::Func {
    assert_is_oneof("node_func", n, NodeKind::Func, NodeKind::FuncExtern);

    auto children = get_array(n.first, n.second.as_count());
    if (children.size() < 3)
        throw std::runtime_error{
            fmt::format("invalid number of children for `node_func`: {} < 3",
                        children.size())};

    return {
        .name = children[0],
        .ret = children[1],
        .body = children[2],
        .args = children.subspan(3),
        .is_extern = n.kind == NodeKind::FuncExtern,
    };
}

auto Ast::node_named(Node const& n) const -> Node::Named {
    assert_is_oneof("node_named", n, NodeKind::FuncArg, NodeKind::Field);

    return {.name = n.value_string(), .child = n.first};
}

auto Ast::node_decl(Node const& n) const -> Node::Decl {
    assert_is_oneof("node_decl", n, NodeKind::DefDecl, NodeKind::VarDecl);

    auto children = get_array(n.second, 2);

    return {
        .ids = n.first,
        .type = children[0],
        .init = children[1],
    };
}

auto Ast::node_if_stmt(Node const& n) const -> Node::IfStmt {
    if (n.kind == NodeKind::IfStmt) {
        return {
            .decl = {},
            .cond = n.first,
            .when_true = n.second,
            .when_false = {},
        };
    }

    if (n.kind == NodeKind::IfStmtWithElse) {
        auto children = get_array(n.second, 2);

        return {
            .decl = {},
            .cond = n.first,
            .when_true = children[0],
            .when_false = children[1],
        };
    }

    if (n.kind == NodeKind::IfStmtWithDecl) {
        auto children = get_array(n.second, 2);

        return {
            .decl = children[0],
            .cond = n.first,
            .when_true = children[1],
            .when_false = {},
        };
    }

    if (n.kind == NodeKind::IfStmtWithDeclAndElse) {
        auto children = get_array(n.second, 3);

        return {
            .decl = children[0],
            .cond = n.first,
            .when_true = children[1],
            .when_false = children[2],
        };
    }

    throw std::runtime_error{
        fmt::format("invalid node for `node_if_stmt`: {}", n.kind)};
}

auto Ast::node_array(Node const& n) const -> Node::Array {
    if (n.kind == NodeKind::Array) {
        auto children = get_array(n.first, n.second.as_count());

        if (children.size() < 2)
            throw std::runtime_error{fmt::format(
                "invalid number of children for `node_array`: {} < 2",
                children.size())};

        return {
            .type = children[1],
            .size = children[0],
            .items = children.subspan(2),
        };
    }

    if (n.kind == NodeKind::ArrayAutoLen) {
        auto children = get_array(n.first, n.second.as_count());

        if (children.size() < 1)
            throw std::runtime_error{fmt::format(
                "invalid number of children for `node_array`: {} < 1",
                children.size())};

        return {
            .type = children[0],
            .size = {},
            .items = children.subspan(1),
        };
    }

    if (n.kind == NodeKind::ArrayType) {
        return {
            .type = n.second,
            .size = n.first,
            .items = {},
        };
    }

    throw std::runtime_error{
        fmt::format("invalid node for `node_array`: {}", n.kind)};
}

auto Ast::node_call(Node const& n) const -> Node::Call {
    assert_is_oneof("node_call", n, NodeKind::Call);

    auto children = get_array(n.first, n.second.as_count());
    if (children.size() < 1)
        throw std::runtime_error{
            fmt::format("invalid number of children for `node_call`: {} < 1",
                        children.size())};

    return {
        .callee = children[0],
        .args = children.subspan(1),
    };
}

// ----------------------------------------------------------------------------

auto Ast::new_node_if(NodeKind kind, Span span, Node::IfStmt params)
    -> NodeHandle {
    if (kind == NodeKind::IfStmt)
        return new_node(kind, span, params.cond, params.when_true);
    if (kind == NodeKind::IfStmtWithElse)
        return new_node(kind, span, params.cond,
                        new_array_of(params.when_true, params.when_false));
    if (kind == NodeKind::IfStmtWithDecl)
        return new_node(kind, span, params.cond,
                        new_array_of(params.decl, params.when_true));
    if (kind == NodeKind::IfStmtWithDeclAndElse)
        return new_node(
            kind, span, params.cond,
            new_array_of(params.decl, params.when_true, params.when_false));

    throw std::runtime_error{
        fmt::format("invalid kind for `new_node_if`: {}", kind)};
}

auto Ast::new_node_array(NodeKind kind, Span span, Node::Array params)
    -> NodeHandle {
    if (kind == NodeKind::Array)
        return new_node(
            kind, span,
            new_array_prepend(params.items, params.size, params.type),
            NodeHandle::init_count(params.items.size() + 2));
    if (kind == NodeKind::ArrayAutoLen)
        return new_node(kind, span,
                        new_array_prepend(params.items, params.type),
                        NodeHandle::init_count(params.items.size() + 1));
    if (kind == NodeKind::ArrayType)
        return new_node(kind, span, params.size, params.type);

    throw std::runtime_error{
        fmt::format("invalid kind for `new_node_array`: {}", kind)};
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto Ast::dump(fmt::format_context& ctx, NodeHandle n,
               TypeStore const* ts) const -> fmt::format_context::iterator {
    auto dump_list = [this](fmt::format_context&        ctx,
                            std::span<NodeHandle const> lst,
                            TypeStore const*            ts) {
        size_t i{};
        for (auto const& chld : lst) {
            if (i++ != 0) format_to(ctx.out(), ", ");
            dump(ctx, chld, ts);
        }
    };

    auto dump_type = [](fmt::format_context& ctx, TypeStore const* ts,
                        Node const& n) {
        if (ts) return format_to(ctx.out(), "{}, ", ts->fatten(n.type));

        return ctx.out();
    };

    if (!n.is_valid()) return format_to(ctx.out(), "{}", n);

    auto node = get(n);
    switch (node->kind) {
        case NodeKind::Err:
            format_to(ctx.out(), "Err(");
            dump_type(ctx, ts, *node);
            return format_to(ctx.out(), "{})", node->span);

        case NodeKind::Break:
        case NodeKind::Nil: {
            auto it = format_to(ctx.out(), "{}", node->kind);
            if (ts) it = format_to(ctx.out(), "({})", ts->fatten(node->type));
            return it;
        }

        case NodeKind::Block:
        case NodeKind::ExprPack:
        case NodeKind::File:
        case NodeKind::FuncRetPack:
        case NodeKind::IdPack: {
            auto p = node_with_children(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            format_to(ctx.out(), "[", node->kind);
            dump_list(ctx, p.children, ts);
            return format_to(ctx.out(), "])");
        }

        case NodeKind::Func: {
            auto p = node_func(*node);
            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);

            dump(ctx, p.name, ts);

            format_to(ctx.out(), ", [");
            dump_list(ctx, p.args, ts);
            format_to(ctx.out(), "], ");

            dump(ctx, p.ret, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.body, ts);

            return format_to(ctx.out(), ")");
        }

        case NodeKind::FuncExtern: {
            auto p = node_func(*node);
            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);

            dump(ctx, p.name, ts);

            format_to(ctx.out(), ", [");
            dump_list(ctx, p.args, ts);
            format_to(ctx.out(), "], ");

            dump(ctx, p.ret, ts);

            return format_to(ctx.out(), ")");
        }

        case NodeKind::FuncArg: {
            auto p = node_named(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            format_to(ctx.out(), "{}, ", p.name);
            dump(ctx, p.child, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::VarDecl:
        case NodeKind::DefDecl: {
            auto p = node_decl(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);

            dump(ctx, p.ids, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.type, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.init, ts);

            return format_to(ctx.out(), ")");
        }

        case NodeKind::AddrOf:
        case NodeKind::BinNot:
        case NodeKind::Defer:
        case NodeKind::Deref:
        case NodeKind::ExprStmt:
        case NodeKind::LogicNot:
        case NodeKind::MultiPtr:
        case NodeKind::MultiPtrConst:
        case NodeKind::Neg:
        case NodeKind::Optional:
        case NodeKind::OrReturn:
        case NodeKind::Plus:
        case NodeKind::Ptr:
        case NodeKind::PtrConst:
        case NodeKind::ReturnStmt:
        case NodeKind::SlicePtr:
        case NodeKind::SlicePtrConst: {
            auto p = node_with_child(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            dump(ctx, p.child, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmt: {
            auto p = node_if_stmt(*node);

            format_to(ctx.out(), "IfStmt(");
            dump_type(ctx, ts, *node);
            dump(ctx, p.cond, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.when_true, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmtWithElse: {
            auto p = node_if_stmt(*node);

            format_to(ctx.out(), "IfStmtWithElse(");
            dump_type(ctx, ts, *node);
            dump(ctx, p.cond, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.when_true, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.when_false, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmtWithDecl: {
            auto p = node_if_stmt(*node);

            format_to(ctx.out(), "IfStmtWithDecl(");
            dump_type(ctx, ts, *node);
            dump(ctx, p.decl, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.cond, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.when_true, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::IfStmtWithDeclAndElse: {
            auto p = node_if_stmt(*node);

            format_to(ctx.out(), "IfStmtWithDeclAndElse(");
            dump_type(ctx, ts, *node);
            dump(ctx, p.decl, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.cond, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.when_true, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.when_false, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::WhileStmt:
        case NodeKind::Assign:
        case NodeKind::Add:
        case NodeKind::BinAnd:
        case NodeKind::BinOr:
        case NodeKind::BinXor:
        case NodeKind::Div:
        case NodeKind::Equal:
        case NodeKind::Greater:
        case NodeKind::GreaterEqual:
        case NodeKind::LogicAnd:
        case NodeKind::LogicOr:
        case NodeKind::Mod:
        case NodeKind::Mul:
        case NodeKind::NotEqual:
        case NodeKind::OrElse:
        case NodeKind::ShftLeft:
        case NodeKind::ShftRight:
        case NodeKind::Smaller:
        case NodeKind::SmallerEqual:
        case NodeKind::Sub:
        case NodeKind::Cast: {
            auto p = node_with_child_pair(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            dump(ctx, p.first, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.second, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::Array: {
            auto p = node_array(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            dump(ctx, p.size, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.type, ts);
            format_to(ctx.out(), ", [");
            dump_list(ctx, p.items, ts);
            return format_to(ctx.out(), "])");
        }

        case NodeKind::ArrayAutoLen: {
            auto p = node_array(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            dump(ctx, p.type, ts);
            format_to(ctx.out(), ", [");
            dump_list(ctx, p.items, ts);
            return format_to(ctx.out(), "])");
        }

        case NodeKind::ArrayType: {
            auto p = node_array(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            dump(ctx, p.size, ts);
            format_to(ctx.out(), ", ");
            dump(ctx, p.type, ts);
            return format_to(ctx.out(), ")");
        }

        case NodeKind::Call: {
            auto c = node_call(*node);
            format_to(ctx.out(), "Call(");
            dump_type(ctx, ts, *node);

            dump(ctx, c.callee, ts);
            format_to(ctx.out(), ", [");
            dump_list(ctx, c.args, ts);
            return format_to(ctx.out(), "])");
        }

        case NodeKind::Field: {
            auto p = node_named(*node);

            format_to(ctx.out(), "{}(", node->kind);
            dump_type(ctx, ts, *node);
            dump(ctx, p.child, ts);
            return format_to(ctx.out(), ", {})", p.name);
        }

        case NodeKind::EnumLit:
            format_to(ctx.out(), "EnumLit(");
            dump_type(ctx, ts, *node);
            return format_to(ctx.out(), ".{})", node->value_string());
        case NodeKind::Int:
            format_to(ctx.out(), "Int(");
            dump_type(ctx, ts, *node);
            return format_to(ctx.out(), "{})", node->value_uint64());
        case NodeKind::Id:
            format_to(ctx.out(), "Id(");
            dump_type(ctx, ts, *node);
            return format_to(ctx.out(), "{})", node->value_string());
        case NodeKind::Str:
            format_to(ctx.out(), "Str(");
            dump_type(ctx, ts, *node);
            return format_to(ctx.out(), "{:?})", node->value_string());
    }

    return format_to(ctx.out(), "unknown");
}

void Ast::dump_dot(FILE* f, NodeHandle n, TypeStore const* ts) const {
    using fmt::println;

    println(f, "digraph g {{");
    println(f, R"~~(fontname="Helvetica,Arial,sans-serif")~~");
    println(f, R"~~(node [fontname="Helvetica,Arial,sans-serif"])~~");
    println(f, R"~~(edge [fontname="Helvetica,Arial,sans-serif"])~~");
    println(f, R"~~(graph [ rankdir = "LR" ];)~~");
    println(f, R"~~(node [ fontsize = "16" shape = "ellipse" ];)~~");
    println(f, R"~~(edge [ ];)~~");

    dump_dot_node(f, n, ts);

    println(f, "}}");
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void Ast::dump_dot_node(FILE* f, NodeHandle n, TypeStore const* ts) const {
    using fmt::print;
    using fmt::println;

    println(f, R"~~("node{}" [)~~", n.as_raw_idx());
    println(f, R"~~(shape = "record")~~");

    if (n.is_valid()) {
        auto node = get(n);
        print(f, R"~~(label = "<f0> {} | {})~~", n.as_raw_idx(), node->kind);
        if (ts) {
            print(f, R"~~( | {})~~", ts->fatten(node->type));
        }

        if (node->first.is_valid())
            print(f, R"~~( | <f1> {:#})~~", node->first);
        if (node->second.is_valid())
            print(f, R"~~( | <f2> {:#})~~", node->second);

        if (node->is_id() || node->is_func_arg() || node->is_field())
            print(f, R"~~( | `{}`)~~", node->value_string());
        if (node->is_enum_lit())
            print(f, R"~~( | `.{}`)~~", node->value_string());
        if (node->is_int()) print(f, R"~~( | {})~~", node->value_uint64());

        if (node->is_str()) {
            // double escape the string before printing it.
            // this is very suboptimal, but it was easy.
            auto s = fmt::format("{:?}", node->value_string());
            s = fmt::format("{:?}", s.substr(1, s.length() - 2));
            print(f, R"~~( | \"{}\")~~", s.substr(1, s.length() - 2));
        }

        println(f, "\"");
    }

    println(f, R"~~(];)~~", n.as_raw_idx());

    auto conn = [f](NodeHandle from, NodeHandle to, std::string_view t = "f1") {
        println(f, R"~~("node{}":{} -> "node{}":f0 [])~~", from.as_raw_idx(), t,
                to.as_raw_idx());
    };

    auto dump_and_conn = [this, f, ts, &conn](NodeHandle from, NodeHandle to,
                                              std::string_view t = "f1") {
        dump_dot_node(f, to, ts);
        conn(from, to, t);
    };

    if (n.is_valid()) {
        auto node = get(n);

        switch (node->kind) {
            case NodeKind::Err:
            case NodeKind::Break:
            case NodeKind::Nil:
            case NodeKind::EnumLit:
            case NodeKind::Int:
            case NodeKind::Id:
            case NodeKind::Str: break;

            case NodeKind::Block:
            case NodeKind::ExprPack:
            case NodeKind::File:
            case NodeKind::FuncRetPack:
            case NodeKind::IdPack: {
                auto p = node_with_children(*node);
                for (auto const& c : p.children) dump_and_conn(n, c);
            } break;

            case NodeKind::Func: {
                auto p = node_func(*node);

                dump_and_conn(n, p.name);
                for (auto const& c : p.args) dump_and_conn(n, c);
                dump_and_conn(n, p.ret);
                dump_and_conn(n, p.body);
            } break;

            case NodeKind::FuncExtern: {
                auto p = node_func(*node);

                dump_and_conn(n, p.name);
                for (auto const& c : p.args) dump_and_conn(n, c);
                dump_and_conn(n, p.ret);
            } break;

            case NodeKind::FuncArg:
            case NodeKind::Field: {
                auto p = node_named(*node);
                dump_and_conn(n, p.child);
            } break;

            case NodeKind::DefDecl:
            case NodeKind::VarDecl: {
                auto p = node_decl(*node);

                dump_and_conn(n, p.ids);
                dump_and_conn(n, p.type);
                dump_and_conn(n, p.init);
            } break;

            case NodeKind::AddrOf:
            case NodeKind::BinNot:
            case NodeKind::Defer:
            case NodeKind::Deref:
            case NodeKind::ExprStmt:
            case NodeKind::LogicNot:
            case NodeKind::MultiPtr:
            case NodeKind::MultiPtrConst:
            case NodeKind::Neg:
            case NodeKind::Optional:
            case NodeKind::OrReturn:
            case NodeKind::Plus:
            case NodeKind::Ptr:
            case NodeKind::PtrConst:
            case NodeKind::ReturnStmt:
            case NodeKind::SlicePtr:
            case NodeKind::SlicePtrConst: {
                auto p = node_with_child(*node);
                dump_and_conn(n, p.child);
            } break;

            case NodeKind::IfStmt: {
                auto p = node_if_stmt(*node);

                dump_and_conn(n, p.cond);
                dump_and_conn(n, p.when_true);
            } break;

            case NodeKind::IfStmtWithElse: {
                auto p = node_if_stmt(*node);

                dump_and_conn(n, p.cond);
                dump_and_conn(n, p.when_true);
                dump_and_conn(n, p.when_false);
            } break;

            case NodeKind::IfStmtWithDecl: {
                auto p = node_if_stmt(*node);

                dump_and_conn(n, p.decl);
                dump_and_conn(n, p.cond);
                dump_and_conn(n, p.when_true);
            } break;

            case NodeKind::IfStmtWithDeclAndElse: {
                auto p = node_if_stmt(*node);

                dump_and_conn(n, p.decl);
                dump_and_conn(n, p.cond);
                dump_and_conn(n, p.when_true);
                dump_and_conn(n, p.when_false);
            } break;

            case NodeKind::WhileStmt:
            case NodeKind::Assign:
            case NodeKind::Add:
            case NodeKind::BinAnd:
            case NodeKind::BinOr:
            case NodeKind::BinXor:
            case NodeKind::Div:
            case NodeKind::Equal:
            case NodeKind::Greater:
            case NodeKind::GreaterEqual:
            case NodeKind::LogicAnd:
            case NodeKind::LogicOr:
            case NodeKind::Mod:
            case NodeKind::Mul:
            case NodeKind::NotEqual:
            case NodeKind::OrElse:
            case NodeKind::ShftLeft:
            case NodeKind::ShftRight:
            case NodeKind::Smaller:
            case NodeKind::SmallerEqual:
            case NodeKind::Sub:
            case NodeKind::Cast: {
                auto p = node_with_child_pair(*node);

                dump_and_conn(n, p.first);
                dump_and_conn(n, p.second, "f2");
            } break;

            case NodeKind::Array: {
                auto p = node_array(*node);

                dump_and_conn(n, p.size);
                dump_and_conn(n, p.type);
                for (auto const& c : p.items) dump_and_conn(n, c);
            } break;

            case NodeKind::ArrayAutoLen: {
                auto p = node_array(*node);

                dump_and_conn(n, p.type);
                for (auto const& c : p.items) dump_and_conn(n, c);
            } break;

            case NodeKind::ArrayType: {
                auto p = node_array(*node);

                dump_and_conn(n, p.size);
                dump_and_conn(n, p.type, "f2");
            } break;

            case NodeKind::Call: {
                auto p = node_call(*node);

                dump_and_conn(n, p.callee);
                for (auto const& c : p.args) dump_and_conn(n, c);
            } break;
        }
    }
}

}  // namespace yal

auto fmt::formatter<yal::NodeHandle>::format(yal::NodeHandle n,
                                             format_context& ctx) const
    -> format_context::iterator {
    if (!is_escaped) fmt::format_to(ctx.out(), "{{");

    if (!n.is_valid()) {
        fmt::format_to(ctx.out(), "!");
    }

    auto it = ctx.out();
    if (n.is_array()) {
        it = fmt::format_to(ctx.out(), "[{:x}]", n.as_raw_idx());
    } else {
        it = fmt::format_to(ctx.out(), "{:x}", n.as_raw_idx());
    }

    if (!is_escaped) it = fmt::format_to(ctx.out(), "{{");

    return it;
}

auto fmt::formatter<yal::NodeKind>::format(yal::NodeKind   n,
                                           format_context& ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yal::NodeKind::Err: name = "Err"; break;
        case yal::NodeKind::Break: name = "Break"; break;
        case yal::NodeKind::Nil: name = "Nil"; break;
        case yal::NodeKind::Block: name = "Block"; break;
        case yal::NodeKind::ExprPack: name = "ExprPack"; break;
        case yal::NodeKind::File: name = "File"; break;
        case yal::NodeKind::FuncRetPack: name = "FuncRetPack"; break;
        case yal::NodeKind::IdPack: name = "IdPack"; break;
        case yal::NodeKind::Func: name = "Func"; break;
        case yal::NodeKind::FuncExtern: name = "FuncExtern"; break;
        case yal::NodeKind::FuncArg: name = "FuncArg"; break;
        case yal::NodeKind::DefDecl: name = "DefDecl"; break;
        case yal::NodeKind::VarDecl: name = "VarDecl"; break;
        case yal::NodeKind::AddrOf: name = "AddrOf"; break;
        case yal::NodeKind::BinNot: name = "BinNot"; break;
        case yal::NodeKind::Defer: name = "Defer"; break;
        case yal::NodeKind::Deref: name = "Deref"; break;
        case yal::NodeKind::ExprStmt: name = "ExprStmt"; break;
        case yal::NodeKind::LogicNot: name = "LogicNot"; break;
        case yal::NodeKind::MultiPtr: name = "MultiPtr"; break;
        case yal::NodeKind::MultiPtrConst: name = "MultiPtrConst"; break;
        case yal::NodeKind::Neg: name = "Neg"; break;
        case yal::NodeKind::Optional: name = "Optional"; break;
        case yal::NodeKind::OrReturn: name = "OrReturn"; break;
        case yal::NodeKind::Plus: name = "Plus"; break;
        case yal::NodeKind::Ptr: name = "Ptr"; break;
        case yal::NodeKind::PtrConst: name = "PtrConst"; break;
        case yal::NodeKind::ReturnStmt: name = "ReturnStmt"; break;
        case yal::NodeKind::SlicePtr: name = "SlicePtr"; break;
        case yal::NodeKind::SlicePtrConst: name = "SlicePtrConst"; break;
        case yal::NodeKind::IfStmt: name = "IfStmt"; break;
        case yal::NodeKind::IfStmtWithElse: name = "IfStmtWithElse"; break;
        case yal::NodeKind::IfStmtWithDecl: name = "IfStmtWithDecl"; break;
        case yal::NodeKind::IfStmtWithDeclAndElse:
            name = "IfStmtWithDeclAndElse";
            break;
        case yal::NodeKind::WhileStmt: name = "WhileStmt"; break;
        case yal::NodeKind::Assign: name = "Assign"; break;
        case yal::NodeKind::Add: name = "Add"; break;
        case yal::NodeKind::BinAnd: name = "BinAnd"; break;
        case yal::NodeKind::BinOr: name = "BinOr"; break;
        case yal::NodeKind::BinXor: name = "BinXor"; break;
        case yal::NodeKind::Div: name = "Div"; break;
        case yal::NodeKind::Equal: name = "Equal"; break;
        case yal::NodeKind::Greater: name = "Greater"; break;
        case yal::NodeKind::GreaterEqual: name = "GreaterEqual"; break;
        case yal::NodeKind::LogicAnd: name = "LogicAnd"; break;
        case yal::NodeKind::LogicOr: name = "LogicOr"; break;
        case yal::NodeKind::Mod: name = "Mod"; break;
        case yal::NodeKind::Mul: name = "Mul"; break;
        case yal::NodeKind::NotEqual: name = "NotEqual"; break;
        case yal::NodeKind::OrElse: name = "OrElse"; break;
        case yal::NodeKind::ShftLeft: name = "ShftLeft"; break;
        case yal::NodeKind::ShftRight: name = "ShftRight"; break;
        case yal::NodeKind::Smaller: name = "Smaller"; break;
        case yal::NodeKind::SmallerEqual: name = "SmallerEqual"; break;
        case yal::NodeKind::Sub: name = "Sub"; break;
        case yal::NodeKind::Cast: name = "Cast"; break;
        case yal::NodeKind::Array: name = "Array"; break;
        case yal::NodeKind::ArrayAutoLen: name = "ArrayAutoLen"; break;
        case yal::NodeKind::ArrayType: name = "ArrayType"; break;
        case yal::NodeKind::Call: name = "Call"; break;
        case yal::NodeKind::Field: name = "Field"; break;
        case yal::NodeKind::EnumLit: name = "EnumLit"; break;
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
    return n.ast->dump(ctx, n.node, n.ts);
}
