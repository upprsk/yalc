#include "compile_qbe.hpp"

#include <cstdint>
#include <ranges>
#include <string_view>

#include "ast-node-conv.hpp"
#include "ast-node.hpp"
#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/ranges.h"
#include "types.hpp"

namespace yal::compile::qbe {
using ast::Ast;
using ast::Node;
using fmt::print;
using fmt::println;
namespace conv = ast::conv;

namespace rv = std::ranges::views;

struct Tmp {
    uint32_t v{};
};
}  // namespace yal::compile::qbe

template <>
struct fmt::formatter<yal::compile::qbe::Tmp> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::compile::qbe::Tmp const& tmp, format_context& ctx) const
        -> format_context::iterator {
        return fmt::format_to(ctx.out(), "%t{}", tmp.v);
    }
};

namespace yal::compile::qbe {

struct Context {
    FILE*          out{};
    ErrorReporter* er{};

    uint32_t                      next_tmp{};
    std::vector<Tmp>              tmp_stack;
    std::vector<std::string_view> pending_string_literals;

    [[nodiscard]] constexpr auto new_tmp() -> Tmp { return {next_tmp++}; }

    constexpr void               push_tmp(Tmp t) { tmp_stack.push_back(t); }
    [[nodiscard]] constexpr auto push_tmp() -> Tmp {
        auto t = new_tmp();
        push_tmp(t);
        return t;
    }

    [[nodiscard]] constexpr auto pop_tmp() -> Tmp {
        auto last = tmp_stack.at(tmp_stack.size() - 1);
        tmp_stack.pop_back();
        return last;
    }
};

auto ptr_to_qbe_integer() -> std::string_view {
    return sizeof(void*) == 4 ? "w" : "l";
}

auto to_qbe_integer(types::Type const& type) -> std::string_view {
    std::string_view ptr_type = ptr_to_qbe_integer();

    switch (type.kind) {
        case types::TypeKind::Uint64:
        case types::TypeKind::Int64: return "l";
        case types::TypeKind::Uint32:
        case types::TypeKind::Int32: return "w";
        case types::TypeKind::Uint16:
        case types::TypeKind::Int16: return "h";
        case types::TypeKind::Uint8:
        case types::TypeKind::Int8: return "b";
        case types::TypeKind::Usize:
        case types::TypeKind::Isize: return ptr_type;
        case types::TypeKind::Bool: return "b";
        case types::TypeKind::Float32: return "f";
        case types::TypeKind::Float64: return "d";
        case types::TypeKind::Ptr:
        case types::TypeKind::PtrConst:
        case types::TypeKind::MultiPtr:
        case types::TypeKind::MultiPtrConst: return ptr_type;
        default:
            PANIC("not a valid integer type for qbe", type.kind,
                  fmt::to_string(type.kind));
    }
}

void compile_stmt(Ast& ast, Node* node, Context& ctx);
void compile_expr(Ast& ast, Node* node, Context& ctx);

void compile_stmt(Ast& ast, Node* node, Context& ctx) {
    auto o = ctx.out;

    if (node->is_oneof(ast::NodeKind::UnscopedGroup)) {
        auto data = conv::unscoped_group(*node);
        for (auto child : data.items) compile_stmt(ast, child, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Discard)) {
        auto data = conv::discard(*node);
        compile_expr(ast, data.child, ctx);
        (void)ctx.pop_tmp();
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprStmt)) {
        auto data = conv::unary(*node);
        compile_expr(ast, data.child, ctx);
        (void)ctx.pop_tmp();
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeclLocalVarDirect)) {
        auto data = conv::decl_local_var_direct(*node);
        compile_expr(ast, data.init, ctx);
        auto tmp = ctx.pop_tmp();

        println(o, "    %{} ={} copy {}", node->get_decl()->link_name,
                to_qbe_integer(*data.init->get_type()), tmp);
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeclLocalVarDirectPack)) {
        auto data = conv::decl_local_var_direct_pack(*node);
        if (data.names.size() != 1) {
            ctx.er->report_bug(node->get_loc(),
                               "declarations using functions that return "
                               "multiple values have not been implemented");
            PANIC("not implemented");
        }

        compile_expr(ast, data.init, ctx);
        auto tmp = ctx.pop_tmp();
        println(o, "    %{} ={} copy {}", data.names[0]->get_decl()->link_name,
                to_qbe_integer(*data.names[0]->get_type()), tmp);

        return;
    }

    if (node->is_oneof(ast::NodeKind::ReturnStmt)) {
        auto data = conv::unary(*node);
        if (data.child) {
            compile_expr(ast, data.child, ctx);
            auto tmp = ctx.pop_tmp();
            println(o, "    ret {}", tmp);
        } else {
            println(o, "    ret");
        }

        return;
    }

    PANIC("invalid node in compile_stmt", node->get_kind(),
          fmt::to_string(node->get_kind()));
}

void compile_block(Ast& ast, Node* node, Context& ctx) {
    auto data = conv::block(*node);
    for (auto child : data.items) compile_stmt(ast, child, ctx);
}

void compile_expr(Ast& ast, Node* node, Context& ctx) {
    auto o = ctx.out;

    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);
        for (auto child : data.items) compile_expr(ast, child, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        auto data = conv::integers(*node);
        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} copy {}", tmp, to_qbe_integer(*node->get_type()),
                data.value);

        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} copy %{}", tmp,
                to_qbe_integer(*node->get_type()), node->get_decl()->link_name);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Str)) {
        // FIXME: this is actually a struct with pointer and len
        auto data = conv::str(*node);
        auto sz = ctx.pending_string_literals.size();
        ctx.pending_string_literals.push_back(data.value);

        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} copy $strlit{}", tmp, ptr_to_qbe_integer(), sz);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Field)) {
        auto data = conv::field(*node);
        auto rty = data.receiver->get_type();

        if (rty->is_strview()) {
            if (data.name == "ptr") {
                compile_expr(ast, data.receiver, ctx);
                return;
            }

            if (data.name == "len") {
                PANIC("len is not implemented");
                return;
            }
        }

        PANIC("invalid Field node", *node);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Add)) {
        auto data = conv::binary(*node);
        compile_expr(ast, data.lhs, ctx);
        compile_expr(ast, data.rhs, ctx);

        auto b = ctx.pop_tmp();
        auto a = ctx.pop_tmp();
        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} add {}, {}", tmp,
                to_qbe_integer(*node->get_type()), a, b);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Call)) {
        auto data = conv::call(*node);
        auto func_type = data.callee->get_type()->as_func();

        if (func_type.get_ret().size() != 1) {
            ctx.er->report_bug(node->get_loc(),
                               "calls to functions that return multiple values "
                               "have not been implemented");
            PANIC("not implemented");
        }

        auto tmp = ctx.push_tmp();
        for (auto arg : data.args) compile_expr(ast, arg, ctx);

        auto r = func_type.get_ret()[0];
        if (!r->is_void()) {
            if (!r->is_integral()) {
                ctx.er->report_bug(
                    node->get_loc(),
                    "calls to functions that return non-integral "
                    "values have not been implemented");
                PANIC("not implemented");
            }

            print(o, "    {} ={} ", tmp, to_qbe_integer(*r));
        } else {
            print(o, "    ");
        }

        // direct call to a function
        if (data.callee->is_oneof(ast::NodeKind::Id)) {
            print(o, "call ${}", data.callee->get_decl()->link_name);
        }

        // method or indirect call
        else {
            ctx.er->report_bug(node->get_loc(),
                               "indirect calls have not been implemented");
            PANIC("not implemented");
        }

        std::vector<Tmp> args;
        for (size_t i = 0; i < data.args.size(); i++) {
            args.push_back(ctx.pop_tmp());
        }

        print(o, "(");

        for (auto [tmp, arg] : rv::zip(rv::reverse(args), data.args)) {
            print(o, "{} {}, ", to_qbe_integer(*arg->get_type()), tmp);
        }

        println(o, ")");

        return;
    }

    PANIC("invalid node in compile_expr", node->get_kind(),
          fmt::to_string(node->get_kind()));
}

void compile_func(Ast& ast, Node* node, Context& ctx) {
    auto o = ctx.out;

    auto data = conv::func_decl(*node);
    auto decl = node->get_decl();
    ASSERT(decl != nullptr);

    // extern declarations are not required in qbe
    if (decl->flags.has_extern()) {
        println(o, "# extern function {}",
                fmt::join(
                    conv::id_pack(*data.name).ids | rv::transform([](Node* n) {
                        return conv::id(*n).name;
                    }),
                    "."));
        return;
    }

    if (decl->flags.has_export()) {
        print(o, "export ");
    }

    auto func_type = decl->get_type()->as_func();
    auto ret_type = func_type.get_ret();
    if (ret_type.size() != 1) {
        ctx.er->report_bug(node->get_loc(),
                           "multiple returns not implemented (found {})",
                           *decl->get_type());
        return;
    }

    print(o, "function ");

    if (ret_type[0]->is_integral()) {
        print(o, "{} ", to_qbe_integer(*ret_type[0]));
    } else if (ret_type[0]->is_void()) {
    } else {
        ctx.er->report_bug(
            node->get_loc(),
            "non-integer or void returns not implemeted (found {})",
            *ret_type[0]);
        return;
    }

    print(o, "${}(", decl->link_name);

    for (auto p : data.get_args().params) {
        print(o, "{} %{},", to_qbe_integer(*p->get_type()),
              p->get_decl()->link_name);
    }

    println(o, ") {{");
    println(o, "@start");

    compile_block(ast, data.body, ctx);

    println(o, "}}");
}

// FIXME: we need an intermediate representation...
void compile_flat_module(Ast& ast, Node* node, Context& ctx) {
    auto o = ctx.out;

    auto mod = conv::flat_module(*node);
    println(o, "# {:?}", mod.name);

    for (auto child : mod.children) {
        if (child->is_oneof(ast::NodeKind::FuncDecl,
                            ast::NodeKind::FuncDeclWithCVarArgs)) {
            compile_func(ast, child, ctx);
        }

        else {
            PANIC("UNEXPECTED top level in module:", child->get_kind());
        }
    }

    for (auto [i, s] : rv::enumerate(ctx.pending_string_literals)) {
        println("data $strlit{} = {{ b {:?}, b 0 }}", i, s);
    }
}

void compile(Ast& ast, Node* root, ErrorReporter& er, types::TypeStore& ts,
             Options const& opt) {
    auto ctx = Context{.out = stdout, .er = &er};
    compile_flat_module(ast, root, ctx);
}

}  // namespace yal::compile::qbe
