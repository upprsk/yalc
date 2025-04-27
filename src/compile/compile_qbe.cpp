#include "compile_qbe.hpp"

#include <cstdint>
#include <ranges>
#include <string_view>

#include "ast-node-conv.hpp"
#include "ast-node.hpp"
#include "file-store.hpp"
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

    uint32_t         next_tmp{};
    std::vector<Tmp> tmp_stack;

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

auto to_qbe_integer(types::Type const& type) -> std::string_view {
    std::string_view ptr_type = sizeof(void*) == 4 ? "w" : "l";

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
        default: PANIC("not a valid integer type for qbe: {}", type.kind);
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

    if (node->is_oneof(ast::NodeKind::ReturnStmt)) {
        auto data = conv::unary(*node);
        compile_expr(ast, data.child, ctx);
        auto tmp = ctx.pop_tmp();
        println(o, "    ret {}", tmp);
        return;
    }

    PANIC("invalid node in compile_stmt", node->get_kind());
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
            PANIC("not implemented");
        }

        // direct call to a function
        if (data.callee->is_oneof(ast::NodeKind::Id)) {
            print(o, "call ${}", conv::id(*data.callee).name);
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

    PANIC("invalid node in compile_expr", node->get_kind());
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

    if (!ret_type[0]->is_integral()) {
        ctx.er->report_bug(node->get_loc(),
                           "non-integer returns not implemeted (found {})",
                           *ret_type[0]);
        return;
    }

    print(o, "function {} ${}(", to_qbe_integer(*ret_type[0]), decl->link_name);

    if (!data.get_args().params.empty()) {
        ctx.er->report_bug(data.args->get_loc(),
                           "function arguments not implemented");
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
}

void compile(Ast& ast, Node* root, ErrorReporter& er, types::TypeStore& ts,
             Options const& opt) {
    auto ctx = Context{.out = stdout, .er = &er};
    compile_flat_module(ast, root, ctx);
}

}  // namespace yal::compile::qbe
