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

struct Label {
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

template <>
struct fmt::formatter<yal::compile::qbe::Label> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::compile::qbe::Label const& label,
                format_context& ctx) const -> format_context::iterator {
        return fmt::format_to(ctx.out(), "@l{}", label.v);
    }
};

namespace yal::compile::qbe {

struct Context {
    FILE*          out{};
    ErrorReporter* er{};

    uint32_t                      next_tmp{};
    uint32_t                      next_block{};
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

    [[nodiscard]] constexpr auto new_block() -> Label { return {next_block++}; }
};

auto ptr_to_qbe_integer() -> std::string_view {
    return sizeof(void*) == 4 ? "w" : "l";
}

auto to_qbe_integer_tmp(types::Type const& type) -> std::string_view {
    auto type_ptr = &type;
    while (type_ptr->is_distinct()) type_ptr = type_ptr->inner[0];

    std::string_view ptr_type = ptr_to_qbe_integer();

    switch (type_ptr->kind) {
        case types::TypeKind::Uint64:
        case types::TypeKind::Int64: return "l";
        case types::TypeKind::Uint32:
        case types::TypeKind::Int32:
        case types::TypeKind::Uint16:
        case types::TypeKind::Int16:
        case types::TypeKind::Uint8:
        case types::TypeKind::Int8: return "w";
        case types::TypeKind::Usize:
        case types::TypeKind::Isize: return ptr_type;
        case types::TypeKind::Bool: return "w";
        case types::TypeKind::Float32: return "f";
        case types::TypeKind::Float64: return "d";
        case types::TypeKind::Ptr:
        case types::TypeKind::PtrConst:
        case types::TypeKind::MultiPtr:
        case types::TypeKind::MultiPtrConst: return ptr_type;
        default:
            PANIC("not a valid integer type for qbe", type_ptr->kind,
                  fmt::to_string(type_ptr->kind));
    }
}

auto to_qbe_integer_exact(types::Type const& type) -> std::string_view {
    auto type_ptr = &type;
    while (type_ptr->is_distinct()) type_ptr = type_ptr->inner[0];

    std::string_view ptr_type = ptr_to_qbe_integer();

    switch (type_ptr->kind) {
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
            PANIC("not a valid integer type for qbe", type_ptr->kind,
                  fmt::to_string(type_ptr->kind));
    }
}

void compile_stmt(Ast& ast, Node* node, Context& ctx);
void compile_expr(Ast& ast, Node* node, Context& ctx);

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
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
                to_qbe_integer_tmp(*data.init->get_type()), tmp);
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
                to_qbe_integer_tmp(*data.names[0]->get_type()), tmp);

        return;
    }

    if (node->is_oneof(ast::NodeKind::Block)) {
        auto data = conv::block(*node);
        for (auto child : data.items) {
            compile_stmt(ast, child, ctx);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::IfStmt)) {
        auto data = conv::if_stmt(*node);
        compile_expr(ast, data.cond, ctx);

        // no else
        if (data.wf == nullptr) {
            auto wt = ctx.new_block();
            auto after = ctx.new_block();

            auto tmp = ctx.pop_tmp();
            println(o, "    jnz {}, {}, {}", tmp, wt, after);
            println(o, "{}", wt);

            compile_stmt(ast, data.wt, ctx);
            println(o, "{}", after);
        }

        // got else
        else {
            auto wt = ctx.new_block();
            auto wf = ctx.new_block();
            auto after = ctx.new_block();

            auto tmp = ctx.pop_tmp();
            println(o, "    jnz {}, {}, {}", tmp, wt, wf);
            println(o, "{}", wt);

            compile_stmt(ast, data.wt, ctx);
            println(o, "    jmp {}", after);
            println(o, "{}", wf);

            compile_stmt(ast, data.wf, ctx);

            println(o, "{}", after);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::WhileStmt)) {
        auto data = conv::while_stmt(*node);

        auto start = ctx.new_block();
        println(o, "{}", start);

        compile_expr(ast, data.cond, ctx);

        auto then = ctx.new_block();
        auto after = ctx.new_block();

        auto tmp = ctx.pop_tmp();
        println(o, "    jnz {}, {}, {}", tmp, then, after);
        println(o, "{}", then);

        compile_stmt(ast, data.body, ctx);
        println(o, "    jmp {}", start);
        println(o, "{}", after);

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

    if (node->is_oneof(ast::NodeKind::AssignDirect)) {
        auto data = conv::assign(*node);

        if (data.lhs->is_oneof(ast::NodeKind::Id)) {
            auto lhs = conv::id(*data.lhs);
            compile_expr(ast, data.rhs, ctx);
            auto rhs = ctx.pop_tmp();

            println(o, "    %{} ={} copy {}", lhs.to->link_name,
                    to_qbe_integer_tmp(*data.lhs->get_type()), rhs);
        }

        else {
            PANIC("other assigmemt kinds not implemented",
                  data.lhs->get_kind());
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

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void compile_expr(Ast& ast, Node* node, Context& ctx) {
    auto o = ctx.out;

    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);
        for (auto child : data.items) compile_expr(ast, child, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Cast)) {
        auto data = conv::binary(*node);
        auto source = data.lhs->get_type();
        auto target = node->get_type();

        while (target->is_distinct()) target = target->inner[0];
        while (source->is_distinct()) source = source->inner[0];

        compile_expr(ast, data.lhs, ctx);
        if (source->is_integral() && target->is_integral()) {
            // both are integers
            if (source->size() < target->size()) {
                // don't need to do anything because of subtyping in qbe
            }

            else {
                auto tmp = ctx.pop_tmp();
                auto res = ctx.push_tmp();
                auto ty_exact = to_qbe_integer_exact(*target);
                auto ty = to_qbe_integer_tmp(*target);

                // need to extend
                if (target->is_signed()) {
                    println(o, "    {} ={} exts{} {}", res, ty, ty_exact, tmp);
                } else {
                    println(o, "    {} ={} extu{} {}", res, ty, ty_exact, tmp);
                }
            }
        }

        else if ((source->is_ptr() && target->is_ptr()) ||
                 (source->is_mptr() && target->is_mptr())) {
            // don't need to do anything because pointers always have the same
            // size
        }

        else {
            PANIC("other conversions not implemented", *target, *source);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        auto data = conv::integers(*node);
        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} copy {}", tmp,
                to_qbe_integer_tmp(*node->get_type()), data.value);

        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        // auto data = conv::id(*node);
        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} copy %{}", tmp,
                to_qbe_integer_tmp(*node->get_type()),
                node->get_decl()->link_name);
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

    if (node->is_oneof(ast::NodeKind::Add, ast::NodeKind::Sub,
                       ast::NodeKind::Less, ast::NodeKind::Greater)) {
        auto data = conv::binary(*node);
        compile_expr(ast, data.lhs, ctx);
        compile_expr(ast, data.rhs, ctx);

        std::string op;
        switch (node->get_kind()) {
            case ast::NodeKind::Add: op = "add"; break;
            case ast::NodeKind::Sub: op = "sub"; break;
            case ast::NodeKind::Less:
                if (data.lhs->get_type()->is_signed())
                    op = fmt::format("cslt{}",
                                     to_qbe_integer_tmp(*node->get_type()));
                else
                    op = fmt::format("cult{}",
                                     to_qbe_integer_tmp(*node->get_type()));
                break;
            case ast::NodeKind::Greater:
                if (data.lhs->get_type()->is_signed())
                    op = fmt::format("csgt{}",
                                     to_qbe_integer_tmp(*node->get_type()));
                else
                    op = fmt::format("cugt{}",
                                     to_qbe_integer_tmp(*node->get_type()));
                break;
            default:
                UNREACHABLE("invalid node kind in binary", node->get_kind());
        }

        auto b = ctx.pop_tmp();
        auto a = ctx.pop_tmp();
        auto tmp = ctx.push_tmp();
        println(o, "    {} ={} {} {}, {}", tmp,
                to_qbe_integer_tmp(*node->get_type()), op, a, b);
        return;
    }

    if (node->is_oneof(ast::NodeKind::CallDirect)) {
        auto data = conv::call_direct(*node);
        auto func_type = node->get_decl()->get_type()->as_func();

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

            print(o, "    {} ={} ", tmp, to_qbe_integer_tmp(*r));
        } else {
            print(o, "    ");
        }

        print(o, "call ${}", node->get_decl()->link_name);

        std::vector<Tmp> args;
        for (size_t i = 0; i < data.args.size(); i++) {
            args.push_back(ctx.pop_tmp());
        }

        print(o, "(");

        for (auto [tmp, arg] : rv::zip(rv::reverse(args), data.args)) {
            print(o, "{} {}, ", to_qbe_integer_tmp(*arg->get_type()), tmp);
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
        print(o, "{} ", to_qbe_integer_tmp(*ret_type[0]));
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
        print(o, "{} %{},", to_qbe_integer_tmp(*p->get_type()),
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

        else if (child->is_oneof(ast::NodeKind::TopDefDecl)) {
            // ignore defs
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
