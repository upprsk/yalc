#include "sema.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "hlir.hpp"
#include "span.hpp"
#include "types.hpp"

namespace yal {

struct Context {
    TypeHandle expected;
};

enum class DeclFlags {
    None = 0,
    Local = (1 << 0),
    Builtin = (1 << 1),
};

constexpr auto operator|(DeclFlags const& lhs, DeclFlags const& rhs)
    -> DeclFlags {
    return static_cast<DeclFlags>(fmt::underlying(lhs) | fmt::underlying(rhs));
}

constexpr auto operator&(DeclFlags const& lhs, DeclFlags const& rhs)
    -> DeclFlags {
    return static_cast<DeclFlags>(fmt::underlying(lhs) & fmt::underlying(rhs));
}

constexpr auto operator|=(DeclFlags& lhs, DeclFlags const& rhs) -> DeclFlags {
    return lhs = lhs | rhs;
}

struct Decl {
    DeclFlags   flags;
    std::string name;
    hlir::Value value;
    Span        span;

    [[nodiscard]] constexpr auto is_local() const -> bool {
        return (flags & DeclFlags::Local) == DeclFlags::Local;
    }

    [[nodiscard]] constexpr auto is_builtin() const -> bool {
        return (flags & DeclFlags::Builtin) == DeclFlags::Builtin;
    }
};

struct Env {
    auto child() -> Env { return {.decls = {}, .parent = this}; }

    auto lookup(std::string_view name) -> Decl* {
        for (auto& item : decls | std::ranges::views::reverse) {
            if (item.name == name) return &item;
        }

        return parent ? parent->lookup(name) : nullptr;
    }

    void declare(Span span, std::string name, hlir::Value value,
                 DeclFlags flags = DeclFlags::None) {
        decls.push_back({flags, name, value, span});
    }

    std::vector<Decl> decls;
    Env*              parent{};
};

struct SemaFunc {
    auto sema(Env& env) -> TypeHandle {
        auto e = env.child();

        auto p = ast->node_func(func_node_handle);

        // first, check for plain name of the function
        if (ast->get(p.name)->is_id()) {
            func->name = ast->get(p.name)->value_string();
        }

        // it might be a bound function
        else if (ast->get(p.name)->is_id_pack()) {
            er->report_bug(ast->get(func_node_handle)->span,
                           "bound functions have not been implemented");

            return ast->get_mut(func_node_handle)->set_type(ts->get_type_err());
        } else {
            er->report_bug(ast->get(p.name)->span,
                           "invalid node found for function name: {}",
                           ast->get(p.name)->kind);
            return ast->get_mut(func_node_handle)->set_type(ts->get_type_err());
        }

        std::vector<TypeHandle> args;
        for (auto const& arg : p.args) {
            auto ty = sema_func_arg(e, arg);
            args.push_back(ty);
        }

        auto ret = ts->get_type_void();
        ret_span = ast->get(p.name)->span;

        if (!ast->get(p.ret)->is_nil()) {
            ret = eval_to_type(e, p.ret);
            ret_span = ast->get(p.ret)->span;
        }

        func->type = ast->get_mut(func_node_handle)
                         ->set_type(ts->get_type_func(args, ret));

        env.declare(ast->get(func_node_handle)->span, func->name,
                    {.type = func->type, .value = func_handle});

        sema_block(e, p.body);

        return func->type;
    }

    // ------------------------------------------------------------------------

    auto sema_func_arg(Env& env, NodeHandle n) -> TypeHandle {
        auto node = ast->get_mut(n);
        if (!node->is_func_arg()) {
            er->report_bug(node->span, "invalid node in `sema_func_arg`: {}",
                           node->kind);
            return node->set_type(ts->get_type_err());
        }

        auto p = ast->node_named(*node);
        auto ty = eval_to_type(env, p.child);

        env.declare(node->span, std::string{p.name}, {.type = ty},
                    DeclFlags::Local);
        append_local(std::string{p.name}, ty);

        return node->set_type(ty);
    }

    auto sema_block(Env& env, NodeHandle n) -> TypeHandle {
        auto node = ast->get(n);
        if (!node->is_block()) {
            er->report_bug(node->span, "invalid node in sema_block: {}",
                           node->kind);
            return ast->get_mut(n)->set_type(ts->get_type_err());
        }

        auto e = env.child();
        auto p = ast->node_with_children(*node);
        for (auto const& c : p.children) {
            sema_stmt(e, c);
        }

        // FIXME: pop all locals define in the block (that are not used later)
        // How do you even do this?

        return ast->get_mut(n)->set_type(ts->get_type_void());
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    auto sema_stmt(Env& env, NodeHandle n) -> TypeHandle {
        auto node = ast->get(n);

        switch (node->kind) {
            case NodeKind::Block: return sema_block(env, n);

            case NodeKind::ExprStmt: {
                auto p = ast->node_with_child(*node);

                auto child = sema_expr(env, {}, p.child);
                if (!ts->get(child)->is_err() && !ts->get(child)->is_void()) {
                    er->report_error(node->span,
                                     "expression result is ignored");
                    er->report_note(ast->get(p.child)->span,
                                    "expression has type: {}",
                                    ts->fatten(child));
                }

                push_inst(node->span, hlir::InstKind::Pop, 1);

                return ast->get_mut(n)->set_type(ts->get_type_void());
            }

            case NodeKind::VarDecl: {
                auto p = ast->node_decl(*node);

                // first, check for multiple returns
                if (!ast->get(p.ids)->is_id()) {
                    er->report_bug(node->span,
                                   "support for multiple returns has not been "
                                   "implemented");
                    return ast->get_mut(n)->set_type(ts->get_type_void());
                }

                if (!ast->get(p.type)->is_nil()) {
                    er->report_bug(node->span,
                                   "explicit typing of parameters has not been "
                                   "implemented");
                }

                auto init = sema_expr(env, {}, p.init);

                auto name = ast->get(p.ids)->value_string();
                env.declare(node->span, name, {.type = init}, DeclFlags::Local);
                append_local(name, init);

                return ast->get_mut(n)->set_type(ts->get_type_void());
            }

            case NodeKind::Assign: {
                auto p = ast->node_with_child_pair(*node);

                if (ast->get(p.first)->is_id()) {
                    auto d = env.lookup(ast->get(p.first)->value_string());
                    if (!d) {
                        er->report_error(node->span, "undefined identifier: {}",
                                         node->value_string());
                        return ast->get_mut(n)->set_type(ts->get_type_err());
                    }

                    auto rhs =
                        sema_expr(env, {.expected = d->value.type}, p.second);

                    // FIXME: type coersion
                    if (rhs != d->value.type) {
                        er->report_error(node->span,
                                         "can't assign to value of type {} "
                                         "with value of type {}",
                                         ts->fatten(d->value.type),
                                         ts->fatten(rhs));

                        er->report_note(d->span, "declared here with type {}",
                                        ts->fatten(d->value.type));
                    }

                    push_inst_store(node->span, *d);
                } else {
                    er->report_error(node->span,
                                     "can't assign to non-l-value {}",
                                     ast->get(p.first)->kind);
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                return ast->get_mut(n)->set_type(ts->get_type_void());
            }

            case NodeKind::IfStmt: {
                auto p = ast->node_if_stmt(*node);

                auto cond =
                    sema_expr(env, {.expected = ts->get_type_bool()}, p.cond);
                if (!ts->get(cond)->is_bool()) {
                    er->report_error(
                        ast->get(p.cond)->span,
                        "expected boolean for if condition, got {}",
                        ts->fatten(cond));
                }

                auto wt = allocate_block();
                auto after = allocate_block();

                push_inst(node->span, hlir::InstKind::Branch, wt, after);

                set_active_block(wt);
                sema_stmt(env, p.when_true);

                push_inst(node->span, hlir::InstKind::Jump, after);

                set_active_block(after);

                return ast->get_mut(n)->set_type(ts->get_type_void());
            }

            case NodeKind::IfStmtWithElse: {
                auto p = ast->node_if_stmt(*node);

                auto cond =
                    sema_expr(env, {.expected = ts->get_type_bool()}, p.cond);
                if (!ts->get(cond)->is_bool()) {
                    er->report_error(
                        ast->get(p.cond)->span,
                        "expected boolean for if condition, got {}",
                        ts->fatten(cond));
                }

                auto wt = allocate_block();
                auto wf = allocate_block();
                auto after = allocate_block();

                push_inst(node->span, hlir::InstKind::Branch, wt, wf);

                set_active_block(wt);
                sema_stmt(env, p.when_true);
                push_inst(node->span, hlir::InstKind::Jump, after);

                set_active_block(wf);
                sema_stmt(env, p.when_false);
                push_inst(node->span, hlir::InstKind::Jump, after);

                set_active_block(after);

                return ast->get_mut(n)->set_type(ts->get_type_void());
            }

            case NodeKind::ReturnStmt: {
                auto p = ast->node_with_child(*node);
                auto ret = ts->get(func->type)->as_func(*ts).ret;

                auto ty = ts->get_type_void();
                if (!ast->get(p.child)->is_nil()) {
                    ty = sema_expr(env, {.expected = ret}, p.child);
                }

                // FIXME: allow type coercion
                if (!ts->get(ty)->is_err() && ret != ty) {
                    er->report_error(
                        ast->get(p.child)->span,
                        "type mismatch, function expects {} but got {}",
                        ts->fatten(ret), ts->fatten(ty));

                    er->report_note(ret_span,
                                    "function return specified as {} here",
                                    ts->fatten(ret));
                }

                push_inst(node->span, hlir::InstKind::Ret);

                return ast->get_mut(n)->set_type(ts->get_type_void());
            }

            default:
                er->report_bug(node->span, "invalid node in sema_stmt: {}",
                               node->kind);
                return ast->get_mut(n)->set_type(ts->get_type_err());
        }
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    auto sema_expr(Env& env, Context ctx, NodeHandle n) -> TypeHandle {
        auto node = ast->get(n);

        switch (node->kind) {
            case NodeKind::Equal:
            case NodeKind::NotEqual:
            case NodeKind::Smaller:
            case NodeKind::SmallerEqual:
            case NodeKind::Greater:
            case NodeKind::GreaterEqual: {
                auto p = ast->node_with_child_pair(*node);

                auto kind = hlir::InstKind::Eq;
                switch (node->kind) {
                    case NodeKind::Equal: kind = hlir::InstKind::Eq; break;
                    case NodeKind::NotEqual: kind = hlir::InstKind::Neq; break;
                    case NodeKind::Smaller: kind = hlir::InstKind::Lt; break;
                    case NodeKind::SmallerEqual:
                        kind = hlir::InstKind::Lte;
                        break;
                    case NodeKind::Greater: kind = hlir::InstKind::Gt; break;
                    case NodeKind::GreaterEqual:
                        kind = hlir::InstKind::Gte;
                        break;
                    default: __builtin_unreachable();
                }

                auto lhs = sema_expr(env, ctx, p.first);
                auto rhs = sema_expr(env, ctx, p.second);

                // FIXME: type coersion
                if ((!ts->get(lhs)->is_err() && !ts->get(rhs)->is_err()) &&
                    lhs != rhs) {
                    er->report_error(node->span, "type mismatch: {} and {}",
                                     ts->fatten(lhs), ts->fatten(rhs));
                }

                push_inst(node->span, kind);
                return ast->get_mut(n)->set_type(ts->get_type_bool());
            }

            case NodeKind::LogicNot: {
                auto p = ast->node_with_child(*node);

                auto kind = hlir::InstKind::LogicNot;
                switch (node->kind) {
                    case NodeKind::LogicNot:
                        kind = hlir::InstKind::LogicNot;
                        break;
                    default: __builtin_unreachable();
                }

                auto child = sema_expr(env, ctx, p.child);

                // FIXME: allow negating things other than integers
                if (!ts->get(child)->is_err() && !ts->get(child)->is_bool()) {
                    er->report_error(
                        node->span,
                        "can't use {} operation on non-boolean type: {}", kind,
                        ts->fatten(child));
                }

                push_inst(node->span, kind);
                return ast->get_mut(n)->set_type(ts->get_type_bool());
            }

            case NodeKind::Add:
            case NodeKind::Sub:
            case NodeKind::Mul:
            case NodeKind::Div: {
                auto p = ast->node_with_child_pair(*node);

                auto kind = hlir::InstKind::Add;
                switch (node->kind) {
                    case NodeKind::Add: kind = hlir::InstKind::Add; break;
                    case NodeKind::Sub: kind = hlir::InstKind::Sub; break;
                    case NodeKind::Mul: kind = hlir::InstKind::Mul; break;
                    case NodeKind::Div: kind = hlir::InstKind::Div; break;
                    default: __builtin_unreachable();
                }

                auto lhs = sema_expr(env, ctx, p.first);
                auto rhs = sema_expr(env, ctx, p.second);

                // FIXME: type coersion
                if ((!ts->get(lhs)->is_err() && !ts->get(rhs)->is_err()) &&
                    lhs != rhs) {
                    er->report_error(node->span, "type mismatch: {} and {}",
                                     ts->fatten(lhs), ts->fatten(rhs));
                }

                // FIXME: allow adding things other than integers
                if (!ts->get(lhs)->is_err() && !ts->get(lhs)->is_integral()) {
                    er->report_error(
                        node->span,
                        "can't use {} operation on non-integral type: {}", kind,
                        ts->fatten(lhs));
                }

                push_inst(node->span, kind);
                return ast->get_mut(n)->set_type(lhs);
            }

            case NodeKind::LogicAnd: {
                auto p = ast->node_with_child_pair(*node);

                // logical and has short-circuiting:
                // only check rhs in case lhs is true

                auto lhs = sema_expr(env, ctx, p.first);

                auto wt = allocate_block();
                auto after = allocate_block();
                push_inst(node->span, hlir::InstKind::Branch, wt, after);

                set_active_block(wt);
                auto rhs = sema_expr(env, ctx, p.second);

                push_inst(node->span, hlir::InstKind::Jump, after);
                set_active_block(after);

                // FIXME: type coersion
                if ((!ts->get(lhs)->is_err() && !ts->get(rhs)->is_err()) &&
                    lhs != rhs && !ts->get(lhs)->is_bool()) {
                    er->report_error(node->span,
                                     "type mismatch: expected booleans in {}, "
                                     "but got {} and {}",
                                     node->kind, ts->fatten(lhs),
                                     ts->fatten(rhs));
                }

                return ast->get_mut(n)->set_type(ts->get_type_bool());
            }

            case NodeKind::Neg: {
                auto p = ast->node_with_child(*node);

                auto patch_idx = push_inst_const(
                    node->span, {.type = {}, .value = uint64_t{0}});
                auto child = sema_expr(env, ctx, p.child);

                current_block()
                    ->consts.at(current_block()->code.at(patch_idx).a)
                    .type = child;

                // FIXME: allow adding things other than integers
                if (!ts->get(child)->is_err() &&
                    !ts->get(child)->is_integral()) {
                    er->report_error(
                        node->span,
                        "can't use {} operation on non-integral type: {}",
                        node->kind, ts->fatten(child));
                }

                push_inst(node->span, hlir::InstKind::Sub);

                return ast->get_mut(n)->set_type(child);
            }

            case NodeKind::Call: {
                auto p = ast->node_call(*node);

                // TODO: handle indirect calls
                if (!ast->get(p.callee)->is_id()) {
                    er->report_error(
                        node->span,
                        "can't call {} (indirect calls not implemented)",
                        ast->get(p.callee)->kind);
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                auto d = env.lookup(ast->get(p.callee)->value_string());
                if (!d) {
                    er->report_error(ast->get(p.callee)->span,
                                     "undefined identifier: {}",
                                     ast->get(p.callee)->value_string());
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                ast->get_mut(p.callee)->set_type(d->value.type);

                auto f = ts->get(d->value.type);
                if (!f->is_func()) {
                    er->report_bug(ast->get(p.callee)->span,
                                   "can't call non-func: {}",
                                   ts->fatten(d->value.type));
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                auto func = f->as_func(*ts);
                if (func.args.size() != p.args.size()) {
                    er->report_error(node->span,
                                     "wrong number of types for func call, "
                                     "expected {} but got {}",
                                     func.args.size(), p.args.size());

                    er->report_note(
                        d->span, "func expects ({})",
                        fmt::join(func.args | std::ranges::views::transform(
                                                  [&](auto h) {
                                                      return ts->fatten(h);
                                                  }),
                                  ", "));
                }

                for (size_t i = 0; i < p.args.size(); i++) {
                    auto v = sema_expr(
                        env,
                        {.expected = i < func.args.size() ? func.args[i]
                                                          : TypeHandle{}},
                        p.args[i]);
                    if (i < func.args.size()) {
                        // FIXME: type coercion
                        if (func.args[i] != v) {
                            er->report_error(ast->get(p.args[i])->span,
                                             "type mismatch, func expects {} "
                                             "for argument but got {}",
                                             ts->fatten(func.args[i]),
                                             ts->fatten(v));

                            er->report_note(
                                d->span, "func expects ({})",
                                fmt::join(
                                    func.args | std::ranges::views::transform(
                                                    [&](auto h) {
                                                        return ts->fatten(h);
                                                    }),
                                    ", "));
                        }
                    }
                }

                if (d->value.holds_func()) {
                    push_inst_call(node->span, d->value.value_func());
                } else {
                    er->report_bug(ast->get(p.callee)->span,
                                   "indirect calls have not been implemented");
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                return ast->get_mut(n)->set_type(func.ret);
            }

            case NodeKind::Id: {
                auto d = env.lookup(node->value_string());
                if (!d) {
                    er->report_error(node->span, "undefined identifier: {}",
                                     node->value_string());
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                push_inst_load(node->span, *d);
                return ast->get_mut(n)->set_type(d->value.type);
            }

            case NodeKind::Int: {
                auto ty = ts->get_type_i32();

                if (ctx.expected.is_valid()) {
                    auto expected = ts->get(ctx.expected);
                    if (expected->is_integral()) {
                        ty = ctx.expected;
                    }
                }

                push_inst_const(node->span,
                                {.type = ty, .value = node->value_uint64()});
                return ast->get_mut(n)->set_type(ty);
            }

            default:
                er->report_bug(node->span, "invalid node in sema_expr: {}",
                               node->kind);
                return ast->get_mut(n)->set_type(ts->get_type_err());
        }
    }

    // ------------------------------------------------------------------------

    auto eval_to_type(Env& env, NodeHandle n) const -> TypeHandle {
        auto node = ast->get_mut(n);

        switch (node->kind) {
            case NodeKind::Id: {
                auto d = env.lookup(node->value_string());
                if (!d) {
                    er->report_error(node->span, "undefined identifier: {}",
                                     node->value_string());
                    return ast->get_mut(n)->set_type(ts->get_type_err());
                }

                if (!ts->get(d->value.type)->is_type()) {
                    er->report_error(node->span,
                                     "can't use value of type `{}` as type",
                                     ts->fatten(d->value.type));
                    return node->set_type(ts->get_type_err());
                }

                node->set_type(ts->get_type_type());

                return d->value.value_type();
            }

            default:
                er->report_error(node->span, "can't evaluate {} to type",
                                 node->kind);
                return node->set_type(ts->get_type_err());
        }
    }

    // ------------------------------------------------------------------------

    // NOLINTNEXTLINE(modernize-use-nodiscard)
    auto push_inst(Span s, hlir::InstKind kind, uint8_t a = 0,
                   uint8_t b = 0) const -> size_t {
        // er->report_note(s, "push_inst({}, {}, {})", s, kind, a);

        auto blk = current_block();
        auto sz = blk->code.size();

        blk->spans.push_back(s);
        blk->code.push_back({kind, a, b});

        return sz;
    }

    // NOLINTNEXTLINE(modernize-use-nodiscard)
    auto push_inst_const(Span s, hlir::Value v) const -> size_t {
        auto blk = current_block();
        auto sz = blk->consts.size();
        if (sz == std::numeric_limits<uint8_t>::max())
            throw std::runtime_error{"too many constants in block"};

        blk->consts.push_back(v);
        return push_inst(s, hlir::InstKind::Const, sz);
    }

    // NOLINTNEXTLINE(modernize-use-nodiscard)
    auto push_inst_call(Span s, hlir::FuncHandle fn) const -> size_t {
        auto sz = func->calls.size();
        if (sz == std::numeric_limits<uint8_t>::max())
            throw std::runtime_error{"too many calls in func"};

        func->calls.push_back(fn);
        return push_inst(s, hlir::InstKind::Call, sz);
    }

    // NOLINTNEXTLINE(modernize-use-nodiscard)
    auto push_inst_load(Span s, Decl const& decl) const -> size_t {
        if (decl.is_local()) {
            auto [loc, off] = lookup_local(decl.name);
            if (!loc)
                throw std::runtime_error{
                    fmt::format("push_inst_load({}): marked as local but not "
                                "found in locals",
                                decl.name)};

            return push_inst(s, hlir::InstKind::LoadLocal, off);
        }

        // this is a value that we can inline (without doing an actual load)
        if (decl.is_builtin()) {
            return push_inst_const(s, decl.value);
        }

        throw std::runtime_error{fmt::format(
            "push_inst_load({}): non-locals have not been implemented",
            decl.name)};
    }

    // NOLINTNEXTLINE(modernize-use-nodiscard)
    auto push_inst_store(Span s, Decl const& decl) const -> size_t {
        if (decl.is_local()) {
            auto [loc, off] = lookup_local(decl.name);
            if (!loc)
                throw std::runtime_error{
                    fmt::format("push_inst_load({}): marked as local but not "
                                "found in locals",
                                decl.name)};

            return push_inst(s, hlir::InstKind::StoreLocal, off);
        }

        throw std::runtime_error{fmt::format(
            "push_inst_store({}): non-locals have not been implemented",
            decl.name)};
    }

    [[nodiscard]] auto current_block() const -> hlir::Block* {
        // make sure we have at least one block
        if (func->blocks.size() == 0) func->blocks.push_back({});

        return &func->blocks.at(current_block_idx);
    }

    [[nodiscard]] auto allocate_block() const -> size_t {
        // make sure we have at least one block
        if (func->blocks.size() == 0) func->blocks.push_back({});

        auto sz = func->blocks.size();
        func->blocks.push_back({});
        return sz;
    }

    constexpr void set_active_block(size_t block) { current_block_idx = block; }

    [[nodiscard]] auto lookup_local(std::string_view name) const
        -> std::pair<hlir::Local const*, size_t> {
        size_t i{};
        for (auto& item : func->locals | std::ranges::views::reverse) {
            if (item.name == name) return {&item, func->locals.size() - i - 1};
            i++;
        }

        return {nullptr, 0};
    }

    void append_local(std::string name, TypeHandle ty) {
        func->locals.push_back({name, ty, stack_top++});
    }

    void pop_local() {
        if (stack_top == 0)
            throw std::runtime_error{"popping empty virtual stack"};

        stack_top--;
    }

    // ------------------------------------------------------------------------

    hlir::Func*      func;
    hlir::FuncHandle func_handle;
    Span             ret_span{};
    uint8_t          stack_top{};
    size_t           current_block_idx{};

    Ast*           ast;
    TypeStore*     ts;
    ErrorReporter* er;
    NodeHandle     func_node_handle;
};

struct Sema {
    auto sema_node(Env& env, NodeHandle n) -> TypeHandle {
        auto node = ast->get_mut(n);

        switch (node->kind) {
            case NodeKind::Err:
                er->report_error(node->span, "found error node in sema");
                return node->set_type(ts->get_type_err());

            case NodeKind::Nil:
                er->report_bug(node->span, "found nil node in sema");
                return node->set_type(ts->get_type_err());

            case NodeKind::File: {
                auto e = env.child();

                auto p = ast->node_with_children(*node);
                for (auto const& node : p.children) sema_node(e, node);

                return node->set_type(ts->get_type_void());
            }

            case NodeKind::Func: return sema_node_func(env, n);

            case NodeKind::FuncExtern:
                er->report_bug(node->span, "{} not implemented", node->kind);
                return ts->get_type_err();

            default:
                er->report_bug(node->span,
                               "invalid node found in the top-level: {}",
                               node->kind);
                return ts->get_type_err();
        }
    }

    // ------------------------------------------------------------------------

    auto sema_node_func(Env& env, NodeHandle h) -> TypeHandle {
        auto fn = mod.add_func({});

        auto sf = SemaFunc{
            .func = mod.get_mut(fn),
            .func_handle = fn,
            .ast = ast,
            .ts = ts,
            .er = er,
            .func_node_handle = h,
        };

        auto ty = sf.sema(env);

        return ty;
    }

    // ------------------------------------------------------------------------

    hlir::Module mod{};

    Ast*           ast;
    TypeStore*     ts;
    ErrorReporter* er;
};

auto sema(Ast& ast, TypeStore& ts, NodeHandle root, ErrorReporter& er)
    -> hlir::Module {
    auto s = Sema{.ast = &ast, .ts = &ts, .er = &er};

    Env env;
    env.declare({}, "i32",
                {.type = ts.get_type_type(), .value = ts.get_type_i32()});

    env.declare({}, "bool",
                {.type = ts.get_type_type(), .value = ts.get_type_bool()});

    env.declare({}, "false", {.type = ts.get_type_bool(), .value = uint64_t{0}},
                DeclFlags::Builtin);
    env.declare({}, "true", {.type = ts.get_type_bool(), .value = uint64_t{1}},
                DeclFlags::Builtin);

    s.sema_node(env, root);

    return s.mod;
}

}  // namespace yal
