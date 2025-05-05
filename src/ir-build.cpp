#include "ir-build.hpp"

#include <algorithm>
#include <ranges>
#include <vector>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "file-store.hpp"
#include "fmt/ranges.h"
#include "ir.hpp"

namespace yal::ir {
using ast::Ast;
using ast::Node;
namespace conv = ast::conv;

namespace rv = std::ranges::views;

struct State {
    void               sstack_push(Inst* inst) { shadow_stack.push_back(inst); }
    [[nodiscard]] auto sstack_pop() -> Inst* {
        auto inst = shadow_stack.at(shadow_stack.size() - 1);
        shadow_stack.pop_back();
        return inst;
    }

    Module module{};

    // NOLINTBEGIN(readability-redundant-member-init)
    std::vector<Inst*>  shadow_stack{};
    std::vector<Inst*>  current_block{};
    std::vector<Block*> blocks{};
    // NOLINTEND(readability-redundant-member-init)

    types::TypeStore* ts;
    ErrorReporter*    er;
    Options const*    opt;
};

struct Context {
    [[nodiscard]] auto with_defers(std::vector<Node*>& defers) const
        -> Context {
        return {.defers = &defers};
    }

    void add_defer(Node* node) {
        ASSERT(defers != nullptr);
        defers->push_back(node);
    }

    std::vector<Node*>* defers{};
};

// ============================================================================

auto create_ir_type_from_general(Module& mod, types::Type const& ty) -> Type* {
    switch (ty.kind) {
        case types::TypeKind::Uint64: return mod.new_type(TypeKind::Uint64);
        case types::TypeKind::Int64: return mod.new_type(TypeKind::Int64);
        case types::TypeKind::Uint32: return mod.new_type(TypeKind::Uint32);
        case types::TypeKind::Int32: return mod.new_type(TypeKind::Int32);
        case types::TypeKind::Uint16: return mod.new_type(TypeKind::Uint16);
        case types::TypeKind::Int16: return mod.new_type(TypeKind::Int16);
        case types::TypeKind::Uint8: return mod.new_type(TypeKind::Uint8);
        case types::TypeKind::Int8: return mod.new_type(TypeKind::Int8);
        case types::TypeKind::Usize: return mod.new_type(TypeKind::Usize);
        case types::TypeKind::Isize: return mod.new_type(TypeKind::Isize);
        case types::TypeKind::Bool: return mod.new_type(TypeKind::Uint8);
        case types::TypeKind::Float32: return mod.new_type(TypeKind::Float32);
        case types::TypeKind::Float64: return mod.new_type(TypeKind::Float64);

        case types::TypeKind::Ptr:
        case types::TypeKind::PtrConst:
        case types::TypeKind::MultiPtr:
        case types::TypeKind::MultiPtrConst: return mod.new_type(TypeKind::Ptr);

        case types::TypeKind::Pack:
            if (ty.inner.size() == 1)
                return create_ir_type_from_general(mod, *ty.inner[0]);
            PANIC("packs not implemented");

        default: PANIC("invalid type", ty.kind);
    }
}

// ============================================================================

void visit_expr(Node* node, State& state, Context& ctx) {
    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);
        if (data.items.size() > 1) {
            state.er->report_bug(node->get_loc(),
                                 "return of multiple values not implemented");
            PANIC("not implemented");
        }

        visit_expr(data.items[0], state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        auto type =
            create_ir_type_from_general(state.module, *node->get_type());
        auto inst = state.module.new_inst_int_const(type, node->get_data_u64());
        state.current_block.push_back(inst);
        state.sstack_push(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::CallDirect)) {
        auto data = conv::call_direct(*node);
        auto ty = node->get_type();

        for (auto arg : data.args) visit_expr(arg, state, ctx);

        std::vector<Inst*> args;
        for (size_t i = 0; i < data.args.size(); i++)
            args.push_back(state.sstack_pop());
        std::ranges::reverse(args);

        // void return
        if (ty->is_pack() && ty->inner[0]->is_void()) {
            auto inst = state.module.new_inst_call_void(
                node->get_decl()->link_name, args);

            state.current_block.push_back(inst);
            state.sstack_push(nullptr);  // FIXME: is this ok?
            return;
        }

        // returns something

        auto type = create_ir_type_from_general(state.module, *ty);
        auto inst =
            state.module.new_inst_call(type, node->get_decl()->link_name, args);

        state.current_block.push_back(inst);
        state.sstack_push(inst);
        return;
    }

    state.er->report_bug(node->get_loc(), "expression not implemented");
    PANIC("not implemented", node->get_kind());
}

// ----------------------------------------------------------------------------

void visit_stmt(Node* node, State& state, Context& ctx) {
    // auto& ts = *state.ts;
    auto& er = *state.er;

    if (node->is_oneof(ast::NodeKind::Block)) {
        auto data = conv::block(*node);
        auto initial = state.shadow_stack.size();

        std::vector<Node*> defers;
        auto               sctx = ctx.with_defers(defers);

        for (auto item : data.items) visit_stmt(item, state, sctx);

        for (auto d : defers) {
            er.report_bug(d->get_loc(), "defer not handled");
        }

        auto ending = state.shadow_stack.size();
        ASSERT(initial == ending);

        return;
    }

    if (node->is_oneof(ast::NodeKind::ReturnStmt)) {
        auto data = conv::unary(*node);
        if (data.child == nullptr) {
            auto block = state.module.new_block(BlockOp::RetVoid, nullptr,
                                                state.current_block, {});

            state.current_block.clear();
            state.blocks.push_back(block);
            return;
        }

        visit_expr(data.child, state, ctx);
        auto expr = state.sstack_pop();

        auto block =
            state.module.new_block(BlockOp::Ret, expr, state.current_block, {});

        state.current_block.clear();
        state.blocks.push_back(block);
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprStmt)) {
        auto data = conv::unary(*node);
        visit_expr(data.child, state, ctx);
        (void)state.sstack_pop();
        return;
    }

    state.er->report_bug(node->get_loc(), "statement not implemented");
    PANIC("not implemented", node->get_kind());
}

// ----------------------------------------------------------------------------

auto create_func_params(std::span<types::Type*> types, State& state)
    -> std::span<Type*> {
    auto out = state.module.new_type_span(types.size());
    for (auto [idx, param] : rv::enumerate(types)) {
        out[idx] = create_ir_type_from_general(state.module, *param);
    }

    return state.module.new_type_span(out);
}

auto create_func_ret(Node* node, std::span<types::Type*> types, State& state)
    -> Type* {
    if (types.size() > 1) {
        state.er->report_bug(node->get_loc(),
                             "multiple return types have not been implemented");
        PANIC("not implemented");
    }

    auto& ty = *types[0];
    if (ty.is_void()) return nullptr;

    return create_ir_type_from_general(state.module, ty);
}

void visit_func_decl(Node* node, State& state, Context& ctx) {
    // auto& ts = *state.ts;
    // auto& er = *state.er;

    auto decl = node->get_decl();
    auto ty = node->get_type()->as_func();

    auto data = conv::func_decl(*node);
    auto params = create_func_params(ty.get_params(), state);
    auto ret = create_func_ret(data.ret, ty.get_ret(), state);

    Block* body = nullptr;
    if (data.body) {
        visit_stmt(data.body, state, ctx);

        // FIXME: implicit returns on void functions.
        body = state.blocks[0];
    }

    auto flags = Func::FlagNone;
    if (decl->flags.has_export()) {
        flags = static_cast<Func::Flags>(flags | Func::FlagExport);
    }

    if (decl->flags.has_extern()) {
        flags = static_cast<Func::Flags>(flags | Func::FlagExtern);
    }

    state.module.add_func({
        .link_name = decl->link_name,
        .params = params,
        .ret = ret,
        .body = body,
        .all_blocks = state.blocks,
        .loc = node->get_loc(),
        .flags = flags,
    });

    state.current_block.clear();
    state.blocks.clear();
}

// ----------------------------------------------------------------------------

void visit_top(Node* node, State& state, Context& ctx) {
    if (node->is_oneof(ast::NodeKind::FuncDecl)) {
        visit_func_decl(node, state, ctx);
        return;
    }

    PANIC("invalid top node", node->get_kind());
}

// ----------------------------------------------------------------------------

void visit_flat_module(Node* node, State& state, Context& ctx) {
    auto data = conv::flat_module(*node);
    for (auto top : data.children) {
        visit_top(top, state, ctx);
    }
}

// ----------------------------------------------------------------------------

auto build(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
           types::TypeStore& ts, Options const& opt) -> Module {
    (void)ast;

    auto state = State{.ts = &ts, .er = &er, .opt = &opt};
    auto ctx = Context{};
    visit_flat_module(root, state, ctx);

    auto m = std::move(state.module);
    return m;
}

}  // namespace yal::ir
