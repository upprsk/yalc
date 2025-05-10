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
#include "types.hpp"

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

    void add_local(Decl* decl, Inst* init) { locals[decl] = init; }
    [[nodiscard]] auto get_local(Decl* decl) const -> Inst* {
        return locals.at(decl);
    }

    void clear_all_data() {
        pending_insts.clear();
        blocks.clear();
        locals.clear();
    }

    auto new_empty_block(BlockOp op) -> Block* {
        return module.new_block(op, nullptr, {}, {});
    }

    void add_inst(Inst* inst) { pending_insts.push_back(inst); }
    void add_and_push_inst(Inst* inst) {
        add_inst(inst);
        sstack_push(inst);
    }

    void close_into_block(Block* block) {
        ASSERT(block->op != BlockOp::Err);

        block->body = module.new_inst_span(pending_insts);
        blocks.push_back(block);

        pending_insts.clear();
    }

    void close_into_pending_block(BlockOp op, Inst* value,
                                  std::span<Block* const> next) {
        ASSERT(pending_block != nullptr);

        pending_block->op = op;
        pending_block->value = value;
        pending_block->next = module.new_block_span(next);
        pending_block->body = module.new_inst_span(pending_insts);
        blocks.push_back(pending_block);

        pending_insts.clear();
        pending_block = nullptr;
    }

    Module module{};

    // NOLINTBEGIN(readability-redundant-member-init)
    std::unordered_map<Decl*, Inst*> locals{};

    std::vector<Inst*>  shadow_stack{};
    std::vector<Inst*>  pending_insts{};
    std::vector<Block*> blocks{};

    Block* pending_block{};
    // NOLINTEND(readability-redundant-member-init)

    types::TypeStore* ts;
    ErrorReporter*    er;
    Options const*    opt;
};

struct Context {
    [[nodiscard]] auto with_defers(std::vector<Node*>& defers) -> Context {
        return {.parent = this, .defers = &defers};
    }

    void add_defer(Node* node) {
        ASSERT(defers != nullptr);
        defers->push_back(node);
    }

    void for_all_defers(auto&& f) {
        if (defers == nullptr) return;

        if (parent) parent->for_all_defers(std::forward<decltype(f)>(f));
        for (auto d : rv::reverse(*defers)) f(d);
    }

    void clear_defers() const {
        if (defers) defers->clear();
    }

    void clear_all_defers() const {
        clear_defers();
        if (parent) parent->clear_defers();
    }

    Context*            parent{};
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

        case types::TypeKind::Distinct:
            return create_ir_type_from_general(mod, *ty.inner[0]);

        default: PANIC("invalid type", ty.kind);
    }
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_expr(Node* node, State& state, Context& ctx) {
    auto& module = state.module;

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
        auto type = create_ir_type_from_general(module, *node->get_type());
        auto inst = module.new_inst_int_const(type, node->get_data_u64());
        state.add_and_push_inst(inst);
        return;
    }

    // FIXME: THIS IS A TEMPORARY HACK TO GET STRINGS TO WORK!
    if (node->is_oneof(ast::NodeKind::Field) &&
        node->get_child(0)->is_oneof(ast::NodeKind::Str) &&
        conv::field(*node).name == "ptr") {
        auto type = module.new_type(TypeKind::Ptr);
        auto inst = module.new_inst_str_const(
            type, conv::str(*node->get_child(0)).value);
        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Index)) {
        auto data = conv::index(*node);
        visit_expr(data.receiver, state, ctx);
        visit_expr(data.index, state, ctx);

        auto type = create_ir_type_from_general(module, *node->get_type());

        auto index = state.sstack_pop();
        auto receiver = state.sstack_pop();

        // in case the index type is smaller then the pointer size, we need to
        // extend it
        if (index->type->size() < module.get_type_isize()->size()) {
            index = module.new_inst_ext(module.get_type_isize(), index);
            state.add_inst(index);
        }

        // receiver + (index * sizeof(*receiver))
        auto inst_sizeof_receiver =
            module.new_inst_int_const(index->type, receiver->type->size());
        auto inst_index_mult = module.new_inst_arith(
            OpCode::Mul, index->type, index, inst_sizeof_receiver);
        auto ptr = module.new_inst_arith(OpCode::Add, receiver->type, receiver,
                                         inst_index_mult);
        auto inst = module.new_inst_load(type, ptr);

        state.add_inst(inst_sizeof_receiver);
        state.add_inst(inst_index_mult);
        state.add_inst(ptr);
        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        // auto type = create_ir_type_from_general(module, *node->get_type());
        auto local = state.get_local(node->get_decl());
        // auto inst = module.new_inst_get_local(type, local);

        // state.add_and_push_inst(inst);
        state.sstack_push(local);
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
            auto inst =
                module.new_inst_call_void(node->get_decl()->link_name, args);

            // FIXME: there is actually no value to push here?
            state.add_and_push_inst(inst);
            return;
        }

        // returns something

        auto type = create_ir_type_from_general(module, *ty);
        auto inst =
            module.new_inst_call(type, node->get_decl()->link_name, args);

        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Add, ast::NodeKind::Sub,
                       ast::NodeKind::Div, ast::NodeKind::Mul,
                       ast::NodeKind::Equal, ast::NodeKind::NotEqual,
                       ast::NodeKind::Less, ast::NodeKind::LessEqual,
                       ast::NodeKind::Greater, ast::NodeKind::GreaterEqual)) {
        auto data = conv::binary(*node);
        visit_expr(data.lhs, state, ctx);
        visit_expr(data.rhs, state, ctx);
        auto rhs = state.sstack_pop();
        auto lhs = state.sstack_pop();

        OpCode op;
        switch (node->get_kind()) {
            case ast::NodeKind::Add: op = OpCode::Add; break;
            case ast::NodeKind::Sub: op = OpCode::Sub; break;
            case ast::NodeKind::Div: op = OpCode::Div; break;
            case ast::NodeKind::Mul: op = OpCode::Mul; break;
            case ast::NodeKind::Equal: op = OpCode::Eq; break;
            case ast::NodeKind::NotEqual: op = OpCode::Neq; break;
            case ast::NodeKind::Less: op = OpCode::Lt; break;
            case ast::NodeKind::LessEqual: op = OpCode::Le; break;

            case ast::NodeKind::Greater:
                op = OpCode::Lt;
                std::swap(lhs, rhs);
                break;

            case ast::NodeKind::GreaterEqual:
                op = OpCode::Le;
                std::swap(lhs, rhs);
                break;

            default:
                UNREACHABLE("invalid node kind in arith", node->get_kind());
        }

        auto type = create_ir_type_from_general(module, *node->get_type());
        auto inst = module.new_inst_arith(op, type, lhs, rhs);

        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Coerce)) {
        auto data = conv::coerce(*node);
        visit_expr(data.child, state, ctx);

        if (*data.target == *node->get_type()) return;

        PANIC("not implemented");
        return;
    }

    state.er->report_bug(node->get_loc(), "expression not implemented");
    PANIC("not implemented", node->get_kind());
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void visit_stmt(Node* node, State& state, Context& ctx) {
    auto& module = state.module;

    // auto& ts = *state.ts;
    // auto& er = *state.er;

    if (node->is_oneof(ast::NodeKind::Block)) {
        auto data = conv::block(*node);
        auto initial = state.shadow_stack.size();

        std::vector<Node*> defers;
        auto               sctx = ctx.with_defers(defers);

        for (auto item : data.items) visit_stmt(item, state, sctx);

        for (auto d : rv::reverse(defers)) {
            visit_stmt(d, state, sctx);
        }

        auto ending = state.shadow_stack.size();
        ASSERT(initial == ending);
        return;
    }

    if (node->is_oneof(ast::NodeKind::UnscopedGroup)) {
        auto data = conv::unscoped_group(*node);
        for (auto item : data.items) visit_stmt(item, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeclLocalVarDirect)) {
        auto data = conv::decl_local_var_direct(*node);

        visit_expr(data.init, state, ctx);

        auto init = state.sstack_pop();
        state.add_local(node->get_decl(), init);
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeclLocalVarDirectPack)) {
        auto data = conv::decl_local_var_direct_pack(*node);

        visit_expr(data.init, state, ctx);

        if (data.names.size() == 1) {
            auto init = state.sstack_pop();
            state.add_local(data.names[0]->get_decl(), init);
            return;
        }

        PANIC("var decl with multiple returns not implemented");
        return;
    }

    if (node->is_oneof(ast::NodeKind::AssignDirect)) {
        auto data = conv::assign(*node);
        auto lhs = state.get_local(conv::id(*data.lhs).to);

        visit_expr(data.rhs, state, ctx);
        auto rhs = state.sstack_pop();

        auto inst = module.new_inst_settmp(lhs, rhs);
        state.add_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeferStmt)) {
        auto data = conv::defer_stmt(*node);
        ctx.add_defer(data.stmt);
        return;
    }

    if (node->is_oneof(ast::NodeKind::IfStmt)) {
        auto data = conv::if_stmt(*node);

        // without else
        if (data.wf == nullptr) {
            visit_expr(data.cond, state, ctx);
            auto cond = state.sstack_pop();

            auto wt = state.new_empty_block(BlockOp::Jmp);
            auto after = state.new_empty_block(BlockOp::Err);

            // finish the current block using a branch op
            state.close_into_pending_block(BlockOp::Branch, cond,
                                           std::array{wt, after});

            // set then branch as the current pending block
            state.pending_block = wt;

            // fill the then branch and finish it with a simple jump
            visit_stmt(data.wt, state, ctx);
            if (state.pending_block)
                state.close_into_pending_block(BlockOp::Jmp, nullptr,
                                               std::array{after});

            // set after as the current block
            state.pending_block = after;
            return;
        }

        // with else
        PANIC("not implemented");
        return;
    }

    if (node->is_oneof(ast::NodeKind::WhileStmt)) {
        auto data = conv::while_stmt(*node);

        // @cond
        //     branch %cond, @begin, @after
        // @begin
        //     <body>
        //     jmp @cond
        // @after

        // create a new block just for the condition
        auto cond_block = state.new_empty_block(BlockOp::Jmp);
        state.close_into_pending_block(BlockOp::Jmp, nullptr,
                                       std::array{cond_block});

        state.pending_block = cond_block;
        visit_expr(data.cond, state, ctx);
        auto cond = state.sstack_pop();

        auto body = state.new_empty_block(BlockOp::Jmp);
        auto after = state.new_empty_block(BlockOp::Err);

        // finish the current block using a branch op, either into the body or
        // after it
        state.close_into_pending_block(BlockOp::Branch, cond,
                                       std::array{body, after});

        // set body branch as the current pending block
        state.pending_block = body;

        // fill the body and finish it with a simple jump back to cond
        visit_stmt(data.body, state, ctx);
        if (state.pending_block)
            state.close_into_pending_block(BlockOp::Jmp, nullptr,
                                           std::array{cond_block});

        // set after as the current block
        state.pending_block = after;
        return;
    }

    if (node->is_oneof(ast::NodeKind::ReturnStmt)) {
        auto data = conv::unary(*node);

        if (data.child == nullptr) {
            ctx.for_all_defers(
                [&](Node* stmt) { visit_stmt(stmt, state, ctx); });
            ctx.clear_all_defers();

            state.close_into_pending_block(BlockOp::RetVoid, nullptr, {});
            return;
        }

        visit_expr(data.child, state, ctx);
        auto expr = state.sstack_pop();

        ctx.for_all_defers([&](Node* stmt) { visit_stmt(stmt, state, ctx); });
        ctx.clear_all_defers();

        state.close_into_pending_block(BlockOp::Ret, expr, {});
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
    auto& module = state.module;
    auto  out = module.new_type_span(types.size());
    for (auto [idx, param] : rv::enumerate(types)) {
        out[idx] = create_ir_type_from_general(module, *param);
    }

    return module.new_type_span(out);
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
    auto& module = state.module;
    // auto& ts = *state.ts;
    // auto& er = *state.er;

    auto decl = node->get_decl();
    auto ty = node->get_type()->as_func();

    auto data = conv::func_decl(*node);
    auto params = create_func_params(ty.get_params(), state);
    auto ret = create_func_ret(data.ret, ty.get_ret(), state);

    std::vector<Inst*> params_insts;
    for (auto param : data.get_args().params) {
        auto type = create_ir_type_from_general(module, *param->get_type());
        auto inst = module.new_inst_param(type);
        state.add_local(param->get_decl(), inst);
        params_insts.push_back(inst);
    }

    Block* body = nullptr;
    if (data.body) {
        state.pending_block = state.new_empty_block(BlockOp::Err);

        visit_stmt(data.body, state, ctx);

        if (ty.is_void() && state.blocks.size() == 0) {
            // in case the function returns void and does not have an explicit
            // return, add it
            state.close_into_pending_block(BlockOp::RetVoid, nullptr, {});
        }

        body = state.blocks[0];
    }

    auto flags = Func::FlagNone;
    if (decl->flags.has_export()) {
        flags = static_cast<Func::Flags>(flags | Func::FlagExport);
    }

    if (decl->flags.has_extern()) {
        flags = static_cast<Func::Flags>(flags | Func::FlagExtern);
    }

    module.add_func({
        .link_name = decl->link_name,
        .params = params,
        .param_insts = module.new_inst_span(params_insts),
        .ret = ret,
        .body = body,
        .all_blocks = state.blocks,
        .loc = node->get_loc(),
        .flags = flags,
    });

    state.clear_all_data();
}

// ----------------------------------------------------------------------------

void visit_top(Node* node, State& state, Context& ctx) {
    if (node->is_oneof(ast::NodeKind::FuncDecl,
                       ast::NodeKind::FuncDeclWithCVarArgs)) {
        visit_func_decl(node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        // ignore these
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
