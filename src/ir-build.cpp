#include "ir-build.hpp"

#include <algorithm>
#include <cstdint>
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

        module.reset_inst_uid_counter();
        module.reset_block_uid_counter();
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

    // NOLINTBEGIN(readability-redundant-member-init)
    Module module{};

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
        case types::TypeKind::Uint64: return mod.get_type_uint64();
        case types::TypeKind::Int64: return mod.get_type_int64();
        case types::TypeKind::Uint32: return mod.get_type_uint32();
        case types::TypeKind::Int32: return mod.get_type_int32();
        case types::TypeKind::Uint16: return mod.get_type_uint16();
        case types::TypeKind::Int16: return mod.get_type_int16();
        case types::TypeKind::Uint8: return mod.get_type_uint8();
        case types::TypeKind::Int8: return mod.get_type_int8();
        case types::TypeKind::Usize: return mod.get_type_usize();
        case types::TypeKind::Isize: return mod.get_type_isize();
        case types::TypeKind::Bool: return mod.get_type_uint8();
        case types::TypeKind::Float32: return mod.get_type_float32();
        case types::TypeKind::Float64: return mod.get_type_float64();

        case types::TypeKind::Ptr:
        case types::TypeKind::PtrConst:
        case types::TypeKind::MultiPtr:
        case types::TypeKind::MultiPtrConst: return mod.get_type_ptr();

        case types::TypeKind::Pack:
            if (ty.inner.size() == 1)
                return create_ir_type_from_general(mod, *ty.inner[0]);
            PANIC("packs not implemented");

        case types::TypeKind::Distinct:
            return create_ir_type_from_general(mod, *ty.inner[0]);

        case types::TypeKind::Struct: return mod.new_type_of(TypeKind::Struct);

        default: PANIC("invalid type", ty.kind);
    }
}

// ============================================================================

void build_expr(Node* node, State& state, Context& ctx);

// source expression value comes from the stack (a POP)
void build_cast(types::Type* target, types::Type* source, State& state) {
    auto& module = state.module;

    target = target->unpacked();
    source = source->unpacked();

    if (*target == *source) return;
    auto arg = state.sstack_pop();

    if (target->is_integral()) {
        if (source->is_integral()) {
            if (target->size() > source->size()) {
                auto type = create_ir_type_from_general(module, *target);
                auto inst = module.new_inst_ext(type, arg);
                state.add_and_push_inst(inst);
                return;
            }

            // TODO: when target is smaller
        }

        // NOTE: just converting directly, which is not always correct...
        // (although, if we allowed the cast in sema, then it should be usize of
        // isize)
        if (source->is_ptr() || source->is_mptr()) {
            state.sstack_push(arg);
            return;
        }

        // TODO: when source is not an integer
    }

    else if (target->is_ptr() || target->is_mptr()) {
        // if both sides are pointers, at the IR level there is not
        // difference, push the inst back in the stack
        if (source->is_ptr() || source->is_mptr()) {
            state.sstack_push(arg);
            return;
        }

        // NOTE: just converting directly, which is not always correct...
        // (although, if we allowed the cast in sema, then it should be usize of
        // isize)
        if (source->is_integral()) {
            state.sstack_push(arg);
            return;
        }
    }

    // TODO: when target is not an integer or pointer

    PANIC("not implemented", *target, *source);
}

void build_expr_lvalue(Node* node, State& state, Context& /*unused*/) {
    if (node->is_oneof(ast::NodeKind::Id)) {
        auto local = state.get_local(node->get_decl());
        state.sstack_push(local);
    }

    else {
        UNREACHABLE("invalid l-value", node->get_kind());
    }
}

auto build_index_ptr(Node* node, State& state, Context& ctx)
    -> std::pair<Inst*, Type*> {
    auto& module = state.module;

    auto data = conv::index(*node);
    build_expr(data.receiver, state, ctx);
    build_expr(data.index, state, ctx);

    auto type = create_ir_type_from_general(module, *node->get_type());

    auto index = state.sstack_pop();
    auto receiver = state.sstack_pop();

    // in case the index type is smaller then the pointer size, we need to
    // extend it
    if (index->type->size() < module.get_type_isize()->size()) {
        index = module.new_inst_ext(module.get_type_isize(), index);
        state.add_inst(index);
    }

    uint64_t sizeof_receiver;
    if (receiver->type->is_strview()) {
        sizeof_receiver = module.get_type_uint8()->size();
        receiver = module.new_inst_load(module.get_type_ptr(), receiver);

        state.add_inst(receiver);
    } else if (receiver->type->is_slice()) {
        auto receiver_type = data.receiver->get_type();
        ASSERT(receiver_type->is_slice());

        sizeof_receiver =
            create_ir_type_from_general(module, *receiver_type->inner[0])
                ->size();
        receiver = module.new_inst_load(module.get_type_ptr(), receiver);

        state.add_inst(receiver);
    } else if (receiver->type->is_array()) {
        auto receiver_type = data.receiver->get_type();
        ASSERT(receiver_type->is_array());

        sizeof_receiver = receiver_type->inner[0]->size();
    } else {
        sizeof_receiver = receiver->type->size();
    }

    // receiver + (index * sizeof(*receiver))
    auto inst_sizeof_receiver =
        module.new_inst_int_const(index->type, sizeof_receiver);
    auto inst_index_mult = module.new_inst_arith(OpCode::Mul, index->type,
                                                 index, inst_sizeof_receiver);
    auto ptr = module.new_inst_arith(OpCode::Add, receiver->type, receiver,
                                     inst_index_mult);

    state.add_inst(inst_sizeof_receiver);
    state.add_inst(inst_index_mult);
    state.add_inst(ptr);

    return {ptr, type};
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void build_expr(Node* node, State& state, Context& ctx) {
    auto& module = state.module;

    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);
        if (data.items.size() > 1) {
            state.er->report_bug(node->get_loc(),
                                 "return of multiple values not implemented");
            PANIC("not implemented");
        }

        build_expr(data.items[0], state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        auto type = create_ir_type_from_general(module, *node->get_type());
        auto inst = module.new_inst_int_const(type, node->get_data_u64());
        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Str)) {
        auto data = conv::str(*node);

        // struct str_t { char* ptr; size_t len; };
        // struct str_t s;
        // s.ptr = <str>;
        // s.len = <sizeof(str)>;

        auto ptr_type = module.get_type_ptr();
        auto usize_type = module.get_type_usize();

        // struct str_t s;
        auto ptr =
            module.new_inst_alloca(module.new_type_of(TypeKind::StrView),
                                   ptr_type->alignment(), ptr_type->size() * 2);
        state.add_inst(ptr);

        // s.ptr = <str>;
        auto str_data_ptr = module.new_inst_str_const(ptr_type, data.value);
        auto ptr_store = module.new_inst_store(ptr, str_data_ptr);
        state.add_inst(str_data_ptr);
        state.add_inst(ptr_store);

        // s.len = <sizeof(str)>;
        auto size_offset =
            module.new_inst_int_const(usize_type, ptr_type->size());
        auto size_ptr =
            module.new_inst_arith(OpCode::Add, usize_type, ptr, size_offset);
        auto size = module.new_inst_int_const(usize_type, data.value.size());
        auto size_store = module.new_inst_store(size_ptr, size);
        state.add_inst(size_offset);
        state.add_inst(size_ptr);
        state.add_inst(size);
        state.add_inst(size_store);

        state.sstack_push(ptr);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Field)) {
        auto data = conv::field(*node);

        build_expr(data.receiver, state, ctx);
        auto receiver = state.sstack_pop();
        auto receiver_type = data.receiver->get_type()->undistinct();

        // automatic dereference of pointers to structs!
        if (receiver_type->is_ptr()) {
            receiver_type = receiver_type->inner[0]->undistinct();
        }

        if (receiver_type->is_struct()) {
            auto field = receiver_type->as_struct_get_fields().at(data.name);

            auto offset_value = module.new_inst_int_const(
                module.get_type_usize(), field.offset);
            auto offset_ptr = module.new_inst_arith(
                OpCode::Add, module.get_type_ptr(), receiver, offset_value);

            auto type = create_ir_type_from_general(module, *field.type);
            auto load = module.new_inst_load(type, offset_ptr);

            state.add_inst(offset_value);
            state.add_inst(offset_ptr);
            state.add_and_push_inst(load);
        }

        else if (receiver_type->is_strview() || receiver_type->is_slice()) {
            if (data.name == "ptr") {
                auto load =
                    module.new_inst_load(module.get_type_ptr(), receiver);
                state.add_and_push_inst(load);
            }

            else if (data.name == "len") {
                auto offset_value = module.new_inst_int_const(
                    module.get_type_usize(), module.get_type_ptr()->size());
                auto offset_ptr = module.new_inst_arith(
                    OpCode::Add, module.get_type_ptr(), receiver, offset_value);
                auto load =
                    module.new_inst_load(module.get_type_usize(), offset_ptr);
                state.add_inst(offset_value);
                state.add_inst(offset_ptr);
                state.add_and_push_inst(load);
            }

            else
                PANIC("invalid field for string view", data.name);
        }

        else if (receiver_type->is_array()) {
            if (data.name == "ptr") {
                // no conversion needed, as we decay to a pointer in the IR
                state.sstack_push(receiver);
            }

            else if (data.name == "len") {
                auto len_value = module.new_inst_int_const(
                    module.get_type_usize(), receiver_type->count);
                state.add_and_push_inst(len_value);
            } else
                PANIC("invalid field for array", data.name);
        }

        else
            PANIC("invalid receiver type for field", *receiver_type);

        return;
    }

    if (node->is_oneof(ast::NodeKind::Array)) {
        auto data = conv::array(*node);
        auto node_type = node->get_type();

        auto type = module.new_type_of(TypeKind::Array);
        auto ptr = module.new_inst_alloca(type, node_type->alignment(),
                                          node_type->size());

        state.add_inst(ptr);

        for (auto [idx, it] : rv::enumerate(data.items)) {
            build_expr(it, state, ctx);
            auto v = state.sstack_pop();

            auto offset_value = module.new_inst_int_const(
                module.get_type_usize(), idx * it->get_type()->size());
            auto offset = module.new_inst_arith(OpCode::Add, offset_value->type,
                                                ptr, offset_value);
            auto store = module.new_inst_store(offset, v);

            state.add_inst(offset_value);
            state.add_inst(offset);
            state.add_inst(store);
        }

        state.sstack_push(ptr);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Lit)) {
        auto data = conv::lit(*node);
        auto node_type = node->get_type();

        if (node_type->is_struct()) {
            auto fields = node_type->as_struct_get_fields();
            auto ptr = module.new_inst_alloca(
                module.new_type_of(TypeKind::Struct), node_type->alignment(),
                node_type->size());
            state.add_and_push_inst(ptr);

            for (auto node : data.items) {
                auto data = conv::lit_param(*node);
                build_expr(data.init, state, ctx);

                auto init = state.sstack_pop();

                auto field = fields.at(data.key);
                auto offset_value = module.new_inst_int_const(
                    module.get_type_usize(), field.offset);
                auto offset_ptr = module.new_inst_arith(
                    OpCode::Add, module.get_type_ptr(), ptr, offset_value);
                auto store = module.new_inst_store(offset_ptr, init);

                state.add_inst(offset_value);
                state.add_inst(offset_ptr);
                state.add_inst(store);
            }
        }

        else
            PANIC("invalid type for lit", node_type->kind);

        return;
    }

    if (node->is_oneof(ast::NodeKind::AddrOf)) {
        auto data = conv::unary(*node);

        if (data.child->is_oneof(ast::NodeKind::Id)) {
            auto d = data.child->get_decl();
            auto local = state.get_local(d);

            // stack variable
            if (d->is_stack_var()) {
                ASSERT(local->type->is_ptr());

                state.sstack_push(local);
            }

            // some stack-native type
            else if (d->get_type()->undistinct()->is_struct()) {
                ASSERT(local->type->is_struct());

                state.sstack_push(local);
            }

            else {
                // something else
                PANIC(
                    "not implemented. AddrOf not implemented for type of decl",
                    *d);
            }
            return;
        }

        PANIC("not implemented. AddrOf not implemented for",
              data.child->get_kind());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Deref)) {
        auto data = conv::unary(*node);
        build_expr(data.child, state, ctx);

        auto ptr = state.sstack_pop();
        ASSERT(ptr->type->is_ptr());

        auto type = create_ir_type_from_general(module, *node->get_type());
        auto inst = module.new_inst_load(type, ptr);
        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Index)) {
        auto [ptr, type] = build_index_ptr(node, state, ctx);

        auto inst = module.new_inst_load(type, ptr);
        state.add_and_push_inst(inst);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Slicing)) {
        auto data = conv::slicing(*node);

        build_expr(data.receiver, state, ctx);

        auto inner = data.receiver->get_type()->unpacked()->undistinct();
        if (inner->is_array()) {
            if (data.start || data.end) {
                PANIC("slicing: not implemented start/end (array)");
            }

            auto receiver = state.sstack_pop();

            // struct slice_t { T* ptr; size_t len; };
            // struct slice_t s;
            // s.ptr = <arr>.ptr;
            // s.len = <arr>.len;

            auto ptr_type = module.get_type_ptr();
            auto usize_type = module.get_type_usize();

            // struct slice_t s;
            auto ptr = module.new_inst_alloca(
                module.new_type_of(TypeKind::Slice), ptr_type->alignment(),
                ptr_type->size() * 2);
            state.add_inst(ptr);

            // s.ptr = <arr>.ptr;
            auto slice_data_ptr = module.new_inst_copy(ptr_type, receiver);
            auto ptr_store = module.new_inst_store(ptr, slice_data_ptr);
            state.add_inst(slice_data_ptr);
            state.add_inst(ptr_store);

            // s.len = <arr>.len;
            auto size_offset =
                module.new_inst_int_const(usize_type, ptr_type->size());
            auto size_ptr = module.new_inst_arith(OpCode::Add, usize_type, ptr,
                                                  size_offset);
            auto size = module.new_inst_int_const(usize_type, inner->count);
            auto size_store = module.new_inst_store(size_ptr, size);
            state.add_inst(size_offset);
            state.add_inst(size_ptr);
            state.add_inst(size);
            state.add_inst(size_store);

            state.sstack_push(ptr);
        } else if (inner->is_mptr()) {
            PANIC("slicing: not implemented (mptr)");
        } else if (inner->is_strview()) {
            PANIC("slicing: not implemented (string_view)");
        } else if (inner->is_slice()) {
            PANIC("slicing: not implemented (slice)");
        } else {
            PANIC("invalid receiver for slicing", *inner);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto d = node->get_decl();
        auto local = state.get_local(node->get_decl());

        if (d->is_stack_var()) {
            ASSERT(local->type->is_ptr());

            auto type = create_ir_type_from_general(module, *node->get_type());
            auto inst = module.new_inst_load(type, local);
            state.add_and_push_inst(inst);
        } else {
            state.sstack_push(local);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::CallDirect)) {
        auto data = conv::call_direct(*node);
        auto ty = node->get_type();

        for (auto arg : data.args) build_expr(arg, state, ctx);

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
        build_expr(data.lhs, state, ctx);
        build_expr(data.rhs, state, ctx);
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
        build_expr(data.child, state, ctx);

        build_cast(data.target, data.child->get_type(), state);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Cast)) {
        auto data = conv::binary(*node);
        build_expr(data.lhs, state, ctx);

        build_cast(node->get_type(), data.lhs->get_type(), state);
        return;
    }

    state.er->report_bug(node->get_loc(), "expression not implemented");
    PANIC("not implemented", node->get_kind());
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void build_assign_direct(Node* node, State& state, Context& ctx) {
    auto& module = state.module;
    auto  data = conv::assign(*node);
    auto  assign_lhs = data.lhs;

    if (assign_lhs->is_oneof(ast::NodeKind::Id)) {
        auto lhs = state.get_local(conv::id(*assign_lhs).to);
        auto rhs = state.sstack_pop();

        auto d = assign_lhs->get_decl();

        // target is a stack variable, need to use a store
        if (d->is_stack_var()) {
            auto inst = module.new_inst_store(lhs, rhs);
            state.add_inst(inst);
        }

        // target is in a temporary, just set it
        else {
            auto inst = module.new_inst_settmp(lhs, rhs);
            state.add_inst(inst);
        }
    }

    else if (assign_lhs->is_oneof(ast::NodeKind::Deref)) {
        build_expr_lvalue(conv::unary(*assign_lhs).child, state, ctx);

        auto lhs = state.sstack_pop();
        auto rhs = state.sstack_pop();
        ASSERT(lhs->type->is_ptr(), *lhs->type);

        auto inst = module.new_inst_store(lhs, rhs);
        state.add_inst(inst);
    }

    else if (assign_lhs->is_oneof(ast::NodeKind::Index)) {
        auto [ptr, type] = build_index_ptr(assign_lhs, state, ctx);

        auto rhs = state.sstack_pop();
        ASSERT(*rhs->type == *type);

        auto inst = module.new_inst_store(ptr, rhs);
        state.add_inst(inst);
    }

    else if (assign_lhs->is_oneof(ast::NodeKind::Field)) {
        auto data = conv::field(*assign_lhs);

        build_expr(data.receiver, state, ctx);
        auto receiver = state.sstack_pop();
        auto receiver_type = data.receiver->get_type()->undistinct();

        auto rhs = state.sstack_pop();

        // automatic dereference of pointers to structs!
        if (receiver_type->is_ptr()) {
            receiver_type = receiver_type->inner[0]->undistinct();
        }

        if (receiver_type->is_struct()) {
            auto field = receiver_type->as_struct_get_fields().at(data.name);

            auto offset_value = module.new_inst_int_const(
                module.get_type_usize(), field.offset);
            auto offset_ptr = module.new_inst_arith(
                OpCode::Add, module.get_type_ptr(), receiver, offset_value);
            auto store = module.new_inst_store(offset_ptr, rhs);

            state.add_inst(offset_value);
            state.add_inst(offset_ptr);
            state.add_inst(store);
        }

        else
            PANIC("invalid receiver type for field", *receiver_type);
    }

    else {
        UNREACHABLE("invalid lhs in AssignDirect", assign_lhs->get_kind());
    }
}

void build_unscoped_assign(Node* node, State& state, Context& ctx) {
    auto data = conv::unscoped_assign(*node);

    // handle the rhs of all assignments
    for (auto item : data.items) {
        if (item->is_oneof(ast::NodeKind::AssignDirect)) {
            auto data = conv::assign(*item);
            build_expr(data.rhs, state, ctx);
        }

        else {
            PANIC("not implemented (unscoped assign)", item->get_kind());
        }
    }

    // process the lhs of all assignments in reverse (because the results are in
    // the shadow stack)
    for (auto item : rv::reverse(data.items)) {
        if (item->is_oneof(ast::NodeKind::AssignDirect)) {
            build_assign_direct(item, state, ctx);
        }

        else {
            PANIC("not implemented (unscoped assign)", item->get_kind());
        }
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void build_stmt(Node* node, State& state, Context& ctx) {
    auto& module = state.module;

    // auto& ts = *state.ts;
    // auto& er = *state.er;

    if (node->is_oneof(ast::NodeKind::Block)) {
        auto data = conv::block(*node);
        auto initial = state.shadow_stack.size();

        std::vector<Node*> defers;
        auto               sctx = ctx.with_defers(defers);

        for (auto item : data.items) build_stmt(item, state, sctx);

        for (auto d : rv::reverse(defers)) {
            build_stmt(d, state, sctx);
        }

        auto ending = state.shadow_stack.size();
        ASSERT(initial == ending);
        return;
    }

    if (node->is_oneof(ast::NodeKind::UnscopedGroup)) {
        auto data = conv::unscoped_group(*node);
        for (auto item : data.items) build_stmt(item, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::UnscopedAssign)) {
        build_unscoped_assign(node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeclLocalVarDirect)) {
        auto data = conv::decl_local_var_direct(*node);

        build_expr(data.init, state, ctx);

        auto init = state.sstack_pop();
        auto d = node->get_decl();

        // this variable must be stored in the stack
        if (d->is_stack_var()) {
            auto dt = d->get_type();
            auto align = dt->alignment();
            auto size = dt->size();
            auto type = create_ir_type_from_general(module, *dt);
            ASSERT(*init->type == *type);

            auto alloc =
                module.new_inst_alloca(module.get_type_ptr(), align, size);
            auto store = module.new_inst_store(alloc, init);

            state.add_inst(alloc);
            state.add_inst(store);

            state.add_local(d, alloc);
        }

        // this can just use a temporary
        else {
            auto inst = module.new_inst_copy(init->type, init);
            state.add_inst(inst);

            state.add_local(d, inst);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::DeclLocalVarDirectPack)) {
        auto data = conv::decl_local_var_direct_pack(*node);

        build_expr(data.init, state, ctx);

        if (data.names.size() == 1) {
            auto init = state.sstack_pop();
            state.add_local(data.names[0]->get_decl(), init);
            return;
        }

        PANIC("var decl with multiple returns not implemented");
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
            build_expr(data.cond, state, ctx);
            auto cond = state.sstack_pop();

            auto wt = state.new_empty_block(BlockOp::Jmp);
            auto after = state.new_empty_block(BlockOp::Err);

            // finish the current block using a branch op
            state.close_into_pending_block(BlockOp::Branch, cond,
                                           std::array{wt, after});

            // set then branch as the current pending block
            state.pending_block = wt;

            // fill the then branch and finish it with a simple jump
            build_stmt(data.wt, state, ctx);
            if (state.pending_block)
                state.close_into_pending_block(BlockOp::Jmp, nullptr,
                                               std::array{after});

            // set after as the current block
            state.pending_block = after;
            return;
        }

        build_expr(data.cond, state, ctx);
        auto cond = state.sstack_pop();

        auto wt = state.new_empty_block(BlockOp::Jmp);
        auto wf = state.new_empty_block(BlockOp::Jmp);
        auto after = state.new_empty_block(BlockOp::Err);

        // finish the current block using the branch op
        state.close_into_pending_block(BlockOp::Branch, cond,
                                       std::array{wt, wf});

        // set the then branch as the current pending block
        state.pending_block = wt;

        // fill the then branch and finish it with a simple jump to after
        build_stmt(data.wt, state, ctx);
        if (state.pending_block)
            state.close_into_pending_block(BlockOp::Jmp, nullptr,
                                           std::array{after});

        // set the else branch as the current pending block
        state.pending_block = wf;

        // fill the else branch and finish it with a simple jump to after
        build_stmt(data.wf, state, ctx);
        if (state.pending_block)
            state.close_into_pending_block(BlockOp::Jmp, nullptr,
                                           std::array{after});

        // set after as the current block
        state.pending_block = after;
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
        build_expr(data.cond, state, ctx);
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
        build_stmt(data.body, state, ctx);
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
                [&](Node* stmt) { build_stmt(stmt, state, ctx); });
            ctx.clear_all_defers();

            state.close_into_pending_block(BlockOp::RetVoid, nullptr, {});
            return;
        }

        build_expr(data.child, state, ctx);
        auto expr = state.sstack_pop();

        ctx.for_all_defers([&](Node* stmt) { build_stmt(stmt, state, ctx); });
        ctx.clear_all_defers();

        state.close_into_pending_block(BlockOp::Ret, expr, {});
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprStmt)) {
        auto data = conv::unary(*node);
        build_expr(data.child, state, ctx);
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

void build_func_decl(Node* node, State& state, Context& ctx) {
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

        build_stmt(data.body, state, ctx);

        if (ty.is_void() && state.blocks.size() == 0) {
            // in case the function returns void and does not have an explicit
            // return, add it
            state.close_into_pending_block(BlockOp::RetVoid, nullptr, {});
        }

        // should not be empty!!
        // FIXME: this should produce an error message or something. Empty block
        // means that we did not have a return in a function that has a single
        // block as body.
        if (state.blocks.empty()) {
            state.close_into_pending_block(BlockOp::Err, nullptr, {});
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

void build_top(Node* node, State& state, Context& ctx) {
    if (node->is_oneof(ast::NodeKind::FuncDecl,
                       ast::NodeKind::FuncDeclWithCVarArgs)) {
        build_func_decl(node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        // ignore these
        return;
    }

    PANIC("invalid top node", node->get_kind());
}

// ----------------------------------------------------------------------------

void build_flat_module(Node* node, State& state, Context& ctx) {
    auto data = conv::flat_module(*node);
    for (auto top : data.children) {
        build_top(top, state, ctx);
    }
}

// ----------------------------------------------------------------------------

auto build(ast::Ast& ast, ast::Node* root, ErrorReporter& er,
           types::TypeStore& ts, Options const& opt) -> Module {
    (void)ast;

    auto state = State{.ts = &ts, .er = &er, .opt = &opt};
    auto ctx = Context{};
    build_flat_module(root, state, ctx);

    auto m = std::move(state.module);
    return m;
}

}  // namespace yal::ir
