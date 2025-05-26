#include "codegen_qbe.hpp"

#include <cstdint>
#include <list>
#include <ranges>
#include <string_view>

#include "fmt/ranges.h"
#include "ir.hpp"

namespace yal::codegen::qbe {
using fmt::print;
using fmt::println;

namespace rv = std::ranges::views;

struct State {
    FILE*             out;
    types::TypeStore* ts;
    ErrorReporter*    er;
    Options const*    opt;
};

struct Context {};

// ----------------------------------------------------------------------------

auto machine_ptr_type() -> std::string_view {
    return sizeof(uintptr_t) == 4 ? "w" : "l";
}

auto machine_load_ptr_op() -> std::string_view {
    return sizeof(uintptr_t) == 4 ? "loaduw" : "loadl";
}

auto machine_load_sptr_op() -> std::string_view {
    return sizeof(uintptr_t) == 4 ? "loadsw" : "loadl";
}

auto machine_store_ptr_op() -> std::string_view {
    return sizeof(uintptr_t) == 4 ? "storew" : "storel";
}

auto to_qbe_type(ir::Type const& type) -> std::string_view {
    switch (type.kind) {
        case ir::TypeKind::Uint64:
        case ir::TypeKind::Int64: return "l";
        case ir::TypeKind::Uint32:
        case ir::TypeKind::Int32: return "w";
        case ir::TypeKind::Uint16:
        case ir::TypeKind::Int16: return "h";
        case ir::TypeKind::Uint8:
        case ir::TypeKind::Int8: return "b";
        case ir::TypeKind::Usize:
        case ir::TypeKind::Isize:
        case ir::TypeKind::Ptr:
        case ir::TypeKind::StrView: return machine_ptr_type();
        case ir::TypeKind::Float32: return "f";
        case ir::TypeKind::Float64: return "d";
        default: PANIC("invalid type kind", type.kind);
    }
}

auto to_qbe_temp(ir::Type const& type) -> std::string_view {
    switch (type.kind) {
        case ir::TypeKind::Uint64:
        case ir::TypeKind::Int64: return "l";
        case ir::TypeKind::Uint32:
        case ir::TypeKind::Int32:
        case ir::TypeKind::Uint16:
        case ir::TypeKind::Int16:
        case ir::TypeKind::Uint8:
        case ir::TypeKind::Int8: return "w";
        case ir::TypeKind::Usize:
        case ir::TypeKind::Isize:
        case ir::TypeKind::Ptr:
        case ir::TypeKind::Array:
        case ir::TypeKind::Struct:
        case ir::TypeKind::Slice:
        case ir::TypeKind::StrView: return machine_ptr_type();
        case ir::TypeKind::Float32: return "f";
        case ir::TypeKind::Float64: return "d";
        default: PANIC("invalid type kind", type.kind);
    }
}

auto to_qbe_fntype(ir::Type const& type) -> std::string_view {
    switch (type.kind) {
        case ir::TypeKind::Uint64:
        case ir::TypeKind::Int64: return "l";
        case ir::TypeKind::Uint32:
        case ir::TypeKind::Int32: return "w";
        case ir::TypeKind::Uint16: return "uh";
        case ir::TypeKind::Int16: return "sh";
        case ir::TypeKind::Uint8: return "ub";
        case ir::TypeKind::Int8: return "sb";
        case ir::TypeKind::Usize:
        case ir::TypeKind::Isize:
        case ir::TypeKind::Ptr: return machine_ptr_type();
        case ir::TypeKind::Float32: return "f";
        case ir::TypeKind::Float64: return "d";
        default: PANIC("invalid type kind", type.kind);
    }
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void codegen_block(ir::Block const& block, State& state, Context& ctx) {
    (void)ctx;
    auto out = state.out;

    println(out, "@b{}", block.uid);

    for (auto const& inst : block.body) {
        switch (inst->op) {
            case ir::OpCode::IntConst:
                println(out, "    %l{} ={} copy {}", inst->uid,
                        to_qbe_temp(*inst->type), inst->get_value_u64());
                break;

            case ir::OpCode::StrConst:
                println(out, "    %l{} ={} copy $str_{}", inst->uid,
                        to_qbe_temp(*inst->type), inst->get_value_u64());
                break;

            case ir::OpCode::Call:
                print(out, "    %l{} ={} call ${}(", inst->uid,
                      to_qbe_temp(*inst->type), inst->get_value_str());
                for (auto arg : inst->get_args()) {
                    print(out, "{} %l{}, ", to_qbe_type(*arg->type), arg->uid);
                }
                println(out, ")");
                break;

            case ir::OpCode::CallVoid:
                print(out, "    call ${}(", inst->get_value_str());
                for (auto arg : inst->get_args()) {
                    print(out, "{} %l{}, ", to_qbe_temp(*arg->type), arg->uid);
                }
                println(out, ")");
                break;

            case ir::OpCode::SetTmp:
                println(out, "    %l{} ={} copy %l{}", inst->get_arg(0)->uid,
                        to_qbe_temp(*inst->get_arg(0)->type),
                        inst->get_arg(1)->uid);
                break;

            case ir::OpCode::Ext: {
                std::string_view is_signed;

                if (inst->get_arg(0)->type->is_signed())
                    is_signed = "s";
                else
                    is_signed = "u";

                println(out, "    %l{} ={} ext{}{} %l{}", inst->uid,
                        to_qbe_temp(*inst->type), is_signed,
                        to_qbe_type(*inst->get_arg(0)->type),
                        inst->get_arg(0)->uid);
            } break;

            case ir::OpCode::Copy:
                println(out, "    %l{} ={} copy %l{}", inst->uid,
                        to_qbe_temp(*inst->type), inst->get_arg(0)->uid);
                break;

            case ir::OpCode::Alloca: {
                auto align = inst->value_2;
                println(out, "    %l{} ={} alloc{} {}", inst->uid,
                        to_qbe_temp(*inst->type), align, inst->get_value_u64());
            } break;

            case ir::OpCode::Load: {
                std::string_view op;
                switch (inst->type->kind) {
                    case ir::TypeKind::Uint64:
                    case ir::TypeKind::Int64: op = "loadl"; break;
                    case ir::TypeKind::Uint32: op = "loaduw"; break;
                    case ir::TypeKind::Int32: op = "loadsw"; break;
                    case ir::TypeKind::Uint16: op = "loaduh"; break;
                    case ir::TypeKind::Int16: op = "loadsh"; break;
                    case ir::TypeKind::Uint8: op = "loadub"; break;
                    case ir::TypeKind::Int8: op = "loadsb"; break;
                    case ir::TypeKind::Float32: op = "loads"; break;
                    case ir::TypeKind::Float64: op = "loadd"; break;
                    case ir::TypeKind::Usize: op = machine_load_ptr_op(); break;
                    case ir::TypeKind::Isize:
                        op = machine_load_sptr_op();
                        break;
                    case ir::TypeKind::Ptr: op = machine_load_ptr_op(); break;
                    default: PANIC("bad type kind in Load", inst->type->kind);
                }

                println(out, "    %l{} ={} {} %l{}", inst->uid,
                        to_qbe_temp(*inst->type), op, inst->get_arg(0)->uid);
            } break;

            case ir::OpCode::Store: {
                std::string_view op;

                // use rhs type as the type to store
                switch (inst->get_arg(1)->type->kind) {
                    case ir::TypeKind::Uint64:
                    case ir::TypeKind::Int64: op = "storel"; break;
                    case ir::TypeKind::Uint32:
                    case ir::TypeKind::Int32: op = "storew"; break;
                    case ir::TypeKind::Uint16:
                    case ir::TypeKind::Int16: op = "storeh"; break;
                    case ir::TypeKind::Uint8:
                    case ir::TypeKind::Int8: op = "storeb"; break;
                    case ir::TypeKind::Usize:
                    case ir::TypeKind::Isize:
                        op = machine_store_ptr_op();
                        break;
                    case ir::TypeKind::Float32: op = "stores"; break;
                    case ir::TypeKind::Float64: op = "stored"; break;
                    case ir::TypeKind::Ptr: op = machine_store_ptr_op(); break;
                    default:
                        PANIC("bad type kind in Store",
                              fmt::to_string(inst->get_arg(1)->type->kind));
                }

                auto offset = inst->get_value_u64();
                auto rhs = inst->get_arg(0)->uid;
                if (offset > 0) {
                    println(out, "    %l{} ={} add %l{}, {}", inst->uid,
                            to_qbe_temp(*inst->get_arg(0)->type),
                            inst->get_arg(0)->uid, offset);
                    rhs = inst->uid;
                }

                println(out, "    {} %l{}, %l{}", op, inst->get_arg(1)->uid,
                        rhs);
            } break;

            case ir::OpCode::Add:
            case ir::OpCode::Sub:
            case ir::OpCode::Div:
            case ir::OpCode::Mul: {
                std::string_view op;
                switch (inst->op) {
                    case ir::OpCode::Add: op = "add"; break;
                    case ir::OpCode::Sub: op = "sub"; break;
                    case ir::OpCode::Div: op = "div"; break;
                    case ir::OpCode::Mul: op = "mul"; break;
                    default: UNREACHABLE("invalid op for arith", inst->op);
                }

                println(out, "    %l{} ={} {} %l{}, %l{}", inst->uid,
                        to_qbe_temp(*inst->type), op, inst->get_arg(0)->uid,
                        inst->get_arg(1)->uid);
            } break;

            case ir::OpCode::Eq:
            case ir::OpCode::Neq:
            case ir::OpCode::Lt:
            case ir::OpCode::Le: {
                std::string_view op;
                switch (inst->op) {
                    case ir::OpCode::Eq: op = "eq"; break;
                    case ir::OpCode::Neq: op = "ne"; break;
                    case ir::OpCode::Lt:
                        if (inst->type->is_signed())
                            op = "slt";
                        else
                            op = "ult";
                        break;
                    case ir::OpCode::Le:
                        if (inst->type->is_signed())
                            op = "sle";
                        else
                            op = "ule";
                        break;

                    default: UNREACHABLE("invalid op for comparison", inst->op);
                }

                println(out, "    %l{} ={} c{}{} %l{}, %l{}", inst->uid,
                        to_qbe_temp(*inst->type), op,
                        to_qbe_temp(*inst->get_arg(0)->type),
                        inst->get_arg(0)->uid, inst->get_arg(1)->uid);

            } break;

            default: PANIC("invalid instruction opcode", inst->op);
        }
    }

    switch (block.op) {
        case ir::BlockOp::Ret:
            ASSERT(block.value != nullptr);
            ASSERT(block.next.empty());
            println(out, "    ret %l{}", block.value->uid);
            break;
        case ir::BlockOp::RetVoid:
            ASSERT(block.value == nullptr);
            ASSERT(block.next.empty());
            println(out, "    ret");
            break;

        case ir::BlockOp::Jmp:
            ASSERT(block.value == nullptr);
            ASSERT(block.next.size() == 1);
            println(out, "    jmp @b{}", block.next[0]->uid);
            break;
        case ir::BlockOp::Branch:
            ASSERT(block.value != nullptr);
            ASSERT(block.next.size() == 2);
            println(out, "    jnz %l{}, @b{}, @b{}", block.value->uid,
                    block.next[0]->uid, block.next[1]->uid);
            break;

        default: PANIC("invalid block opcode", block.op, block.uid);
    }
}

void codegen_func(ir::Func const& fn, State& state, Context& ctx) {
    auto out = state.out;

    if (fn.is_extern()) return;

    if (fn.is_export()) {
        print(out, "export ");
    }

    if (fn.ret != nullptr) {
        print(out, "function {} ${}", to_qbe_fntype(*fn.ret), fn.link_name);
    } else {
        print(out, "function ${}", fn.link_name);
    }

    print(out, "(");

    for (auto [inst, ty] : rv::zip(fn.param_insts, fn.params)) {
        print(out, "{} %l{}, ", to_qbe_fntype(*ty), inst->uid);
    }

    println(out, ") {{");
    println(out, "@start");

    std::unordered_set<ir::Block*> visited;
    std::list<ir::Block*>          pending{fn.body};

    while (!pending.empty()) {
        auto blk = pending.front();
        pending.pop_front();

        if (visited.contains(blk)) continue;
        visited.insert(blk);

        codegen_block(*blk, state, ctx);
        pending.insert(pending.end(), blk->next.begin(), blk->next.end());
    }

    println(out, "}}");
}

void codegen(FILE* out, ir::Module const& module, ErrorReporter& er,
             types::TypeStore& ts, Options const& opt) {
    auto state = State{.out = out, .ts = &ts, .er = &er, .opt = &opt};
    auto ctx = Context{};

    for (auto const& fn : module.get_funcs()) {
        codegen_func(fn, state, ctx);
    }

    for (auto const& [id, str] : rv::enumerate(module.get_strings())) {
        println(out, "data $str_{} = {{ b {:?}, b 0 }}", id, str);
    }
}

}  // namespace yal::codegen::qbe
