#include "codegen_qbe.hpp"

#include <cstdint>
#include <list>
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

    for (auto const& inst : block.body) {
        switch (inst->op) {
            case ir::OpCode::IntConst:
                println(out, "    %l{} ={} copy {}", inst->uid,
                        to_qbe_type(*inst->type), inst->get_value_u64());
                break;

            case ir::OpCode::CallVoid:
                print(out, "    call ${}(", inst->get_value_str());
                for (auto arg : inst->get_args()) {
                    print(out, "{} %l{}, ", to_qbe_type(*arg->type), arg->uid);
                }
                println(out, ")");
                break;

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

        default: PANIC("invalid block opcode", block.op);
    }
}

void codegen_func(ir::Func const& fn, State& state, Context& ctx) {
    auto out = state.out;

    if (fn.is_extern()) return;

    if (fn.is_export()) {
        print(out, "export ");
    }

    if (fn.ret != nullptr) {
        print(out, "function {} ${}", to_qbe_type(*fn.ret), fn.link_name);
    } else {
        print(out, "function ${}", fn.link_name);
    }

    if (fn.params.size() > 0) {
        PANIC("arguments not implemented", fn.link_name);
    }

    println(out, "() {{");
    println(out, "@start");

    std::unordered_set<ir::Block*> visited;
    std::list<ir::Block*>          pending{fn.body};

    while (!pending.empty()) {
        auto blk = pending.front();
        pending.pop_front();

        if (visited.contains(blk)) continue;
        visited.insert(blk);

        codegen_block(*blk, state, ctx);
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
}

}  // namespace yal::codegen::qbe
