#include "ir.hpp"

#include <list>
#include <ranges>
#include <string_view>
#include <variant>

#include "file-store.hpp"
#include "fmt/base.h"
#include "fmt/color.h"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "fmt/std.h"

namespace yal::ir {

namespace rv = std::ranges::views;

void disasm_inst(FILE* out, Inst const& inst) {
    if (inst.type) {
        fmt::print(out, "    %{} = {}<{}> ", inst.uid, inst.op, *inst.type);
    } else {
        fmt::print(out, "    {} ", inst.op);
        if (inst.type) fmt::print(out, "<{}>", *inst.type);
    }

    if (inst.has_u64()) {
        if (inst.op == OpCode::Store || inst.op == OpCode::Load)
            fmt::print(out, "+{}, ", inst.get_value_u64());
        else
            fmt::print(out, "{}", inst.get_value_u64());
    } else if (inst.has_str()) {
        fmt::print(out, "{}", inst.get_value_str());
    }

    if (inst.op == OpCode::Alloca) {
        fmt::print(out, "/{}", inst.value_2);
    }

    if (!inst.get_args().empty()) {
        if (inst.op == OpCode::Call || inst.op == OpCode::CallVoid)
            fmt::print(out, "(");

        for (auto [idx, arg] : rv::enumerate(inst.get_args())) {
            if (idx) fmt::print(out, ", ");
            fmt::print(out, "%{}", arg->uid);
        }

        if (inst.op == OpCode::Call || inst.op == OpCode::CallVoid)
            fmt::print(out, ")");
    }

    fmt::println(out, "");
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void disasm_block(FILE* out, Block const& blk) {
    fmt::println(out, "@{}", blk.uid);
    for (auto inst : blk.body) {
        disasm_inst(out, *inst);
    }

    switch (blk.op) {
        case BlockOp::Ret:
            ASSERT(blk.value != nullptr);
            ASSERT(blk.next.empty());
            fmt::println(out, "    ret %{}", blk.value->uid);
            break;
        case BlockOp::RetVoid:
            ASSERT(blk.value == nullptr);
            ASSERT(blk.next.empty());
            fmt::println(out, "    ret");
            break;
        case BlockOp::Jmp:
            ASSERT(blk.value == nullptr);
            ASSERT(blk.next.size() == 1);
            fmt::println(out, "    jmp @{}",
                         blk.next.empty() ? 420 : blk.next[0]->uid);
            break;
        case BlockOp::Branch:
            ASSERT(blk.value != nullptr);
            ASSERT(blk.next.size() == 2);
            fmt::println(out, "    branch %{}, @{} @{}", blk.value->uid,
                         blk.next[0]->uid, blk.next[1]->uid);
            break;
        case BlockOp::Err: fmt::println(out, "    <ERROR>"); break;
    }
}

void disasm_func(FILE* out, Func const& fn) {
    if (fn.is_export()) fmt::print(out, "export ");
    if (fn.is_extern()) fmt::print(out, "extern ");

    fmt::print(out, "func {:?}(", fn.link_name);
    for (auto [idx, p, pi] : rv::zip(rv::iota(0), fn.params, fn.param_insts)) {
        if (idx) fmt::print(out, ", ");
        fmt::print(out, "%{}: {}", pi->uid, *p);
    }
    fmt::print(out, ")");
    if (fn.ret) {
        fmt::print(out, " {}", *fn.ret);
    }

    if (fn.body != nullptr) {
        fmt::println(out, " {{");

        std::unordered_set<Block*> visited;
        std::list<Block*>          pending{fn.body};

        while (!pending.empty()) {
            auto blk = pending.front();
            pending.pop_front();

            if (visited.contains(blk)) continue;
            visited.insert(blk);

            disasm_block(out, *blk);
            pending.insert(pending.end(), blk->next.begin(), blk->next.end());
        }

        fmt::println(out, "}}");
    } else {
        fmt::println(out, ";");
    }
}

void disasm_module(FILE* out, Module const& mod) {
    for (auto const& fn : mod.get_funcs()) {
        disasm_func(out, fn);
    }

    for (auto [idx, str] : rv::enumerate(mod.get_strings())) {
        fmt::println(out, "data $str_{} = {:?}", idx, str);
    }
}

}  // namespace yal::ir

auto fmt::formatter<yal::ir::Type>::format(yal::ir::Type const& n,
                                           format_context&      ctx) const
    -> format_context::iterator {
    std::string_view name;
    switch (n.kind) {
        case yal::ir::TypeKind::Err: name = "Err"; break;
        case yal::ir::TypeKind::Uint64: name = "u64"; break;
        case yal::ir::TypeKind::Int64: name = "i64"; break;
        case yal::ir::TypeKind::Uint32: name = "u32"; break;
        case yal::ir::TypeKind::Int32: name = "i32"; break;
        case yal::ir::TypeKind::Uint16: name = "u16"; break;
        case yal::ir::TypeKind::Int16: name = "i16"; break;
        case yal::ir::TypeKind::Uint8: name = "u8"; break;
        case yal::ir::TypeKind::Int8: name = "i8"; break;
        case yal::ir::TypeKind::Usize: name = "usize"; break;
        case yal::ir::TypeKind::Isize: name = "isize"; break;
        case yal::ir::TypeKind::Float32: name = "f32"; break;
        case yal::ir::TypeKind::Float64: name = "f64"; break;
        case yal::ir::TypeKind::Ptr: name = "ptr"; break;
        case yal::ir::TypeKind::Slice: name = "Slice"; break;
        case yal::ir::TypeKind::StrView: name = "string_view"; break;
        case yal::ir::TypeKind::Array: name = "Array"; break;
        case yal::ir::TypeKind::Struct: name = "struct"; break;
    }

    return fmt::format_to(ctx.out(), "{}", name);
}

auto fmt::formatter<yal::ir::Inst>::format(yal::ir::Inst const& inst,
                                           format_context&      ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "Inst({}, {}, {}", inst.op, inst.uid, *inst.type);

    if (inst.has_value()) {
        fmt::format_to(ctx.out(), ", {}", inst.value);
    }

    if (std::holds_alternative<yal::ir::Inst*>(inst.args)) {
        auto arg = inst.get_arg();
        if (arg) fmt::format_to(ctx.out(), ", {}", *arg);
    } else {
        auto args = inst.get_args();
        fmt::format_to(
            ctx.out(), ", {}",
            args | std::ranges::views::transform(
                       [](yal::ir::Inst* inst) -> yal::ir::Inst const& {
                           return *inst;
                       }));
    }

    return fmt::format_to(ctx.out(), ")");
}

auto fmt::formatter<yal::ir::Block>::format(yal::ir::Block const& block,
                                            format_context&       ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "Block({}", block.op);

    if (block.value) {
        fmt::format_to(ctx.out(), ", {}", *block.value);
    }

    return fmt::format_to(
        ctx.out(), ", {}, {})",
        block.body | std::ranges::views::transform(
                         [](yal::ir::Inst* inst) -> yal::ir::Inst const& {
                             return *inst;
                         }),
        block.next | std::ranges::views::transform(
                         [](yal::ir::Block* blk) { return fmt::ptr(blk); }));
}

auto fmt::formatter<yal::ir::Func>::format(yal::ir::Func const& fn,
                                           format_context&      ctx) const
    -> format_context::iterator {
    fmt::format_to(
        ctx.out(), "Func({:?}, {}", fn.link_name,
        fn.params | std::ranges::views::transform(
                        [](yal::ir::Type* type) -> yal::ir::Type const& {
                            return *type;
                        }));

    if (fn.ret) {
        fmt::format_to(ctx.out(), ", {}", *fn.ret);
    }

    return fmt::format_to(
        ctx.out(), ", {})",
        fn.all_blocks | std::ranges::views::transform(
                            [](yal::ir::Block* block) -> yal::ir::Block const& {
                                return *block;
                            }));
}
