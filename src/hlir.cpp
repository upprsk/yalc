#include "hlir.hpp"

#include <cstddef>

#include "fmt/ranges.h"
#include "fmt/std.h"

namespace yal::hlir {

void Func::disasm(FILE* f, TypeStore const& ts) const {
    using fmt::print;
    using fmt::println;

    println(f, "{}: # {}", name, ts.fatten(type));

    size_t i{};
    for (auto const& blk : blocks) {
        println(f, "b {}:", i);

        size_t j{};
        for (auto const& inst : blk.code) {
            print(f, "{:04} | {}", j, inst.kind);

            switch (inst.kind) {
                case InstKind::Const: {
                    auto c = blk.consts.at(inst.arg);
                    print(f, " {} ({}, {})", inst.arg, c.value,
                          ts.fatten(c.type));
                } break;

                case InstKind::LoadLocal:
                case InstKind::StoreLocal: {
                    auto l = locals.at(inst.arg);
                    print(f, " {} ({:?}, {})", inst.arg, l.name,
                          ts.fatten(l.type));
                } break;

                case InstKind::Branch:
                case InstKind::BranchFalse: {
                    print(f, " b {}", inst.arg);
                } break;

                default: break;
            }

            println(f, "");

            j++;
        }

        i++;
    }
}

}  // namespace yal::hlir

auto fmt::formatter<yal::hlir::Value>::format(yal::hlir::Value n,
                                              format_context&  ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}}}", n.type, n.value);
}

auto fmt::formatter<yal::hlir::InstKind>::format(yal::hlir::InstKind n,
                                                 format_context&     ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yal::hlir::InstKind::Err: name = "Err"; break;
        case yal::hlir::InstKind::Const: name = "Const"; break;
        case yal::hlir::InstKind::LoadLocal: name = "LoadLocal"; break;
        case yal::hlir::InstKind::StoreLocal: name = "StoreLocal"; break;
        case yal::hlir::InstKind::Add: name = "Add"; break;
        case yal::hlir::InstKind::Sub: name = "Sub"; break;
        case yal::hlir::InstKind::Mul: name = "Mul"; break;
        case yal::hlir::InstKind::Div: name = "Div"; break;
        case yal::hlir::InstKind::Eq: name = "Eq"; break;
        case yal::hlir::InstKind::Ret: name = "Ret"; break;
        case yal::hlir::InstKind::BranchFalse: name = "BranchFalse"; break;
        case yal::hlir::InstKind::Branch: name = "Branch"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::hlir::Inst>::format(yal::hlir::Inst n,
                                             format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "({}, {})", n.kind, n.arg);
}

auto fmt::formatter<yal::hlir::Local>::format(yal::hlir::Local n,
                                              format_context&  ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", n.name, n.type, n.idx);
}
