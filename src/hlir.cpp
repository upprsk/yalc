#include "hlir.hpp"

#include <cstddef>
#include <ranges>

#include "fmt/ranges.h"
#include "fmt/std.h"

namespace yal::hlir {

void Func::disasm(FILE* f, TypeStore const& ts) const {
    using fmt::print;
    using fmt::println;

    println(f, "{}: # {}", name, ts.fatten(type));

    print(f, "[");
    for (size_t i{}; auto const& l : locals) {
        if (i != 0) print(f, ", ");
        print(f, "({}, {:?}, {})", l.idx, l.name, ts.fatten(l.type));

        i++;
    }
    println(f, "]");

    size_t i{};
    for (auto const& blk : blocks) {
        println(f, "b {}:", i);

        size_t j{};
        for (auto const& inst : blk.code) {
            print(f, "{:04} | {}", j, inst.kind);

            switch (inst.kind) {
                case InstKind::Const: {
                    auto c = blk.consts.at(inst.a);
                    print(f, " {} ({}, {})", inst.a, c.value,
                          ts.fatten(c.type));
                } break;

                case InstKind::LoadLocal:
                case InstKind::StoreLocal: {
                    auto l = locals.at(inst.a);
                    print(f, " {} ({:?}, {})", inst.a, l.name,
                          ts.fatten(l.type));
                } break;

                case InstKind::Jump: {
                    print(f, " j {}", inst.a);
                } break;

                case InstKind::Branch: {
                    print(f, " b {}, {}", inst.a, inst.b);
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
        case yal::hlir::InstKind::Neq: name = "Neq"; break;
        case yal::hlir::InstKind::Ret: name = "Ret"; break;
        case yal::hlir::InstKind::Jump: name = "Jump"; break;
        case yal::hlir::InstKind::Branch: name = "Branch"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::hlir::Inst>::format(yal::hlir::Inst n,
                                             format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "({}, {})", n.kind, n.a);
}

auto fmt::formatter<yal::hlir::Local>::format(yal::hlir::Local n,
                                              format_context&  ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", n.name, n.type, n.idx);
}
