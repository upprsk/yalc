#include "codegen_qbe.hpp"

#include <cstdint>
#include <string_view>

#include "error_reporter.hpp"
#include "fmt/base.h"
#include "types.hpp"

namespace yal::codegen::qbe {

using fmt::println;

struct CodegenFunc {
    void codegen(hlir::Func const& func) {
        println(out, "# {}", ts->fatten(func.type));

        auto ft = ts->get(func.type)->as_func(*ts);
        if (!ts->get(ft.ret)->is_integral()) {
            er->report_bug(
                func.blocks.at(0).spans[0],
                "non-integral return types not implemented. Found {}",
                ts->fatten(ft.ret));
            return;
        }

        // TODO: function arguments
        println(out, "export function {} ${}() {{",
                qbe_type_for_primitive(ft.ret), func.name);
        println(out, "@start");

        for (size_t blk_idx{}; auto const& blk : func.blocks) {
            println(out, "@blk_{}", blk_idx);

            for (size_t i{}; auto const& inst : blk.code) {
                switch (inst.kind) {
                    case hlir::InstKind::Err:
                        er->report_bug(blk.spans[i],
                                       "found Err opcode in codegen");
                        return;  // abort this function

                    case hlir::InstKind::Const: {
                        auto        r = push_tmp();
                        auto const& v = blk.consts.at(inst.a);

                        if (!ts->get(v.type)->is_integral()) {
                            er->report_bug(
                                blk.spans[i],
                                "constants other than integral "
                                "types have not been implemented. Found {}",
                                ts->fatten(v.type));
                            return;  // abort
                        }

                        println(out, "    %tmp_{} = w copy {}", r,
                                v.value_uint64());
                    } break;

                    case hlir::InstKind::Pop:
                    case hlir::InstKind::LoadLocal:
                    case hlir::InstKind::StoreLocal:
                    case hlir::InstKind::Add:
                    case hlir::InstKind::Sub:
                    case hlir::InstKind::Mul:
                    case hlir::InstKind::Div:
                    case hlir::InstKind::Eq:
                    case hlir::InstKind::Neq:
                    case hlir::InstKind::Lt:
                    case hlir::InstKind::Lte:
                    case hlir::InstKind::Gt:
                    case hlir::InstKind::Gte:
                    case hlir::InstKind::LogicNot:
                    case hlir::InstKind::Iext:
                    case hlir::InstKind::Uext:
                    case hlir::InstKind::Trunc:
                        er->report_bug(blk.spans[i], "not implemented: {}",
                                       inst.kind);
                        break;

                    case hlir::InstKind::Ret: {
                        auto r = pop_tmp();
                        println(out, "    ret %tmp_{}", r);
                    } break;

                    case hlir::InstKind::Call:
                    case hlir::InstKind::Jump:
                    case hlir::InstKind::Branch:
                        er->report_bug(blk.spans[i], "not implemented: {}",
                                       inst.kind);
                        break;
                }

                i++;
            }

            blk_idx++;
        }

        println(out, "}} # end of {}", func.name);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto qbe_type_for_primitive(TypeHandle ty) const
        -> std::string_view {
        std::string_view ret_type = "w";

        switch (ts->get(ty)->size(*ts)) {
            case 1: ret_type = "b"; break;
            case 2: ret_type = "h"; break;
            case 4: ret_type = "w"; break;
            case 8: ret_type = "l"; break;
            default: throw std::runtime_error{"invalid size for return type"};
        }

        return ret_type;
    }

    // ------------------------------------------------------------------------

    auto push_tmp() -> uint32_t { return tmp_top++; }
    auto pop_tmp() -> uint32_t { return --tmp_top; }

    // ------------------------------------------------------------------------

    uint32_t tmp_top{};

    ErrorReporter*   er;
    TypeStore const* ts;
    FILE*            out;
};

void codegen(hlir::Module const& mod, TypeStore const& ts, ErrorReporter& er,
             FILE* f) {
    for (auto const& func : mod.funcs) {
        auto cf = CodegenFunc{.er = &er, .ts = &ts, .out = f};
        cf.codegen(func);
    }
}

}  // namespace yal::codegen::qbe
