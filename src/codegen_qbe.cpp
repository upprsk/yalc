#include "codegen_qbe.hpp"

#include <cstdint>
#include <string_view>

#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/ranges.h"
#include "hlir.hpp"
#include "types.hpp"

namespace yal::codegen::qbe {

using fmt::print;
using fmt::println;

struct CodegenFunc {
    void codegen(hlir::Func const& func) {
        // in case we have no body, this is an extern function. In QBE there is
        // no need to declare externs.
        if (func.blocks.empty()) return;

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

                        if (ts->get(v.type)->is_integral()) {
                            println(out, "    %tmp_{} = w copy {}", r,
                                    v.value_uint64());
                        }

                        else if (ts->get(v.type)->is_array()) {
                            auto r = push_tmp();
                            auto sz = pending_data.size();
                            pending_data.push_back(v);

                            println(out, "    %tmp_{} = w copy $data_{}", r,
                                    sz);
                        }

                        else {
                            er->report_bug(
                                blk.spans[i],
                                "constants other than integral "
                                "types have not been implemented. Found {}",
                                ts->fatten(v.type));
                            return;  // abort
                        }
                    } break;

                    case hlir::InstKind::Pop: {
                        pop_tmp();
                    } break;

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

                    case hlir::InstKind::Call: {
                        // FIXME: this needs some rework

                        auto r = push_tmp();

                        // we don't want to touch this during the args
                        pop_tmp();

                        auto callee = mod->get(func.calls.at(inst.a));
                        auto ct = ts->get(callee->type)->as_func(*ts);

                        if (!ts->get(ct.ret)->is_void()) {
                            print(out, "    %tmp_{} = {} ", r,
                                  qbe_type_for_primitive(ct.ret));
                        } else
                            print(out, "    ");

                        print(out, "call ${}(", callee->name);

                        std::vector<uint32_t> args;
                        for (auto const& _ : ct.args) {
                            args.push_back(pop_tmp());
                        }

                        for (size_t i{}; auto const& arg : ct.args) {
                            if (i != 0) print(out, ", ");

                            print(out, "{} %tmp_{}",
                                  qbe_type_for_primitive(arg),
                                  args.at(args.size() - i - 1));

                            i++;
                        }

                        push_tmp(r);

                        println(out, ")");
                    } break;

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

        for (size_t i{}; auto const& p : pending_data) {
            if (ts->get(p.type)->is_array()) {
                println(out, "data $data_{} = {{ b {} }}", i,
                        fmt::join(p.value_bytes(), " "));
            } else {
                throw std::runtime_error{fmt::format(
                    "invalid type in pending data: {}", ts->fatten(p.type))};
            }

            i++;
        }
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

    auto push_tmp() -> uint32_t {
        auto r = next_reg++;
        stack.push_back(r);
        return r;
    }

    auto push_tmp(uint32_t reg) -> uint32_t {
        stack.push_back(reg);
        return reg;
    }

    auto pop_tmp() -> uint32_t {
        auto r = stack.end();
        stack.pop_back();

        return *--r;
    }

    // ------------------------------------------------------------------------

    uint32_t              next_reg{};
    std::vector<uint32_t> stack{};  // NOLINT(readability-redundant-member-init)

    // NOLINTNEXTLINE(readability-redundant-member-init)
    std::vector<hlir::Value> pending_data{};

    ErrorReporter*      er;
    TypeStore const*    ts;
    hlir::Module const* mod;
    FILE*               out;
};

void codegen(hlir::Module const& mod, TypeStore const& ts, ErrorReporter& er,
             FILE* f) {
    for (auto const& func : mod.funcs) {
        auto cf = CodegenFunc{.er = &er, .ts = &ts, .mod = &mod, .out = f};
        cf.codegen(func);
    }
}

}  // namespace yal::codegen::qbe
