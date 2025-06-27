#pragma once

#include <fmt/base.h>

#include <string>

#include "error_reporter.hpp"
#include "fmt/format.h"
#include "macros.hpp"

namespace yalc {

struct DumpStep {
    enum Step {
        None = 0,
        Tokens = 1 << 0,
        Ast = 1 << 1,
        TopNameRes = 1 << 2,
        Attributes = 1 << 3,
        NameRes = 1 << 4,
        Sema = 1 << 5,
        Ir = 1 << 6,
        IrLower = 1 << 7,
    };

#define define_with(_name, _enum_case)                            \
    [[nodiscard]] constexpr auto with_##_name() const->DumpStep { \
        return {static_cast<Step>(value | _enum_case)};           \
    }

#define define_has(_name, _enum_case)                        \
    [[nodiscard]] constexpr auto has_##_name() const->bool { \
        return (value & _enum_case) != 0;                    \
    }

#define define_parts(_name, _enum_case) \
    define_with(_name, _enum_case) define_has(_name, _enum_case)

    define_parts(tokens, Tokens);
    define_parts(ast, Ast);
    define_parts(top_name_res, TopNameRes);
    define_parts(attributes, Attributes);
    define_parts(name_res, NameRes);
    define_parts(sema, Sema);
    define_parts(ir, Ir);
    define_parts(ir_lower, IrLower);

#undef define_with
#undef define_has
#undef define_parts

    [[nodiscard]] constexpr auto has(Step step) const -> bool {
        return (value & step) != 0;
    }

    Step value{None};
};

struct Args {
    std::string program;
    std::string output;

    bool single_file = false;
    bool verbose = false;
    bool verbose_parser = false;
    bool just_analyse = false;
    bool dump_tokens = false;
    bool dump_parsed_ast = false;
    bool dump_named_ast = false;

    DumpStep dump{};

    yal::ErrorReporterFormat error_format = yal::ErrorReporterFormat::Pretty;
};

[[nodiscard]] auto argparse(int argc, char** argv) -> Args;

// ============================================================================

auto format_as(DumpStep::Step step) -> std::string_view;

}  // namespace yalc

// ============================================================================

define_formatter_from_string_view(yalc::DumpStep);
define_formatter_from_string_view(yalc::Args);
