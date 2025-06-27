#include "argparser.hpp"

#include <fmt/base.h>
#include <fmt/ranges.h>

#include <cstdio>
#include <libassert/assert.hpp>
#include <ranges>
#include <string_view>

#include "error_reporter.hpp"
#include "utils.hpp"
#include "yal.hpp"

namespace rv = std::ranges::views;

namespace yalc {
using fmt::println;

void print_usage(std::string_view self) {
    println(stderr, "usage: {} [options] <program>", self);
}

void print_version() { println(stderr, "yalc: {}", yal::get_version()); }

void print_help(std::string_view self) {
    print_usage(self);

    // clang-format off
    println(stderr, "");
    println(stderr, "options:");
    println(stderr, "    -h,--help: show this message and exit.");
    println(stderr, "    --usage: show usage and exit.");
    println(stderr, "    --version: print compiler version.");
    println(stderr, "    --verbose <steps>: show more output (on stderr) for each step. This option");
    println(stderr, "        accepts a list of steps separetd by commas: step1,step2. Thi option can");
    println(stderr, "        also be passed multiple times: --verbose step1 --verbose step2");
    println(stderr, "        available steps:");
    println(stderr, "            exe: make the compiler plumming itself more verbose.");
    println(stderr, "            parser: make the parser verbose.");
    println(stderr, "            sort: make top-level AST sorting and name resolution verbose.");
    println(stderr, "    --verbose-parser: show details of parsing (on stderr).");
    println(stderr, "    --verbose-name-res: show details of name resolution (on stderr).");
    println(stderr, "    --file: single file compilation mode.");
    println(stderr, "    --dump <step>: dump the result of an internal compilation step. This option");
    println(stderr, "        accepts a list of steps separated by a comma: step1,step2. The option");
    println(stderr, "        can also be passed multiple times: --dump step1 --dump step2");
    println(stderr, "        available steps:");
    println(stderr, "            tokens: dump tokenization result.");
    println(stderr, "            ast: dump parsed AST.");
    println(stderr, "            sorted: dump top-level name-resolved and sorted AST.");
    println(stderr, "            attributes: dump AST after attributes are applied.");
    println(stderr, "            named: dump AST full name resolutio.");
    println(stderr, "            sema: dump the AST of each function after semantic-analysis.");
    println(stderr, "            ir: dump the intermediate representation.");
    println(stderr, "            ir-lower: dump the intermediate representation after lowering.");
    println(stderr, "            deps-mermaid: dump the results of top-level name-resolution as a");
    println(stderr, "                mermaid diagram.");
    println(stderr, "    -o,--output: path to output file (QBE)");
    println(stderr, "    --dump-parsed-ast: dump the output of parsing as json");
    println(stderr, "    --dump-named-ast: dump the output of name resolution as json");
    println(stderr, "    --just-analyse: do not compile, just check the source");
    println(stderr, "    --error-format <format>: Change how errors are formatted.");
    println(stderr, "        available formats:");
    println(stderr, "            pretty: show the error message and context information in a readable way (default).");
    println(stderr, "            json: all data in the report is given in a json format.");
    // clang-format on
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto argparse(int argc, char** argv) -> Args {
    auto it = yal::ArgIterator{.argc = argc, .argv = argv};

    std::string_view self;
    DEBUG_ASSERT(it.next(self), "no argv[0]");

    Args args;

    std::string_view arg;
    while (it.next(arg)) {
        if (arg == "-h" || arg == "--help") {
            print_help(self);
            std::exit(0);
        }

        if (arg == "--usage") {
            print_usage(self);
            std::exit(0);
        }

        if (arg == "--version") {
            print_version();
            std::exit(0);
        }

        if (arg == "--verbose") {
            if (!it.next(arg)) {
                println(stderr, "error: missing argument for --dump.");
                std::exit(1);
            }

            for (auto it : rv::split(arg, ',')) {
                std::string_view part{it};

                if (part == "exe") {
                    args.verbose = args.verbose.with_exe();
                } else if (part == "parser") {
                    args.verbose = args.verbose.with_parser();
                } else if (part == "sort") {
                    args.verbose = args.verbose.with_sort();
                } else {
                    println(stderr, "error: unknown verbose step: {:?}", part);
                }
            }
        }

        else if (arg == "--file") {
            args.single_file = true;
        }

        else if (arg == "--dump") {
            if (!it.next(arg)) {
                println(stderr, "error: missing argument for --dump.");
                std::exit(1);
            }

            for (auto it : rv::split(arg, ',')) {
                std::string_view part{it};

                if (part == "tokens") {
                    args.dump = args.dump.with_tokens();
                } else if (part == "ast") {
                    args.dump = args.dump.with_ast();
                } else if (part == "sorted") {
                    args.dump = args.dump.with_sorted();
                } else if (part == "attributes") {
                    args.dump = args.dump.with_attributes();
                } else if (part == "named") {
                    args.dump = args.dump.with_named();
                } else if (part == "sema") {
                    args.dump = args.dump.with_sema();
                } else if (part == "ir") {
                    args.dump = args.dump.with_ir();
                } else if (part == "ir-lower") {
                    args.dump = args.dump.with_ir_lower();
                } else if (part == "deps-debug") {
                    args.dump = args.dump.with_deps_debug();
                } else if (part == "deps-mermaid") {
                    args.dump = args.dump.with_deps_mermaid();
                } else {
                    println(stderr, "error: unknown dump step: {:?}", part);
                }
            }
        }

        else if (arg == "-o" || arg == "--output") {
            if (!args.output.empty()) {
                println(stderr, "error: output already provided");
                std::exit(1);
            }

            if (!it.next(arg)) {
                println(stderr, "error: missing output file path");
                std::exit(1);
            }

            args.output = arg;
        }

        else if (arg == "--error-format") {
            if (!it.next(arg)) {
                println(stderr, "error: missing argument for --error-format.");
                std::exit(1);
            }

            if (arg == "pretty") {
                args.error_format = yal::ErrorReporterFormat::Pretty;
            } else if (arg == "json") {
                args.error_format = yal::ErrorReporterFormat::Json;
            } else {
                println(stderr,
                        "error: invalid argument for --error-format: {}", arg);
                std::exit(1);
            }
        }

        else if (args.program.empty()) {
            args.program = arg;
        }

        else {
            println(stderr, "error: unknown option: {:?}", arg);
            std::exit(1);
        }
    }

    if (args.program.empty()) {
        println(stderr, "error: missing required argument: program");
        std::exit(1);
    }

    return args;
}

auto format_as(DumpStep::Step step) -> std::string_view {
    std::string_view s;

    switch (step) {
        case DumpStep::None: s = "none"; break;
        case DumpStep::Tokens: s = "tokens"; break;
        case DumpStep::Ast: s = "ast"; break;
        case DumpStep::Sorted: s = "sorted"; break;
        case DumpStep::Attributes: s = "attributes"; break;
        case DumpStep::Named: s = "named"; break;
        case DumpStep::Sema: s = "sema"; break;
        case DumpStep::Ir: s = "ir"; break;
        case DumpStep::IrLower: s = "ir-lower"; break;
        case DumpStep::DepsMermaid: s = "deps-mermaid"; break;
        case DumpStep::DepsDebug: s = "deps-debug"; break;
    }

    return s;
}

auto format_as(VerboseStep::Step step) -> std::string_view {
    std::string_view name;

    switch (step) {
        case VerboseStep::None: name = "none"; break;
        case VerboseStep::Exe: name = "exe"; break;
        case VerboseStep::Parser: name = "parser"; break;
        case VerboseStep::Sort: name = "sort"; break;
    }

    return name;
}

}  // namespace yalc

// ============================================================================

auto fmt::formatter<yalc::DumpStep>::format(yalc::DumpStep const& step,
                                            format_context&       ctx) const
    -> format_context::iterator {
    std::array steps{
        yalc::DumpStep::Tokens,      yalc::DumpStep::Ast,
        yalc::DumpStep::Sorted,      yalc::DumpStep::Attributes,
        yalc::DumpStep::Named,       yalc::DumpStep::Sema,
        yalc::DumpStep::Ir,          yalc::DumpStep::IrLower,
        yalc::DumpStep::DepsMermaid, yalc::DumpStep::DepsDebug,
    };

    auto it =
        rv::filter(steps, [&](yalc::DumpStep::Step s) { return step.has(s); });
    return fmt::format_to(ctx.out(), "{}", fmt::join(it, ","));
}

auto fmt::formatter<yalc::VerboseStep>::format(yalc::VerboseStep const& step,
                                               format_context& ctx) const
    -> format_context::iterator {
    std::array steps{
        yalc::VerboseStep::None,
        yalc::VerboseStep::Exe,
        yalc::VerboseStep::Parser,
        yalc::VerboseStep::Sort,
    };

    auto it = rv::filter(
        steps, [&](yalc::VerboseStep::Step s) { return step.has(s); });
    return fmt::format_to(ctx.out(), "{}", fmt::join(it, ","));
}

auto fmt::formatter<yalc::Args>::format(yalc ::Args const& p,
                                        format_context&    ctx) const
    -> format_context ::iterator {
    return fmt::format_to(ctx.out(),
                          "Args{{program={:?}, output={:?}, single_file={}, "
                          "just_analyze={}, dump={}, verbose={}}}",
                          p.program, p.output, p.single_file, p.just_analyse,
                          p.dump, p.verbose);
}
