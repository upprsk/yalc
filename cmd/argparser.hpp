#pragma once

#include <string>

namespace yalc {

struct Args {
    std::string program;
    std::string output;

    bool single_file = false;

    bool dump_tokens = false;
    bool dump_ast = false;
    bool dump_ast_json = false;
    bool dump_individual_ast_json = false;
    bool dump_ast_dot = false;
    bool dump_type_store_json = false;
    bool dump_ir_module = false;
};

auto argparse(int argc, char** argv) -> Args;

}  // namespace yalc
