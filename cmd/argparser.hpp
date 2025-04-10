#pragma once

#include <string>

namespace yalc {

struct Args {
    std::string program;

    bool single_file = false;

    bool dump_tokens = false;
    bool dump_ast = false;
    bool dump_ast_json = false;
    bool dump_individual_ast_json = false;
    bool dump_ast_dot = false;
    bool dump_type_store_json = false;
};

auto argparse(int argc, char** argv) -> Args;

}  // namespace yalc
