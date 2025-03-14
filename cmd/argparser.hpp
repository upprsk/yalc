#pragma once

#include <string>

namespace yalc {

struct Args {
    std::string program;
    bool        dump_tokens = false;
    bool        dump_ast = false;
    bool        dump_ast_dot = false;
};

auto argparse(int argc, char** argv) -> Args;

}  // namespace yalc
