#pragma once

struct Args {
    bool verbose;
    bool ask;
    bool diff;
};

[[nodiscard]] auto argparse(int argc, char** argv) -> Args;
