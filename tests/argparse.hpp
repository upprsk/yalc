#pragma once

struct Args {
    int  verbose{};
    bool ask{};
    bool diff{};
};

[[nodiscard]] auto argparse(int argc, char** argv) -> Args;
