#pragma once

#include <utility>

struct Params {
    bool ask_for_updates;
};

auto tokenizer_tests(Params const& p) -> std::pair<int, int>;
