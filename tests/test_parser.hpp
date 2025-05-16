#pragma once

#include <utility>

#include "test_helpers.hpp"

auto test_parser(TestParams const& p) -> std::tuple<int, int, int>;
