#include "test_helpers.hpp"

#include <algorithm>
#include <filesystem>

#include "fmt/color.h"
#include "fmt/format.h"
#include "nlohmann/json.hpp"
#include "utils.hpp"

auto gen_filepath(std::string name) -> std::string {
    std::filesystem::path path = __FILE__;
    std::ranges::replace(name, ' ', '-');

    return path.parent_path().append(fmt::format("{}.test.json", name));
}

auto load_expectation(std::string filename) -> json {
    auto filedata = yal::read_entire_file(filename);
    if (!filedata) return json{};

    return json::parse(*filedata);
}
auto ask_for_updates(std::string_view name) -> bool {
    fmt::print(fmt::fg(fmt::color::yellow), "Wrong expectation found for ");
    fmt::print("{}", name);
    fmt::print(fmt::fg(fmt::color::yellow), ", generate? (Y/n)");

    auto c = getchar();
    return c == '\n' || c == 'y' || c == 'Y';
}
