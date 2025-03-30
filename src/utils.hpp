#pragma once

#include <optional>
#include <string>

namespace yal {

struct ArgIterator {
    int    argc;
    char** argv;

    constexpr auto next(std::string_view& arg) -> bool {
        if (argc == 0) return false;

        argc--;
        arg = std::string_view{*argv++};
        return true;
    }
};

auto read_entire_file(std::string const& path) -> std::optional<std::string>;
auto write_file(std::string const& path, std::string_view contents) -> bool;

}  // namespace yal
