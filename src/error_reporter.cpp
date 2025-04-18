#include "error_reporter.hpp"

#include <unistd.h>

#include <string_view>

#include "fmt/base.h"
#include "fmt/core.h"

namespace yal {

void ErrorReporterForFile::report(Span s, std::string_view prefix,
                                  fmt::text_style color, fmt::string_view fmt,
                                  fmt::format_args args, uint32_t context) {
    ASSERT(s.begin <= s.end);

    auto [row, col] = find_rowcol(s);

    fmt::print(out, "{}:{}:{}: ", source_path, row, col, prefix);

    auto fd = fileno(out);

    if (isatty(fd)) {
        fmt::print(out, color, "{}", prefix);
        fmt::print(out, ": ");
    } else {
        fmt::print(out, "{}: ", prefix);
    }

    fmt::vprint(out, fmt, args);
    fmt::println(out, "");

    std::string_view source = this->source;
    auto [ls, le] = find_linestart(s);

    // print `context` lines from before the line with the error
    auto ils = ls;
    for (size_t i = 0; i < context && ils > 1; i++) {
        auto [pls, ple] = find_linestart(ils - 2);
        ils = pls;

        fmt::println(out, "  {:04} | {}", row - i - 1,
                     source.substr(pls, ple - pls));
    }

    if (isatty(fd)) {
        fmt::print(out, color, ">");
        fmt::println(out, " {:04} | {}", row, source.substr(ls, le - ls));
    } else {
        fmt::println(out, "> {:04} | {}", row, source.substr(ls, le - ls));
    }

    if (isatty(fd)) {
        fmt::print(out, "{0: <{2}}{1:^<{3}}", "", fmt::styled("", color),
                   2 + 4 + 3 + s.begin - ls, std::min(s.end, le) - s.begin);
    } else {
        fmt::print(out, "{0: <{1}}{0:^<{2}}", "", 2 + 4 + 3 + s.begin - ls,
                   std::min(s.end, le) - s.begin);
    }

    fmt::println(out, "");

    auto ile = le;
    for (size_t i = 0; i < context && ile < source.size(); i++) {
        auto [pls, ple] = find_linestart(ile + 1);
        ile = ple;

        fmt::println(out, "  {:04} | {}", row + i + 1,
                     source.substr(pls, ple - pls));
    }
}
}  // namespace yal
