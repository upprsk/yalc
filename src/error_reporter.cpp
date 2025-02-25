#include "error_reporter.hpp"

#include <unistd.h>

#include <string_view>

#include "fmt/base.h"
#include "fmt/core.h"

namespace yal {

void ErrorReporter::report(Span s, std::string_view prefix,
                           fmt::text_style color, fmt::string_view fmt,
                           fmt::format_args args) {
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
    fmt::println(out, "{:04} | {}", row, source.substr(ls, le - ls));

    if (isatty(fd)) {
        fmt::print(out, color, "{0: <{1}}{0:^<{2}}", "", 4 + 3 + s.begin - ls,
                   std::min(s.end, le) - s.begin);
    } else {
        fmt::print(out, "       {:<{}}", "^", le - ls);
    }

    fmt::println(out, "");
}
}  // namespace yal
