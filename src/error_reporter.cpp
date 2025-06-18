#include "error_reporter.hpp"

#include <cstdio>
#include <libassert/assert.hpp>
#include <string_view>

namespace yal {

[[nodiscard]] constexpr auto find_rowcol(std::string_view source, Span s)
    -> std::pair<uint32_t, uint32_t> {
    uint32_t col{};
    uint32_t row{};

    auto count = std::min(s.begin, static_cast<uint32_t>(source.size()));
    for (size_t i = 0; i < count; i++, col++) {
        if (source.at(i) == '\n') {
            row++;
            col = 0;
        }
    }

    // need to adjust rows to be one based
    return {row + 1, col};
}

[[nodiscard]] constexpr auto find_linestart(std::string_view source,
                                            uint32_t         begin) -> Span {
    uint32_t line_start{};
    uint32_t line_end = source.length();

    for (ssize_t i = begin; i >= 0; i--) {
        if (i < static_cast<ssize_t>(source.length()) && source[i] == '\n') {
            line_start = i + 1;
            break;
        }
    }

    for (size_t i = begin; i < source.length(); i++) {
        if (source[i] == '\n') {
            line_end = i;
            break;
        }
    }

    // the line is made of a single '\n'
    if (line_end == line_start - 1) {
        line_end = line_start;
    }

    return {.begin = line_start, .end = line_end};
}

[[nodiscard]] constexpr auto find_linestart(std::string_view source, Span s)
    -> Span {
    return find_linestart(source, s.begin);
}

void LocalErrorReporter::vreport_error(Span s, fmt::string_view fmt,
                                       fmt::format_args args) const {
    parent->error_count++;
    report(s, "error", error_style, fmt, args);
}

void LocalErrorReporter::vreport_warn(Span s, fmt::string_view fmt,
                                      fmt::format_args args) const {
    report(s, "warn", warn_style, fmt, args);
}

void LocalErrorReporter::vreport_note(Span s, fmt::string_view fmt,
                                      fmt::format_args args) const {
    report(s, "note", note_style, fmt, args);
}

void LocalErrorReporter::vreport_debug(Span s, fmt::string_view fmt,
                                       fmt::format_args args) const {
    report(s, "debug", debug_style, fmt, args);
}

void LocalErrorReporter::vreport_bug(Span s, fmt::string_view fmt,
                                     fmt::format_args args) const {
    parent->error_count++;
    report(s, "bug", bug_style, fmt, args);
}

void LocalErrorReporter::report(Span s, std::string_view prefix,
                                fmt::text_style color, fmt::string_view fmt,
                                fmt::format_args args, uint32_t context) const {
    ASSERT(s.begin <= s.end);

    auto             file = get_file();
    std::string_view source = file.contents;
    auto [row, col] = find_rowcol(source, s);

    fmt::print(out, "{}:{}:{}: ", file.original_path, row, col, prefix);

    auto fd = fileno(out);

    if (isatty(fd)) {
        fmt::print(out, color, "{}", prefix);
        fmt::print(out, ": ");
    } else {
        fmt::print(out, "{}: ", prefix);
    }

    fmt::vprint(out, fmt, args);
    fmt::println(out, "");

    auto [ls, le] = find_linestart(source, s);

    // FIXME: currently, context walks the lins backward, so this only works for
    // when context=1

    // print `context` lines from before the line with the error
    auto ils = ls;
    for (size_t i = 0; i < context && ils > 1; i++) {
        auto [pls, ple] = find_linestart(source, ils - 2);
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
        auto [pls, ple] = find_linestart(source, ile + 1);
        ile = ple;

        fmt::println(out, "  {:04} | {}", row + i + 1,
                     source.substr(pls, ple - pls));
    }
}

void ErrorReporter::vreport_error(Location const& s, fmt::string_view fmt,
                                  fmt::format_args args) {
    for_file(s).vreport_error(s.span, fmt, args);
}

void ErrorReporter::vreport_warn(Location const& s, fmt::string_view fmt,
                                 fmt::format_args args) {
    for_file(s).vreport_warn(s.span, fmt, args);
}

void ErrorReporter::vreport_note(Location const& s, fmt::string_view fmt,
                                 fmt::format_args args) {
    for_file(s).vreport_note(s.span, fmt, args);
}

void ErrorReporter::vreport_debug(Location const& s, fmt::string_view fmt,
                                  fmt::format_args args) {
    for_file(s).vreport_debug(s.span, fmt, args);
}

void ErrorReporter::vreport_bug(Location const& s, fmt::string_view fmt,
                                fmt::format_args args) {
    for_file(s).vreport_bug(s.span, fmt, args);
}
}  // namespace yal
