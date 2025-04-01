#pragma once

#include <unistd.h>

#include <cstdint>
#include <cstdio>
#include <string_view>
#include <utility>

#include "file-store.hpp"
#include "fmt/color.h"
#include "fmt/core.h"
#include "span.hpp"

namespace yal {

class ErrorReporterForFile {
    static constexpr auto const error_style = fmt::fg(fmt::color::red);
    static constexpr auto const warn_style = fmt::fg(fmt::color::yellow);
    static constexpr auto const note_style = fmt::fg(fmt::color::cyan);
    static constexpr auto const debug_style =
        fmt::fg(fmt::color::medium_purple);
    static constexpr auto const bug_style =
        fmt::fg(fmt::color::crimson) | fmt::emphasis::bold;

public:
    constexpr ErrorReporterForFile(FileId fileid, std::string source,
                                   std::string source_path, FILE* out)
        : source{std::move(source)},
          source_path{std::move(source_path)},
          out{out},
          fileid{fileid} {}

    template <typename... T>
    void report_error(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_error(s, fmt, fmt::make_format_args(args...));
    }

    void vreport_error(Span s, fmt::string_view fmt, fmt::format_args args) {
        error_count++;
        report(s, "error", error_style, fmt, args);
    }

    template <typename... T>
    void report_warn(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_warn(s, fmt, fmt::make_format_args(args...));
    }

    void vreport_warn(Span s, fmt::string_view fmt, fmt::format_args args) {
        report(s, "warn", warn_style, fmt, args);
    }

    template <typename... T>
    void report_note(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_note(s, fmt, fmt::make_format_args(args...));
    }

    void vreport_note(Span s, fmt::string_view fmt, fmt::format_args args) {
        report(s, "note", note_style, fmt, args);
    }

    template <typename... T>
    void report_debug(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_debug(s, fmt, fmt::make_format_args(args...));
    }

    void vreport_debug(Span s, fmt::string_view fmt, fmt::format_args args) {
        report(s, "debug", debug_style, fmt, args);
    }

    template <typename... T>
    void report_bug(Span s, fmt::format_string<T...> fmt, T&&... args) {
        error_count++;
        report(s, "BUG", bug_style, fmt, fmt::make_format_args(args...));
    }

    void report(Span s, std::string_view prefix, fmt::text_style color,
                fmt::string_view fmt, fmt::format_args args,
                uint32_t context = 1);

    // -----------------------------------------------------------------------

    [[nodiscard]] constexpr auto had_error() const -> bool {
        return error_count > 0;
    }

    [[nodiscard]] constexpr auto get_source() const -> std::string const& {
        return source;
    }

    [[nodiscard]] constexpr auto get_source_path() const -> std::string const& {
        return source_path;
    }

    [[nodiscard]] constexpr auto get_error_count() const -> uint32_t {
        return error_count;
    }

    [[nodiscard]] constexpr auto get_fileid() const -> FileId { return fileid; }

    // -----------------------------------------------------------------------

private:
    [[nodiscard]] constexpr auto find_rowcol(Span s) const
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

    [[nodiscard]] constexpr auto find_linestart(uint32_t begin) const -> Span {
        uint32_t line_start{};
        uint32_t line_end = source.length();

        for (ssize_t i = begin; i >= 0; i--) {
            if (source[i] == '\n') {
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

    [[nodiscard]] constexpr auto find_linestart(Span s) const -> Span {
        return find_linestart(s.begin);
    }

    // -----------------------------------------------------------------------

private:
    std::string source;
    std::string source_path;
    FILE*       out;
    FileId      fileid;
    uint32_t    error_count{};
};

class ErrorReporter {
public:
    constexpr ErrorReporter(FileStore& fs, FILE* out) : fs{&fs}, out{out} {}

    [[nodiscard]] auto for_file(FileId id) const -> ErrorReporterForFile {
        return ErrorReporterForFile{
            id,
            fs->get_contents(id),
            fs->get_filename(id),
            out,
        };
    }

    [[nodiscard]] constexpr auto had_error() const -> bool {
        return error_count > 0;
    }

    [[nodiscard]] constexpr auto get_error_count() const -> uint32_t {
        return error_count;
    }

    constexpr void update_error_count(uint32_t cnt) { error_count += cnt; }
    constexpr void update_error_count(ErrorReporterForFile const& er) {
        update_error_count(er.get_error_count());
    }

private:
    FileStore* fs;
    FILE*      out;
    uint32_t   error_count{};
};

}  // namespace yal
