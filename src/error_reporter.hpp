#pragma once

#include <fmt/color.h>

#include <cstdint>
#include <string_view>

#include "file_store.hpp"
#include "location.hpp"

namespace yal {

class ErrorReporter;

class LocalErrorReporter {
    static constexpr auto const error_style = fmt::fg(fmt::color::red);
    static constexpr auto const warn_style = fmt::fg(fmt::color::yellow);
    static constexpr auto const note_style = fmt::fg(fmt::color::cyan);
    static constexpr auto const debug_style =
        fmt::fg(fmt::color::medium_purple);
    static constexpr auto const bug_style = fmt::bg(fmt::color::crimson) |
                                            fmt::fg(fmt::color::white) |
                                            fmt::emphasis::bold;

public:
    constexpr explicit LocalErrorReporter(FileId fileid, FILE* out,
                                          FileStore* fs, ErrorReporter* parent)
        : parent{parent}, fs{fs}, out{out}, fileid{fileid} {}

    template <typename... T>
    void report_error(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_error(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_warn(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_warn(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_note(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_note(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_debug(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_debug(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_bug(Span s, fmt::format_string<T...> fmt, T&&... args) {
        vreport_bug(s, fmt, fmt::make_format_args(args...));
    }

    void vreport_error(Span s, fmt::string_view fmt, fmt::format_args args);
    void vreport_warn(Span s, fmt::string_view fmt, fmt::format_args args);
    void vreport_note(Span s, fmt::string_view fmt, fmt::format_args args);
    void vreport_debug(Span s, fmt::string_view fmt, fmt::format_args args);
    void vreport_bug(Span s, fmt::string_view fmt, fmt::format_args args);

    void report(Span s, std::string_view prefix, fmt::text_style color,
                fmt::string_view fmt, fmt::format_args args,
                uint32_t context = 1);

    // -----------------------------------------------------------------------

    // Get the file metadata this error reporter belongs to. In case the file is
    // not valid, then this method panics.
    [[nodiscard]] auto get_file() const -> FileStore::File {
        return *fs->get_file_by_id(fileid);
    }

    // Get the contents of the file this error reporter belongs to. In case the
    // file is not valid, then this method panics.
    [[nodiscard]] auto get_source() const -> std::string_view {
        return get_file().contents;
    }

    // Get the contents of the file this error reporter belongs to. In case the
    // file is not valid, then this method panics.
    [[nodiscard]] auto get_source_path() const -> std::string_view {
        return get_file().original_path;
    }

    [[nodiscard]] constexpr auto get_fileid() const -> FileId { return fileid; }

    // -----------------------------------------------------------------------

private:
    // reference to the `ErrorReporter` that made this `LocalErrorReporter`
    ErrorReporter* parent;

    // we use this to get contents and file information
    FileStore* fs;

    FILE*  out;
    FileId fileid;
};

// =======================================================================

class ErrorReporter {
    friend LocalErrorReporter;

public:
    constexpr ErrorReporter(FileStore* fs, FILE* out) : fs{fs}, out{out} {}

    template <typename... T>
    void report_error(Location const& s, fmt::format_string<T...> fmt,
                      T&&... args) {
        vreport_error(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_warn(Location const& s, fmt::format_string<T...> fmt,
                     T&&... args) {
        vreport_warn(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_note(Location const& s, fmt::format_string<T...> fmt,
                     T&&... args) {
        vreport_note(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_debug(Location const& s, fmt::format_string<T...> fmt,
                      T&&... args) {
        vreport_debug(s, fmt, fmt::make_format_args(args...));
    }

    template <typename... T>
    void report_bug(Location const& s, fmt::format_string<T...> fmt,
                    T&&... args) {
        vreport_bug(s, fmt, fmt::make_format_args(args...));
    }

    void vreport_error(Location const& s, fmt::string_view fmt,
                       fmt::format_args args);
    void vreport_warn(Location const& s, fmt::string_view fmt,
                      fmt::format_args args);
    void vreport_note(Location const& s, fmt::string_view fmt,
                      fmt::format_args args);
    void vreport_debug(Location const& s, fmt::string_view fmt,
                       fmt::format_args args);
    void vreport_bug(Location const& s, fmt::string_view fmt,
                     fmt::format_args args);

    // -----------------------------------------------------------------------

    [[nodiscard]] auto for_file(Location const& loc) -> LocalErrorReporter {
        return for_file(loc.fileid);
    }

    [[nodiscard]] auto for_file(FileId id) -> LocalErrorReporter {
        return LocalErrorReporter{id, out, fs, this};
    }

    // -----------------------------------------------------------------------

    [[nodiscard]] constexpr auto had_error() const -> bool {
        return error_count > 0;
    }

    [[nodiscard]] constexpr auto get_error_count() const -> uint32_t {
        return error_count;
    }

    // -----------------------------------------------------------------------

private:
    // we use this to get contents and file information
    FileStore* fs;

    FILE*    out;
    uint32_t error_count{};
};

}  // namespace yal
