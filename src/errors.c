#include "errors.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#include "span.h"

#define ANSI_ESC "\033["
#define ANSI_END "m"

#define ANSI_RED   ANSI_ESC "31" ANSI_END
#define ANSI_CYAN  ANSI_ESC "36" ANSI_END
#define ANSI_RESET ANSI_ESC "0" ANSI_END

static uint32_t find_line_start(char const* source, span_t span) {
    uint32_t i = span.start;
    for (; i > 0 && source[i] != '\n'; --i) {
    }

    return i == 0 ? 0 : i + 1;
}

static uint32_t count_lines(char const* source, span_t span) {
    uint32_t lines = 0;

    for (uint32_t i = 0; i < span.start; ++i) {
        if (source[i] == '\n') lines++;
        if (i == span.start) return lines;
    }

    return lines;
}

static uint32_t count_line_cols(uint32_t line_start, span_t span) {
    uint32_t cols = 0;
    for (uint32_t i = line_start; i < span.start; ++i, ++cols) {
    }

    return cols;
}

static uint32_t find_line_end(char const* source, span_t span) {
    uint32_t i = span.start;
    for (; source[i] != 0 && source[i] != '\n'; ++i) {
    }

    return i;
}

static void show_span(error_reporter_t* er, char const* color,
                      char const* source, span_t span, uint32_t line,
                      uint32_t line_start, uint32_t line_end,
                      uint32_t line_col) {
    fprintf(er->stream, "% 4d | %.*s\n", line + 1, line_end - line_start,
            &source[line_start]);
    fprintf(er->stream, "       ");
    for (size_t i = 0; i < line_col; ++i) fputc(' ', er->stream);

    if (isatty(fileno(er->stream))) fprintf(er->stream, "%s", color);

    for (size_t i = span.start; i < span.end && i < line_end; ++i)
        fputc('^', er->stream);

    if (isatty(fileno(er->stream))) fprintf(er->stream, ANSI_RESET);

    fprintf(er->stream, "\n");
}

static void show_message(error_reporter_t* er, char const* color,
                         char const* kind, char const* filename, uint32_t line,
                         uint32_t line_col, char const* format, va_list va) {
    fprintf(er->stream, "%s:%d:%d: ", filename, line + 1, line_col + 1);

    if (isatty(fileno(er->stream))) {
        fprintf(er->stream, "%s%s:" ANSI_RESET " ", color, kind);
    } else {
        fprintf(er->stream, "%s: ", kind);
    }

    vfprintf(er->stream, format, va);
    fprintf(er->stream, "\n");
}

void report_error_opt(error_reporter_t* er, char const* filename,
                      char const* source, span_t span, char const* format,
                      ...) {
    va_list va;
    va_start(va, format);
    vreport_error_opt(er, filename, source, span, format, va);
    va_end(va);
}

void vreport_error_opt(error_reporter_t* er, char const* filename,
                       char const* source, span_t span, char const* format,
                       va_list va) {
    er->error_count++;

    uint32_t line = count_lines(source, span);
    uint32_t line_start = find_line_start(source, span);
    uint32_t line_end = find_line_end(source, span);
    uint32_t line_col = count_line_cols(line_start, span);

    munit_assert_uint32(line_end, >=, line_start);

    show_message(er, ANSI_RED, "error", filename, line, line_col, format, va);
    show_span(er, ANSI_RED, source, span, line, line_start, line_end, line_col);
}

void report_note_opt(error_reporter_t* er, char const* filename,
                     char const* source, span_t span, char const* format, ...) {
    va_list va;
    va_start(va, format);

    vreport_note_opt(er, filename, source, span, format, va);

    va_end(va);
}

void vreport_note_opt(error_reporter_t* er, char const* filename,
                      char const* source, span_t span, char const* format,
                      va_list va) {
    munit_assert_not_null(er);
    munit_assert_not_null(filename);
    munit_assert_not_null(source);

    uint32_t line = count_lines(source, span);
    uint32_t line_start = find_line_start(source, span);
    uint32_t line_end = find_line_end(source, span);
    uint32_t line_col = count_line_cols(line_start, span);

    munit_assert_uint32(line_end, >=, line_start);
    show_message(er, ANSI_CYAN, "note", filename, line, line_col, format, va);
    show_span(er, ANSI_CYAN, source, span, line, line_start, line_end,
              line_col);
}

void report_error(error_reporter_t* er, span_t span, char const* format, ...) {
    va_list va;
    va_start(va, format);
    vreport_error(er, span, format, va);
    va_end(va);
}

void vreport_error(error_reporter_t* er, span_t span, char const* format,
                   va_list va) {
    vreport_error_opt(er, er->filename, er->source, span, format, va);
}

void report_note(error_reporter_t* er, span_t span, char const* format, ...) {
    va_list va;
    va_start(va, format);
    vreport_note(er, span, format, va);
    va_end(va);
}

void vreport_note(error_reporter_t* er, span_t span, char const* format,
                  va_list va) {
    vreport_note_opt(er, er->filename, er->source, span, format, va);
}
