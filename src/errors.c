#include "errors.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#include "span.h"

#define ANSI_ESC "\033["
#define ANSI_END "m"

#define ANSI_RED   ANSI_ESC "31" ANSI_END
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

void report_error(error_reporter_t* er, char const* filename,
                  char const* source, span_t span, char const* format, ...) {
    va_list va;
    va_start(va, format);

    vreport_error(er, filename, source, span, format, va);

    va_end(va);
}

void vreport_error(error_reporter_t* er, char const* filename,
                   char const* source, span_t span, char const* format,
                   va_list va) {
    er->error_count++;

    uint32_t line = count_lines(source, span);
    uint32_t line_start = find_line_start(source, span);
    uint32_t line_end = find_line_end(source, span);
    uint32_t line_col = count_line_cols(line_start, span);

    fprintf(er->stream, "%s:%d:%d: ", filename, line + 1, line_col + 1);

    if (isatty(fileno(er->stream))) {
        fprintf(er->stream, ANSI_RED "error:" ANSI_RESET " ");
    } else {
        fprintf(er->stream, "error: ");
    }

    vfprintf(er->stream, format, va);
    fprintf(er->stream, "\n");

    munit_assert_uint32(line_end, >=, line_start);

    fprintf(er->stream, "% 4d | %.*s\n", line + 1, line_end - line_start,
            &source[line_start]);
    fprintf(er->stream, "       ");
    for (size_t i = 0; i < line_col; ++i) fputc(' ', er->stream);

    if (isatty(fileno(er->stream))) {
        fprintf(er->stream, ANSI_RED "^" ANSI_RESET "\n");
    } else {
        fprintf(er->stream, "^\n");
    }
}
