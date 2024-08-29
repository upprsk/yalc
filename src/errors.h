#pragma once

#include <stdarg.h>
#include <stdio.h>

#include "span.h"

typedef struct error_reporter {
    uint32_t error_count;

    FILE* stream;

    char const* filename;
    char const* source;
    size_t      source_len;
} error_reporter_t;

void report_error_opt(error_reporter_t* er, char const* filename,
                      char const* source, span_t span, char const* format, ...);
void vreport_error_opt(error_reporter_t* er, char const* filename,
                       char const* source, span_t span, char const* format,
                       va_list va);

void report_warn_opt(error_reporter_t* er, char const* filename,
                     char const* source, span_t span, char const* format, ...);
void vreport_warn_opt(error_reporter_t* er, char const* filename,
                      char const* source, span_t span, char const* format,
                      va_list va);

void report_note_opt(error_reporter_t* er, char const* filename,
                     char const* source, span_t span, char const* format, ...);
void vreport_note_opt(error_reporter_t* er, char const* filename,
                      char const* source, span_t span, char const* format,
                      va_list va);

void report_error(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_error(error_reporter_t* er, span_t span, char const* format,
                   va_list va);

void report_warn(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_warn(error_reporter_t* er, span_t span, char const* format,
                   va_list va);

void report_note(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_note(error_reporter_t* er, span_t span, char const* format,
                  va_list va);
