#pragma once

#include <stdarg.h>
#include <stdio.h>

#include "slice/slice.h"
#include "span.h"

typedef struct error_reporter {
    uint32_t error_count;

    FILE* stream;

    char const* filename;
    str_t       source;
} error_reporter_t;

__attribute__((format(printf, 3, 4))) 
void report_error(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_error(error_reporter_t* er, span_t span, char const* format,
                   va_list va);

__attribute__((format(printf, 3, 4))) 
void report_warn(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_warn(error_reporter_t* er, span_t span, char const* format,
                  va_list va);

__attribute__((format(printf, 3, 4))) 
void report_note(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_note(error_reporter_t* er, span_t span, char const* format,
                  va_list va);

__attribute__((format(printf, 3, 4))) 
void report_success(error_reporter_t* er, span_t span, char const* format, ...);
void vreport_success(error_reporter_t* er, span_t span, char const* format,
                     va_list va);
