#pragma once

#include <stdarg.h>
#include <stdio.h>

#include "span.h"

typedef struct error_reporter {
    uint32_t error_count;

    FILE* stream;
} error_reporter_t;

void report_error(error_reporter_t* er, char const* filename,
                  char const* source, span_t span, char const* format, ...);

void vreport_error(error_reporter_t* er, char const* filename,
                   char const* source, span_t span, char const* format,
                   va_list va);
