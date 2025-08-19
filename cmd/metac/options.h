#pragma once

#include "string_view.h"
#include <stdint.h>
#include <stdio.h>

typedef struct {
    string_view *items;
    uint32_t     len;
    uint32_t     cap;
} String_Array;

typedef struct {
    char const **items;
    uint32_t     len;
    uint32_t     cap;
} CString_Array;

typedef enum {
    OPTION_NONE            = 0,
    OPTION_VERBOSE_OPTIONS = 1 << 0,
    OPTION_VERBOSE_PARSE   = 1 << 1,
    OPTION_VERBOSE_SEARCH  = 1 << 2,
    OPTION_VERBOSE_PATHS   = 1 << 3,
    OPTION_PRINT_ALL_TYPES = 1 << 4,
} Option_Flags;

typedef struct {
    String_Array  input_files;
    string_view   output_file;
    CString_Array extra_clang_args;
    string_view   clang_resource_dir;
    Option_Flags  flags;
} Options;

void print_usage(string_view self);
void print_help(void);

void options_destroy(Options *opt);

void options_dump(FILE *o, Options const *opt);

Options parse_options(int argc, char **argv);
