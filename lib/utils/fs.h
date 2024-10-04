#pragma once

#include "alloc/allocator.h"
#include "slice/slice.h"

typedef enum fs_result {
    FSR_OK,
    FSR_ERR,
    FSR_NOT_OPEN,
} fs_result_t;

/// Read the entirety of a file into a dynamically allocated string. The
/// returned string is null-terminated (which is not included in the length),
/// but there is no garantee that the file does not contain other null bytes.
fs_result_t read_entire_file(char const* filename, allocator_t alloc,
                             slice_char_t* c);
