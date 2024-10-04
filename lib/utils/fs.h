#pragma once

#include "alloc/allocator.h"

char* read_entire_file(char const* filename, allocator_t alloc, size_t* len);
