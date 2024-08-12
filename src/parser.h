#pragma once

#include <stdint.h>

#include "tokenizer.h"

void parse(error_reporter_t* er, char const* filename, char const* source,
           uint32_t source_len, token_t const* tokens);
