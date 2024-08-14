#pragma once

#include <stdio.h>

#include "ast.h"
#include "typestore.h"

typedef struct codegen_params {
    FILE*   out;
    node_t* ast;

    typestore_t*      ts;
} codegen_params_t;

void codegen_wasm(codegen_params_t* params);
