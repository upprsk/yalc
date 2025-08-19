#pragma once

#include "string_view.h"

typedef struct base {
    struct base *next;
    string_view  type_name;
} Base;

typedef struct {
    Base        base;
    string_view name;
    float       age;
} Named;
