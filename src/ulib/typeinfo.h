#pragma once

#include "string_view.h"
#include <stdint.h>

typedef struct Type_Info Type_Info;
typedef enum Type_Kind   Type_Kind;

// This is a tag for all payloads for Type_Info.
enum Type_Kind {
    TYPE_INVALID,
    TYPE_INTEGER,
    TYPE_REAL,
    TYPE_POINTER,
    TYPE_RECORD,
    TYPE_UNION,
};

// Information about an integer. We store the byte width and if it is signed.
typedef struct {
    uint8_t byte_width;
    bool    is_signed;
} Type_Integer;

// Information abot a floating pointer number. We store the byte width.
typedef struct {
    uint8_t byte_width;
} Type_Real;

// Pointer bit flags.
typedef enum {
    TYPE_POINTER_ATTR_NONE  = 0,
    TYPE_POINTER_ATTR_CONST = 1 << 0,
} Type_Pointer_Attrs;

// Information about a pointer. We store the inner type and some attributes
// (like if it is const).
typedef struct {
    Type_Info const   *inner;
    Type_Pointer_Attrs attrs;
} Type_Pointer;

// Information about a struct field (record field). We store both alignment,
// offset and type.
typedef struct {
    string_view      name;
    size_t           offset;
    Type_Info const *type;
} Type_Record_Field;

// Information about a struct (record). We store all of the fields.
typedef struct {
    size_t                   field_count;
    Type_Record_Field const *fields;
} Type_Record;

typedef struct {
    string_view      name;
    Type_Info const *type;
} Type_Union_Field;

typedef struct {
    size_t                  field_count;
    Type_Union_Field const *fields;
} Type_Union;

typedef union {
    Type_Integer integer;
    Type_Real    real;
    Type_Pointer pointer;
    Type_Record  record;
    Type_Union   union_;
} Type_Info_As;

// This struct holds data about a type. It holds a union tagged by kind with
// more details.
struct Type_Info {
    string_view  name;
    Type_Kind    kind;
    uint32_t     size;
    uint32_t     alignment;
    Type_Info_As as;
};

static inline string_view type_kind_to_string(Type_Kind kind) {
    char const *s = "";
    switch (kind) {
    case TYPE_INVALID: s = "INVALID"; break;
    case TYPE_INTEGER: s = "INTEGER"; break;
    case TYPE_REAL:    s = "REAL"; break;
    case TYPE_POINTER: s = "POINTER"; break;
    case TYPE_RECORD:  s = "RECORD"; break;
    case TYPE_UNION:   s = "UNION"; break;
    }

    return (string_view){.ptr = s, .len = strlen(s)};
}
