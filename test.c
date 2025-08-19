#include "test.h"
#include "string_view.h"
#include "typeinfo.h"
#include <stdio.h>

#define TYPEINFO_IMPLEMENTATION
#include "type-info.gen.h"

void dump_info(Type_Info const *info) {
    printf("info of \"%.*s\":\n", (int)info->name.len, info->name.ptr);
    printf("    - size: %d\n", info->size);
    printf("    - alignment: %d\n", info->alignment);
    printf("    - kind: %s\n", type_kind_to_string(info->kind).ptr);

    switch (info->kind) {
    case TYPE_INVALID: printf("    <INVALID>\n"); break;
    case TYPE_INTEGER:
        printf("    - format: %s%d\n", info->as.integer.is_signed ? "s" : "u",
               info->as.integer.byte_width * 8);
        break;
    case TYPE_REAL:
        printf("    - format: f%d\n", info->as.real.byte_width * 8);
        break;
    case TYPE_POINTER:
        printf("    - inner: %s%.*s\n",
               info->as.pointer.attrs & TYPE_POINTER_ATTR_CONST ? "const " : "",
               (int)info->as.pointer.inner->name.len,
               info->as.pointer.inner->name.ptr);
        break;
    case TYPE_RECORD:
        printf("    - fields:\n");
        for (size_t i = 0; i < info->as.record.field_count; ++i) {
            auto field = info->as.record.fields[i];

            printf("        - %.*s: %.*s\n", (int)field.name.len,
                   field.name.ptr, (int)field.type->name.len,
                   field.type->name.ptr);
        }
        break;
    case TYPE_UNION:
        printf("    - fields:\n");
        for (size_t i = 0; i < info->as.union_.field_count; ++i) {
            auto field = info->as.union_.fields[i];

            printf("        - %.*s: %.*s\n", (int)field.name.len,
                   field.name.ptr, (int)field.type->name.len,
                   field.type->name.ptr);
        }
        break;
    }
}

typedef enum {
    DUMP_NONE   = 0,
    DUMP_PRETTY = 1 << 0,
} Dump_Flags;

typedef struct {
    Dump_Flags flags;
    int        depth;
} Dump_Options;

void indent(FILE *f, int depth) {
    for (int i = 0; i < depth; ++i) {
        fprintf(f, "    ");
    }
}

void dump_value(FILE *f, void const *value, Type_Info const *type_info,
                Dump_Options const *options) {
    static const Dump_Options default_options = {};
    if (options == nullptr) {
        options = &default_options;
    }

    switch (type_info->kind) {
    case TYPE_INVALID: fprintf(f, "<invalid>"); break;

    case TYPE_INTEGER: {
        if (type_info == typeinfo_of_char()) {
            char const *v = value;
            fprintf(f, "%d", *v);
        } else if (type_info == typeinfo_of_unsigned_char()) {
            unsigned char const *v = value;
            fprintf(f, "%u", *v);
        } else if (type_info == typeinfo_of_short()) {
            short const *v = value;
            fprintf(f, "%d", *v);
        } else if (type_info == typeinfo_of_unsigned_short()) {
            unsigned short const *v = value;
            fprintf(f, "%u", *v);
        } else if (type_info == typeinfo_of_int()) {
            int const *v = value;
            fprintf(f, "%d", *v);
        } else if (type_info == typeinfo_of_unsigned_int()) {
            unsigned int const *v = value;
            fprintf(f, "%u", *v);
        } else if (type_info == typeinfo_of_long()) {
            long const *v = value;
            fprintf(f, "%ld", *v);
        } else if (type_info == typeinfo_of_unsigned_long()) {
            unsigned long const *v = value;
            fprintf(f, "%lu", *v);
        } else if (type_info == typeinfo_of_long_long()) {
            long long const *v = value;
            fprintf(f, "%lld", *v);
        } else if (type_info == typeinfo_of_unsigned_long_long()) {
            unsigned long long const *v = value;
            fprintf(f, "%llu", *v);
        }
    } break;

    case TYPE_REAL: {
        if (type_info == typeinfo_of_float()) {
            float const *v = value;
            fprintf(f, "%f", *v);
        } else if (type_info == typeinfo_of_float()) {
            double const *v = value;
            fprintf(f, "%f", *v);
        }
    } break;

    case TYPE_POINTER: {
        void const *const *v = value;
        if (*v == nullptr) {
            fprintf(f, "(nil)");
        } else {
            fprintf(f, "&");
            dump_value(f, *v, type_info->as.pointer.inner, options);
        }
    } break;

    case TYPE_RECORD: {
        if (type_info == typeinfo_of_string_view()) {
            string_view const *v = value;
            fprintf(f, "\"%.*s\"", (int)v->len, v->ptr);
            break;
        }

        fprintf(f, "%.*s{", (int)type_info->name.len, type_info->name.ptr);
        if (options->flags & DUMP_PRETTY) fprintf(f, "\n");

        auto as_record = &type_info->as.record;
        for (size_t i = 0; i < as_record->field_count; ++i) {
            auto field = &as_record->fields[i];

            if (options->flags & DUMP_PRETTY) indent(f, options->depth + 1);
            fprintf(f, ".%.*s=", (int)field->name.len, field->name.ptr);

            Dump_Options inner_options = {.flags = options->flags,
                                          .depth = options->depth + 1};

            // calculate offset into the struct
            void const *field_value = (uint8_t const *)value + field->offset;
            dump_value(f, field_value, field->type, &inner_options);

            if (options->flags & DUMP_PRETTY) fprintf(f, "\n");
        }

        if (options->flags & DUMP_PRETTY) indent(f, options->depth);
        fprintf(f, "}");
    } break;

    case TYPE_UNION: {
        fprintf(f, "%.*s{}", (int)type_info->name.len, type_info->name.ptr);
    } break;
    }
}

int main() {
    Named n = {.base = {.type_name = string_view_from_cstr("Named")},
               .name = string_view_from_cstr("Named"),
               .age  = 3.14};

    auto info = typeinfo_of(n);
    dump_value(stdout, &n, info, &(Dump_Options){.flags = DUMP_PRETTY});
    fprintf(stdout, "\n");

    dump_value(stdout, info, typeinfo_of(*info),
               &(Dump_Options){.flags = DUMP_PRETTY});
    fprintf(stdout, "\n");

    return 0;
}
