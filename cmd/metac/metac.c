#include "arena.h"
#include "da.h"
#include "defer.h"
#include "options.h"
#include "string_view.h"
#include "typeinfo.h"
#include <clang-c/CXString.h>
#include <clang-c/Index.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef struct Type Type;
struct Type {
    Type_Info info;
    Type     *next;

    bool is_in_symtable;
};

typedef struct {
    string_view      name;
    Type            *type;
    CXSourceLocation location;

    bool is_typedef;
} Symbol;

// Store a list of named types (that can be referenced by name) and a
// linked-list of all types created.
typedef struct {
    Symbol  *items;
    uint32_t len;
    uint32_t cap;

    Type *head;
} Sym_Table;

typedef struct {
    Type_Record_Field *items;
    uint32_t           len;
    uint32_t           cap;
} Field_Array;

typedef struct {
    Options const *opt;
    Sym_Table      symtable;

    CXType      current_type;
    Type       *current_type_def;
    Field_Array current_fields;

    // The invalid type, reused for all instances
    Type_Info *inval_type;

    // just for pretty printing
    int indent_level;

    Arena arena;
} State;

typedef struct {
    Type_Info const **items;
    uint32_t          len;
    uint32_t          cap;

    int type_depth;
} Type_Array;

void state_destroy(State *s) {
    da_destroy(&s->symtable);
    da_destroy(&s->current_fields);
    arena_free(&s->arena);
}

string_view my_strdup(State *s, string_view str) {
    char *new_str = arena_alloc(&s->arena, str.len + 1);
    memcpy(new_str, str.ptr, str.len);
    new_str[str.len] = 0; // null termintate

    return (string_view){.ptr = new_str, .len = str.len};
}

Symbol *find_sym_by_name(State *s, string_view name) {
    for (size_t i = 0; i < s->symtable.len; ++i) {
        if (string_view_eq(s->symtable.items[i].name, name))
            return &s->symtable.items[i];
    }

    return nullptr;
}

Type *new_type(State *s, Type_Info info) {
    Type *t = arena_alloc(&s->arena, sizeof(*t));
    *t      = (Type){.info = info};

    t->next          = s->symtable.head;
    s->symtable.head = t;

    return t;
}

Symbol *add_sym(State *s, CXSourceLocation location, string_view name,
                Type *type) {
    type->is_in_symtable = true;

    Symbol sym = {.name = name, .location = location, .type = type};
    da_append(&s->symtable, sym);

    return &da_last(&s->symtable);
}

Type *resolve_type(State *s, CXType type) {
    auto raw_type_name = clang_getTypeSpelling(type);
    defer { clang_disposeString(raw_type_name); }

    auto type_name = string_view_from_cstr(clang_getCString(raw_type_name));

    auto sym = find_sym_by_name(s, type_name);
    if (sym) return sym->type;

    if (type.kind == CXType_Pointer) {
        auto raw_child = clang_getPointeeType(type);
        auto child     = resolve_type(s, clang_getUnqualifiedType(raw_child));
        if (child == nullptr) {
            auto child_str = clang_getTypeSpelling(clang_getPointeeType(type));
            defer { clang_disposeString(child_str); }

            printf("warn: failed to resolve pointer inner type: %s\n",
                   clang_getCString(child_str));
            return nullptr;
        }

        // check if the target is const
        auto is_const = clang_isConstQualifiedType(raw_child);

        Type_Pointer_Attrs attrs = TYPE_POINTER_ATTR_NONE;
        if (is_const) attrs |= TYPE_POINTER_ATTR_CONST;

        auto nt = new_type(
            s,
            (Type_Info){.name       = my_strdup(s, type_name),
                        .size       = clang_Type_getSizeOf(raw_child),
                        .alignment  = clang_Type_getAlignOf(raw_child),
                        .kind       = TYPE_POINTER,
                        .as.pointer = {.inner = &child->info, .attrs = attrs}});
        add_sym(s, (CXSourceLocation){}, nt->info.name, nt);

        return nt;
    }

    return nullptr;
}

void fill_fields_for_struct(State *s, Type *type) {
    assert(type->info.kind == TYPE_RECORD);

    auto size = sizeof(*type->info.as.record.fields) * s->current_fields.len;
    Type_Record_Field *fields = arena_alloc(&s->arena, size);
    for (size_t i = 0; i < s->current_fields.len; ++i) {
        fields[i] =
            (Type_Record_Field){.name   = s->current_fields.items[i].name,
                                .offset = s->current_fields.items[i].offset,
                                .type   = s->current_fields.items[i].type};
    }

    type->info.as.record.field_count = s->current_fields.len;
    type->info.as.record.fields      = fields;

    da_clear(&s->current_fields);
}

void fill_fields_for_union(State *s, Type *type) {
    assert(type->info.kind == TYPE_UNION);

    auto size = sizeof(*type->info.as.union_.fields) * s->current_fields.len;
    Type_Union_Field *fields = arena_alloc(&s->arena, size);
    for (size_t i = 0; i < s->current_fields.len; ++i) {
        fields[i] = (Type_Union_Field){.name = s->current_fields.items[i].name,
                                       .type = s->current_fields.items[i].type};
    }

    type->info.as.union_.field_count = s->current_fields.len;
    type->info.as.union_.fields      = fields;

    da_clear(&s->current_fields);
}

void state_add_bultin_types(State *s) {
    // add the invalid type
    s->inval_type  = arena_alloc(&s->arena, sizeof(*s->inval_type));
    *s->inval_type = (Type_Info){.name = string_view_from_cstr("inval"),
                                 .kind = TYPE_INVALID};

#define add(name, ...)                                                         \
    add_sym(s, (CXSourceLocation){}, name, new_type(s, (Type_Info)__VA_ARGS__))

    add(string_view_from_cstr("char"),
        {.name       = string_view_from_cstr("char"),
         .size       = sizeof(char),
         .alignment  = alignof(char),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(char), .is_signed = true}});
    add(string_view_from_cstr("unsigned char"),
        {.name       = string_view_from_cstr("unsigned char"),
         .size       = sizeof(unsigned char),
         .alignment  = alignof(unsigned char),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(unsigned char),
                        .is_signed  = false}});

    add(string_view_from_cstr("short"),
        {.name       = string_view_from_cstr("short"),
         .size       = sizeof(short),
         .alignment  = alignof(short),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(short), .is_signed = true}});
    add(string_view_from_cstr("unsigned short"),
        {.name       = string_view_from_cstr("unsigned short"),
         .size       = sizeof(unsigned short),
         .alignment  = alignof(unsigned short),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(unsigned short),
                        .is_signed  = false}});

    add(string_view_from_cstr("int"),
        {.name       = string_view_from_cstr("int"),
         .size       = sizeof(int),
         .alignment  = alignof(int),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(int), .is_signed = true}});
    add(string_view_from_cstr("unsigned int"),
        {.name       = string_view_from_cstr("unsigned int"),
         .size       = sizeof(unsigned int),
         .alignment  = alignof(unsigned int),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(unsigned int),
                        .is_signed  = false}});

    add(string_view_from_cstr("long"),
        {.name       = string_view_from_cstr("long"),
         .size       = sizeof(long),
         .alignment  = alignof(long),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(long), .is_signed = true}});
    add(string_view_from_cstr("unsigned long"),
        {.name       = string_view_from_cstr("unsigned long"),
         .size       = sizeof(unsigned long),
         .alignment  = alignof(unsigned long),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(unsigned long),
                        .is_signed  = false}});

    add(string_view_from_cstr("long long"),
        {.name       = string_view_from_cstr("long long"),
         .size       = sizeof(long long),
         .alignment  = alignof(long long),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(long long), .is_signed = true}});
    add(string_view_from_cstr("unsigned long long"),
        {.name       = string_view_from_cstr("unsigned long long"),
         .size       = sizeof(unsigned long long),
         .alignment  = alignof(unsigned long long),
         .kind       = TYPE_INTEGER,
         .as.integer = {.byte_width = sizeof(unsigned long long),
                        .is_signed  = false}});

    add(string_view_from_cstr("float"),
        {.name      = string_view_from_cstr("float"),
         .size      = sizeof(float),
         .alignment = alignof(float),
         .kind      = TYPE_REAL,
         .as.real   = {.byte_width = sizeof(float)}});
    add(string_view_from_cstr("double"),
        {.name      = string_view_from_cstr("double"),
         .size      = sizeof(double),
         .alignment = alignof(double),
         .kind      = TYPE_REAL,
         .as.real   = {.byte_width = sizeof(double)}});

#undef add
}

// ----------------------------------------------------------------------------

char *type_info_to_string(Arena *a, Type_Info const *ti) {
    switch (ti->kind) {
    case TYPE_INVALID: return "invalid";
    case TYPE_INTEGER:
        return arena_sprintf(a, "%c%d", ti->as.integer.is_signed ? 'i' : 'u',
                             ti->as.integer.byte_width * 8);
    case TYPE_REAL: return arena_sprintf(a, "f%d", ti->as.real.byte_width * 8);
    case TYPE_POINTER:
        return arena_sprintf(
            a, "%s%s*", type_info_to_string(a, ti->as.pointer.inner),
            ti->as.pointer.attrs & TYPE_POINTER_ATTR_CONST ? " const" : "");
    case TYPE_RECORD: {
        char *s = "struct {";

        for (size_t i = 0; i < ti->as.record.field_count; ++i) {
            char const *comma = "";
            if (i != 0) comma = ", ";

            s = arena_sprintf(a, "%s%s%.*s: %.*s", s, comma,
                              (int)ti->as.record.fields[i].name.len,
                              ti->as.record.fields[i].name.ptr,
                              (int)ti->as.record.fields[i].type->name.len,
                              ti->as.record.fields[i].type->name.ptr);
        }

        s = arena_sprintf(a, "%s}", s);
        return s;
    }
    case TYPE_UNION: {
        char *s = "union {";

        for (size_t i = 0; i < ti->as.union_.field_count; ++i) {
            char const *comma = "";
            if (i != 0) comma = ", ";

            s = arena_sprintf(a, "%s%s%.*s: %.*s", s, comma,
                              (int)ti->as.union_.fields[i].name.len,
                              ti->as.union_.fields[i].name.ptr,
                              (int)ti->as.union_.fields[i].type->name.len,
                              ti->as.union_.fields[i].type->name.ptr);
        }

        s = arena_sprintf(a, "%s}", s);
        return s;
    }
    }

    assert(false && "invalid typeinfo kind");
}

// ----------------------------------------------------------------------------

void indent(int level) {
    for (int i = 0; i < level - 1; ++i) {
        printf("    ");
    }

    if (level > 0) {
        printf("  L ");
    }
}

void print_location(CXSourceLocation location) {
    CXFile       file;
    unsigned int line, column, offset;
    clang_getSpellingLocation(location, &file, &line, &column, &offset);

    auto filename = clang_getFileName(file);
    defer { clang_disposeString(filename); }

    printf("%s:%d:%d: ", clang_getCString(filename), line, column);
}

// ----------------------------------------------------------------------------

enum CXChildVisitResult struct_visitor(CXCursor cursor, CXCursor parent,
                                       void *user_data) {
    State *s = user_data;

    auto kind = clang_getCursorKind(cursor);
    auto type = clang_getCursorType(cursor);

    auto cursor_str = clang_getCursorSpelling(cursor);
    defer { clang_disposeString(cursor_str); }

    if (kind == CXCursor_FieldDecl) {
        auto field_name =
            my_strdup(s, string_view_from_cstr(clang_getCString(cursor_str)));

        auto type_str = clang_getTypeSpelling(type);
        defer { clang_disposeString(type_str); }

        auto offset =
            clang_Type_getOffsetOf(s->current_type, field_name.ptr) / 8;

        if (s->opt->flags & OPTION_VERBOSE_PARSE) {
            auto kind_str = clang_getCursorKindSpelling(kind);
            defer { clang_disposeString(kind_str); }

            if (s->opt->flags & OPTION_VERBOSE_PATHS)
                print_location(clang_getCursorLocation(cursor));

            indent(s->indent_level);
            printf("parsed %s for: '%s' with type '%s' (offset=%lld)\n",
                   clang_getCString(kind_str), clang_getCString(cursor_str),
                   clang_getCString(type_str), offset);
        }

        auto rtype = resolve_type(s, type);
        if (rtype == nullptr) {
            if (s->opt->flags & OPTION_VERBOSE_PATHS)
                print_location(clang_getCursorLocation(cursor));

            auto parent_str = clang_getCursorSpelling(parent);
            defer { clang_disposeString(parent_str); }

            auto type_kind_str = clang_getTypeKindSpelling(type.kind);
            defer { clang_disposeString(type_kind_str); }

            indent(s->indent_level);
            printf("warn: when visiting %s failed to resolve type: '%s' (of "
                   "kind %s)\n",
                   clang_getCString(parent_str), clang_getCString(type_str),
                   clang_getCString(type_kind_str));
        }

        if (rtype) {
            da_append(&s->current_fields,
                      (Type_Record_Field){.name   = field_name,
                                          .offset = offset,
                                          .type   = &rtype->info});
        }

        else {
            da_append(&s->current_fields,
                      (Type_Record_Field){.name   = field_name,
                                          .offset = offset,
                                          .type   = s->inval_type});
        }
    }

    else if (kind == CXCursor_AnnotateAttr) {
        if (s->opt->flags & OPTION_VERBOSE_SEARCH) {
            indent(s->indent_level);
            printf("parsed annotation: '%s'\n", clang_getCString(cursor_str));
        }

        indent(s->indent_level + 1);
        printf("warn: annotations not implemented\n");
    }

    return CXChildVisit_Continue;
}

enum CXChildVisitResult
top_level_visitor(CXCursor cursor, CXCursor /* parent */, void *user_data) {
    State *s = user_data;

    auto kind = clang_getCursorKind(cursor);
    auto type = clang_getCursorType(cursor);

    auto cursor_str = clang_getCursorSpelling(cursor);
    defer { clang_disposeString(cursor_str); }

    auto type_str = clang_getTypeSpelling(type);
    defer { clang_disposeString(type_str); }

    auto location = clang_getCursorLocation(cursor);

    if (s->opt->flags & OPTION_VERBOSE_PARSE) {
        auto kind_str = clang_getCursorKindSpelling(kind);
        defer { clang_disposeString(kind_str); }

        if (s->opt->flags & OPTION_VERBOSE_PATHS) print_location(location);

        indent(s->indent_level);
        printf("parsed %s for '%s' with type '%s'\n",
               clang_getCString(kind_str), clang_getCString(cursor_str),
               clang_getCString(type_str));
    }

    if (kind == CXCursor_StructDecl || kind == CXCursor_UnionDecl) {
        s->indent_level++;
        defer { s->indent_level--; };

        auto actual_type     = clang_getCanonicalType(type);
        auto actual_type_str = clang_getTypeSpelling(actual_type);
        defer { clang_disposeString(actual_type_str); }

        auto type_name =
            string_view_from_cstr(clang_getCString(actual_type_str));
        auto sym = find_sym_by_name(s, type_name);
        if (sym) {
            indent(s->indent_level);
            printf("warn: symbol already defined: '%s'\n",
                   clang_getCString(cursor_str));
        }
        if (s->opt->flags & OPTION_VERBOSE_SEARCH) {
            indent(s->indent_level);
            printf("defining with actual name: '%s'\n",
                   clang_getCString(actual_type_str));
        }

        Type_Kind expected_kind = TYPE_INVALID;
        if (kind == CXCursor_StructDecl)
            expected_kind = TYPE_RECORD;
        else if (CXCursor_UnionDecl)
            expected_kind = TYPE_UNION;

        Type *t;
        if (sym) {
            t = sym->type;
        } else {
            t = new_type(
                s, (Type_Info){.name      = my_strdup(s, type_name),
                               .size      = clang_Type_getSizeOf(actual_type),
                               .alignment = clang_Type_getAlignOf(actual_type),
                               .kind      = expected_kind});
            add_sym(s, location, t->info.name, t);
        }

        s->current_type     = type;
        s->current_type_def = t;
        da_clear(&s->current_fields);

        if (t->info.as.record.field_count == 0) {
            clang_visitChildren(cursor, struct_visitor, s);
        }
    }

    if (kind == CXCursor_StructDecl) {
        fill_fields_for_struct(s, s->current_type_def);
    }

    if (kind == CXCursor_UnionDecl) {
        fill_fields_for_union(s, s->current_type_def);
    }

    if (kind == CXCursor_TypedefDecl) {
        auto underlying     = clang_getTypedefDeclUnderlyingType(cursor);
        auto underlying_str = clang_getTypeSpelling(underlying);
        defer { clang_disposeString(underlying_str); }

        s->indent_level++;
        defer { s->indent_level--; };

        if (s->opt->flags & OPTION_VERBOSE_SEARCH) {
            indent(s->indent_level);
            printf("parsed typedef to '%s'\n",
                   clang_getCString(underlying_str));
        }

        auto underlying_sym = resolve_type(s, underlying);
        if (underlying_sym == nullptr) {
            indent(s->indent_level);
            printf("warn: failed to resolve type: '%s'\n",
                   clang_getCString(underlying_str));
        } else {
            auto name = string_view_from_cstr(clang_getCString(cursor_str));
            auto sym = add_sym(s, location, my_strdup(s, name), underlying_sym);
            sym->is_typedef = true;
        }
    }

    return CXChildVisit_Continue;
}

// ----------------------------------------------------------------------------

int parse_translation_unit(State *s, CXIndex index, string_view file,
                           string_view          clang_resource_dir,
                           CString_Array const *extra_clang_args) {
    CString_Array cmd_args = {};
    defer { da_destroy(&cmd_args); }

    da_append(&cmd_args, "-xc");
    da_append(&cmd_args, "-resource-dir");
    da_append(&cmd_args, clang_resource_dir.ptr);

    for (size_t i = 0; i < extra_clang_args->len; ++i) {
        da_append(&cmd_args, extra_clang_args->items[i]);
    }

    if (s->opt->flags & OPTION_VERBOSE_PARSE) {
        printf("[*] parsing: '%.*s'\n", (int)file.len, file.ptr);
    }

    auto unit = clang_parseTranslationUnit(index, file.ptr, cmd_args.items,
                                           cmd_args.len, nullptr, 0,
                                           CXTranslationUnit_None);
    if (unit == nullptr) {
        fprintf(stderr, "[!] error: failed to parse translation unit: %.*s\n",
                (int)file.len, file.ptr);
        return 1;
    }
    defer { clang_disposeTranslationUnit(unit); }

    auto cursor = clang_getTranslationUnitCursor(unit);
    clang_visitChildren(cursor, top_level_visitor, s);

    return 0;
}

// ----------------------------------------------------------------------------

string_view sanitize_name(Arena *a, string_view name) {
    char *new_str = arena_alloc(a, name.len + 1);
    memcpy(new_str, name.ptr, name.len);
    new_str[name.len] = 0; // null termintate

    for (size_t i = 0; i < name.len; ++i) {
        if (new_str[i] == ' ') new_str[i] = '_';
        if (new_str[i] == '*') new_str[i] = '_';
    }

    return (string_view){.ptr = new_str, .len = name.len};
}

bool find_type_in_deps_by_addr(Type_Array *deps, Type_Info const *type) {
    bool found = false;
    for (size_t i = 0; i < deps->len; ++i) {
        if (deps->items[i] == type) {
            found = true;
            break;
        }
    }

    return found;
}

void add_type_to_deps(Type_Array *deps, Type_Info const *type) {
    if (deps->type_depth > 10) {
        fprintf(stderr, "warn: type depth of %d exceeded, stopping\n",
                deps->type_depth - 1);
        return;
    }

    deps->type_depth++;
    defer { deps->type_depth--; }

    if (find_type_in_deps_by_addr(deps, type)) return;

    // add the dependencies of this type
    if (type->kind == TYPE_POINTER) {
        if (type->as.pointer.inner->kind != TYPE_RECORD)
            add_type_to_deps(deps, type->as.pointer.inner);
    } else if (type->kind == TYPE_RECORD) {
        for (size_t i = 0; i < type->as.record.field_count; ++i) {
            if (type->as.record.fields[i].type->kind != TYPE_RECORD)
                add_type_to_deps(deps, type->as.record.fields[i].type);
        }
    } else if (type->kind == TYPE_UNION) {
        for (size_t i = 0; i < type->as.union_.field_count; ++i) {
            if (type->as.union_.fields[i].type->kind != TYPE_UNION)
                add_type_to_deps(deps, type->as.union_.fields[i].type);
        }
    }

    if (find_type_in_deps_by_addr(deps, type)) return;

    // then add the type itself
    da_append(deps, type);
}

void generate_function_proto_for(State *s, FILE *o, Type_Info const *info) {
    auto name_id = sanitize_name(&s->arena, info->name);

    fprintf(o, "Type_Info const *typeinfo_of_%.*s(void)", (int)name_id.len,
            name_id.ptr);
}

void generate_function_for(State *s, FILE *o, Type_Info const *info) {
    generate_function_proto_for(s, o, info);

    auto name_id = sanitize_name(&s->arena, info->name);

    fprintf(o, " {\n");
    fprintf(o, "    return &info_for_%.*s;\n", (int)name_id.len, name_id.ptr);
    fprintf(o, "}\n\n");
}

void generate_typeinfo_for(State *s, FILE *o, Type_Info const *info) {
    auto name_id = sanitize_name(&s->arena, info->name);

    // FIXME: save that a name is a typedef and not define info for it. We can
    // just return the same thing when creating the public api.

    if (info->kind == TYPE_RECORD) {
        fprintf(o, "static const Type_Record_Field fields_of_%.*s[%zu] = {\n",
                (int)name_id.len, name_id.ptr, info->as.record.field_count);

        for (size_t i = 0; i < info->as.record.field_count; ++i) {
            auto field          = info->as.record.fields[i];
            auto field_type_str = sanitize_name(&s->arena, field.type->name);

            fprintf(o,
                    "    {.name = {\"%.*s\", %zu}, .offset = %zu, .type = "
                    "&info_for_%.*s},\n",
                    (int)field.name.len, field.name.ptr, field.name.len,
                    field.offset, (int)field_type_str.len, field_type_str.ptr);
        }

        fprintf(o, "};\n");
    } else if (info->kind == TYPE_UNION) {
        fprintf(o, "static const Type_Union_Field fields_of_%.*s[%zu] = {\n",
                (int)name_id.len, name_id.ptr, info->as.union_.field_count);

        for (size_t i = 0; i < info->as.union_.field_count; ++i) {
            auto field          = info->as.union_.fields[i];
            auto field_type_str = sanitize_name(&s->arena, field.type->name);

            fprintf(o,
                    "    {.name = {\"%.*s\", %zu}, .type = &info_for_%.*s},\n",
                    (int)field.name.len, field.name.ptr, field.name.len,
                    (int)field_type_str.len, field_type_str.ptr);
        }

        fprintf(o, "};\n");
    }

    fprintf(o, "static const Type_Info info_for_%.*s = {\n", (int)name_id.len,
            name_id.ptr);

    fprintf(o, "    .name = {\"%.*s\", %zu},\n", (int)info->name.len,
            info->name.ptr, info->name.len);

    auto kind_str = type_kind_to_string(info->kind);
    fprintf(o, "    .kind = TYPE_%.*s,\n", (int)kind_str.len, kind_str.ptr);
    fprintf(o, "    .size = %d,\n", info->size);
    fprintf(o, "    .alignment = %d,\n", info->alignment);

    if (info->kind == TYPE_INTEGER) {
        fprintf(o, "    .as.integer = {.byte_width = %d, .is_signed = %s},\n",
                info->as.integer.byte_width,
                info->as.integer.is_signed ? "true" : "false");
    }

    else if (info->kind == TYPE_REAL) {
        fprintf(o, "    .as.real = {.byte_width = %d},\n",
                info->as.real.byte_width);
    }

    else if (info->kind == TYPE_POINTER) {
        auto inner          = info->as.pointer.inner;
        auto field_type_str = sanitize_name(&s->arena, inner->name);

        fprintf(o, "    .as.pointer = {.inner = &info_for_%.*s},\n",
                (int)field_type_str.len, field_type_str.ptr);
    }

    else if (info->kind == TYPE_RECORD) {
        fprintf(o,
                "    .as.record = {.field_count = %zu, .fields = "
                "fields_of_%.*s},\n",
                info->as.record.field_count, (int)name_id.len, name_id.ptr);
    }

    else if (info->kind == TYPE_UNION) {
        fprintf(o,
                "    .as.union_ = {.field_count = %zu, .fields = "
                "fields_of_%.*s},\n",
                info->as.union_.field_count, (int)name_id.len, name_id.ptr);
    }

    fprintf(o, "};\n");
    fprintf(o, "\n");
}

void generate_typeinfo(State *s, FILE *o) {
    Type_Array type_dependencies = {};
    defer { da_destroy(&type_dependencies); }

    for (size_t i = 0; i < s->symtable.len; ++i) {
        if (s->symtable.items[i].is_typedef) continue;

        add_type_to_deps(&type_dependencies, &s->symtable.items[i].type->info);
    }

    fprintf(o, "// AUTOGENERATED: DO NOT EDIT\n");
    fprintf(o, "#pragma once\n\n");

    fprintf(o, "#include \"typeinfo.h\"\n");
    fprintf(o, "\n");

    for (size_t i = 0; i < type_dependencies.len; ++i) {
        generate_function_proto_for(s, o, type_dependencies.items[i]);
        fprintf(o, ";\n");
    }

    fprintf(o, "\n");
    fprintf(o, "#define typeinfo_of(T) \\\n");
    fprintf(o, "    _Generic((T), \\\n");

    for (size_t i = 0; i < type_dependencies.len; ++i) {
        auto type = type_dependencies.items[i];
        if (type->kind == TYPE_INVALID) continue;

        auto type_name = sanitize_name(&s->arena, type->name);

        fprintf(o, "        %.*s: typeinfo_of_%.*s%c \\\n", (int)type->name.len,
                type->name.ptr, (int)type_name.len, type_name.ptr,
                i < type_dependencies.len - 1 ? ',' : ' ');
    }

    fprintf(o, "    )()\n");

    fprintf(o, "\n");
    fprintf(o, "#ifdef TYPEINFO_IMPLEMENTATION\n");

    fprintf(o, "// forward declarations (many types are self-referential)\n");
    for (size_t i = 0; i < type_dependencies.len; ++i) {
        auto name_id =
            sanitize_name(&s->arena, type_dependencies.items[i]->name);
        fprintf(o, "static const Type_Info info_for_%.*s;\n", (int)name_id.len,
                name_id.ptr);
    }

    fprintf(o, "\n");

    for (size_t i = 0; i < type_dependencies.len; ++i) {
        fprintf(o, "// Type info for: '%.*s'\n",
                (int)type_dependencies.items[i]->name.len,
                type_dependencies.items[i]->name.ptr);

        generate_typeinfo_for(s, o, type_dependencies.items[i]);
        generate_function_for(s, o, type_dependencies.items[i]);
    }

    fprintf(o, "#endif // TYPEINFO_IMPLEMENTATION\n");
    fprintf(o, "\n");
}

// ----------------------------------------------------------------------------

int main(int argc, char **argv) {
    Options options = parse_options(argc, argv);
    defer { options_destroy(&options); }

    if (options.flags & OPTION_VERBOSE_OPTIONS) options_dump(stderr, &options);

    auto index = clang_createIndex(0, 1);
    defer { clang_disposeIndex(index); }

    State state = {.opt = &options};
    defer { state_destroy(&state); }

    state_add_bultin_types(&state);

    for (size_t i = 0; i < options.input_files.len; ++i) {
        int r = parse_translation_unit(
            &state, index, options.input_files.items[i],
            options.clang_resource_dir, &options.extra_clang_args);
        if (r) return r;
    }

    if (options.flags & OPTION_PRINT_ALL_TYPES) {
        for (size_t i = 0; i < state.symtable.len; ++i) {
            auto type = &state.symtable.items[i].type->info;

            printf("found: '%.*s' (aka. '%.*s'): %s\n",
                   (int)state.symtable.items[i].name.len,
                   state.symtable.items[i].name.ptr, (int)type->name.len,
                   type->name.ptr, type_info_to_string(&state.arena, type));
            if (type->kind == TYPE_RECORD) {
                state.indent_level++;
                defer { state.indent_level--; }

                indent(state.indent_level);
                printf("where fields are:\n");

                state.indent_level++;
                defer { state.indent_level--; }
                for (size_t i = 0; i < type->as.record.field_count; ++i) {
                    auto field = type->as.record.fields[i];
                    indent(state.indent_level);
                    printf("%.*s: type=%s, offset=%zu\n", (int)field.name.len,
                           field.name.ptr,
                           type_info_to_string(&state.arena, field.type),
                           field.offset);
                }
            }
        }
    }

    {
        auto f = fopen(options.output_file.ptr, "wb");
        defer { fclose(f); }

        generate_typeinfo(&state, f);
    }

    return 0;
}
