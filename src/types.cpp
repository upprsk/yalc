#include "types.hpp"

#include "error_reporter.hpp"
#include "fmt/base.h"

namespace yal {
using fmt::format_to;

auto TypeStore::new_store() -> TypeStore {
    auto ts = TypeStore{};
    ts.err_type = ts.new_type(TypeKind::Err);
    ts.void_type = ts.new_type(TypeKind::Void);
    ts.type_type = ts.new_type(TypeKind::Type);

    ts.i64_type = ts.new_type(TypeKind::Int64);
    ts.u64_type = ts.new_type(TypeKind::Uint64);
    ts.i32_type = ts.new_type(TypeKind::Int32);
    ts.u32_type = ts.new_type(TypeKind::Uint32);
    ts.i16_type = ts.new_type(TypeKind::Int16);
    ts.u16_type = ts.new_type(TypeKind::Uint16);
    ts.i8_type = ts.new_type(TypeKind::Int8);
    ts.u8_type = ts.new_type(TypeKind::Uint8);

    ts.bool_type = ts.new_type(TypeKind::Bool);

    ts.usize_type = ts.new_type(TypeKind::Usize);
    ts.isize_type = ts.new_type(TypeKind::Isize);

    return ts;
}

auto TypeStore::dump(fmt::format_context& ctx, TypeHandle n) const
    -> fmt::format_context::iterator {
    if (!n.is_valid()) return format_to(ctx.out(), "{}", n);

    auto type = get(n);
    switch (type->kind) {
        case TypeKind::Err: return format_to(ctx.out(), "Err");
        case TypeKind::Type: return format_to(ctx.out(), "type");
        case TypeKind::Pack: {
            auto children = get_children(type);

            format_to(ctx.out(), "(");

            size_t i{};
            for (auto const& chld : children) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            return format_to(ctx.out(), ")");
        }
        case TypeKind::Void: return format_to(ctx.out(), "void");

        case TypeKind::Int64: return format_to(ctx.out(), "i64");
        case TypeKind::Uint64: return format_to(ctx.out(), "u64");
        case TypeKind::Int32: return format_to(ctx.out(), "i32");
        case TypeKind::Uint32: return format_to(ctx.out(), "u32");
        case TypeKind::Int16: return format_to(ctx.out(), "i16");
        case TypeKind::Uint16: return format_to(ctx.out(), "u16");
        case TypeKind::Int8: return format_to(ctx.out(), "i8");
        case TypeKind::Uint8: return format_to(ctx.out(), "u8");
        case TypeKind::Usize: return format_to(ctx.out(), "usize");
        case TypeKind::Isize: return format_to(ctx.out(), "isize");

        case TypeKind::Bool: return format_to(ctx.out(), "bool");

        case TypeKind::Array:
            format_to(ctx.out(), "[{}]{}", type->second.value(),
                      type->is_const() ? "const " : "");
            return dump(ctx, type->first);

        case TypeKind::Ptr:
            format_to(ctx.out(), "*");
            if (type->is_const()) format_to(ctx.out(), "const ");
            return dump(ctx, type->first);
        case TypeKind::MultiPtr:
            format_to(ctx.out(), "[*]");
            if (type->is_const()) format_to(ctx.out(), "const ");
            return dump(ctx, type->first);
        case TypeKind::Func: {
            auto f = type->as_func(*this);
            format_to(ctx.out(), "func(");

            size_t i{};
            for (auto const& chld : f.args) {
                if (i++ != 0) format_to(ctx.out(), ", ");
                dump(ctx, chld);
            }

            format_to(ctx.out(), ") ");
            return dump(ctx, f.ret);
        }
    }

    return format_to(ctx.out(), "unknown");
}

}  // namespace yal

auto fmt::formatter<yal::TypeHandle>::format(yal::TypeHandle n,
                                             format_context& ctx) const
    -> format_context::iterator {
    fmt::format_to(ctx.out(), "{{");

    if (!n.is_valid()) {
        fmt::format_to(ctx.out(), "!");
    }

    if (n.is_array()) {
        fmt::format_to(ctx.out(), "[{:x}]", n.as_idx());
    } else {
        fmt::format_to(ctx.out(), "{:x}", n.as_idx());
    }

    return fmt::format_to(ctx.out(), "}}");
}

auto fmt::formatter<yal::TypeKind>::format(yal::TypeKind   n,
                                           format_context& ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yal::TypeKind::Err: name = "Err"; break;
        case yal::TypeKind::Type: name = "Type"; break;
        case yal::TypeKind::Void: name = "Void"; break;
        case yal::TypeKind::Int64: name = "Int64"; break;
        case yal::TypeKind::Uint64: name = "Uint64"; break;
        case yal::TypeKind::Int32: name = "Int32"; break;
        case yal::TypeKind::Uint32: name = "Uint32"; break;
        case yal::TypeKind::Int16: name = "Int16"; break;
        case yal::TypeKind::Uint16: name = "Uint16"; break;
        case yal::TypeKind::Int8: name = "Int8"; break;
        case yal::TypeKind::Uint8: name = "Uint8"; break;
        case yal::TypeKind::Usize: name = "Usize"; break;
        case yal::TypeKind::Isize: name = "Isize"; break;
        case yal::TypeKind::Bool: name = "Bool"; break;
        case yal::TypeKind::Array: name = "Array"; break;
        case yal::TypeKind::Ptr: name = "Ptr"; break;
        case yal::TypeKind::MultiPtr: name = "MultiPtr"; break;
        case yal::TypeKind::Func: name = "Func"; break;
        case yal::TypeKind::Pack: name = "Pack"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::Type>::format(yal::Type t, format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}}}", t.kind, t.first,
                          t.second);
}

auto fmt::formatter<yal::FatTypeHandle>::format(yal::FatTypeHandle n,
                                                format_context&    ctx) const
    -> format_context::iterator {
    return n.ts->dump(ctx, n.type);
}
