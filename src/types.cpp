#include "types.hpp"

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <nlohmann/json.hpp>

#include "symbol.hpp"

namespace yal::ty {
using nlohmann::json;

void to_json(nlohmann::json& j, TypeKind const& n) { j = fmt::to_string(n); }

void to_json(nlohmann::json& j, TypeFlags const& n) {
    j = json::array();
    if (n.value & TypeFlags::Const) j.push_back("const");
}

void to_json(nlohmann::json& j, TypeStructField const& n) {
    j = json{
        {"name", n.name},
        {"type", n.type},
    };
}

void to_json(nlohmann::json& j, Type const& n) {
    j = json{
        { "kind",  n.kind},
        {"flags", n.flags},
    };

    switch (n.kind) {
        case TypeKind::Err:
        case TypeKind::PendingCast:
        case TypeKind::Void:
        case TypeKind::Type: break;

        case TypeKind::Int:
            j["byte_size"] = n.as.integer.byte_size;
            j["is_signed"] = n.as.integer.is_signed;
            break;
        case TypeKind::ComptimeInt:
        case TypeKind::Bool: break;

        case TypeKind::Ptr:
        case TypeKind::MultiPtr:
        case TypeKind::Slice: j["inner"] = n.as.ptr->inner; break;

        case TypeKind::Array:
            j["count"] = n.as.array->count;
            j["inner"] = n.as.array->inner;
            break;

        case TypeKind::Struct: j["fields"] = n.as.st->fields; break;

        case TypeKind::Func:
            j["params"] = n.as.func->params;
            j["rets"] = n.as.func->rets;
            break;

        case TypeKind::Tuple: j["items"] = n.as.tuple->items; break;
    }
}

void to_repr(fmt::format_context& ctx, Type const* type,
             TypeReprOptions const& opts) {
    if (type) {
        to_repr(ctx, *type, opts);
    } else {
        fmt::format_to(ctx.out(), "#nullptr#");
    }
}

void to_repr(fmt::format_context& ctx, Type const& type,
             TypeReprOptions const& opts) {
    if (type.sym) fmt::format_to(ctx.out(), "{} (", type.sym->name);

    switch (type.kind) {
        case TypeKind::Err: fmt::format_to(ctx.out(), "#error#"); break;
        case TypeKind::PendingCast:
            fmt::format_to(ctx.out(), "#pending-cast#");
            break;
        case TypeKind::Void: fmt::format_to(ctx.out(), "void"); break;
        case TypeKind::Type: fmt::format_to(ctx.out(), "type"); break;

        case TypeKind::Int:
            fmt::format_to(ctx.out(), "{}{}",
                           type.as.integer.is_signed ? "s" : "u",
                           type.as.integer.byte_size * 8);
            break;
        case TypeKind::Bool: fmt::format_to(ctx.out(), "bool"); break;

        case TypeKind::ComptimeInt:
            fmt::format_to(ctx.out(), "comptime_int");
            break;

        case TypeKind::Ptr:
            fmt::format_to(ctx.out(), "*{}",
                           type.flags.is_const() ? "const " : "");
            to_repr(ctx, type.as.ptr->inner, opts);
            break;
        case TypeKind::MultiPtr:
            fmt::format_to(ctx.out(), "[*]{}",
                           type.flags.is_const() ? "const " : "");
            to_repr(ctx, type.as.ptr->inner, opts);
            break;
        case TypeKind::Slice:
            fmt::format_to(ctx.out(), "[]{}",
                           type.flags.is_const() ? "const " : "");
            to_repr(ctx, type.as.ptr->inner, opts);
            break;

        case TypeKind::Array:
            fmt::format_to(ctx.out(), "[{}]{}", type.as.array->count,
                           type.flags.is_const() ? "const " : "");
            to_repr(ctx, type.as.array->inner, opts);
            break;

        case TypeKind::Struct:
            fmt::format_to(ctx.out(), "struct {{");

            if (opts.short_structs) {
                fmt::format_to(ctx.out(), " ... ");
            } else {
                for (auto const& [idx, field] :
                     std::views::enumerate(type.as.st->fields)) {
                    fmt::format_to(ctx.out(), "{}: ", field.name);
                    to_repr(ctx, field.type, opts.with_short_structs());
                    if (static_cast<size_t>(idx) !=
                        type.as.st->fields.size() - 1)
                        // NOTE: this is not a semicolon because that breakes
                        // lisp syntax highlighting
                        fmt::format_to(ctx.out(), ", ");
                }
            }

            fmt::format_to(ctx.out(), "}}");
            break;

        case TypeKind::Func:
            fmt::format_to(ctx.out(), "func({})",
                           fmt::join(type.as.func->params, ", "));
            if (type.as.func->rets.empty()) {
                // nothing
            } else if (type.as.func->rets.size() == 1) {
                fmt::format_to(ctx.out(), " {}", type.as.func->rets[0]);
            } else {
                fmt::format_to(ctx.out(), " ({})",
                               fmt::join(type.as.func->rets, ", "));
            }
            break;
        case TypeKind::Tuple:
            fmt::format_to(ctx.out(), "({})",
                           fmt::join(type.as.tuple->items, ", "));
            break;
    }

    if (type.sym) fmt::format_to(ctx.out(), ")");
}

}  // namespace yal::ty

auto fmt::formatter<yal::ty::TypeKind>::format(yal::ty::TypeKind const& p,
                                               format_context& ctx) const
    -> format_context ::iterator {
    std::string_view name = "???";
    switch (p) {
        case yal::ty::TypeKind::Err: name = "Err"; break;
        case yal::ty::TypeKind::PendingCast: name = "PendingCast"; break;
        case yal::ty::TypeKind::Void: name = "Void"; break;
        case yal::ty::TypeKind::Type: name = "Type"; break;
        case yal::ty::TypeKind::Int: name = "Int"; break;
        case yal::ty::TypeKind::ComptimeInt: name = "ComptimeInt"; break;
        case yal::ty::TypeKind::Bool: name = "Bool"; break;
        case yal::ty::TypeKind::Ptr: name = "Ptr"; break;
        case yal::ty::TypeKind::MultiPtr: name = "MultiPtr"; break;
        case yal::ty::TypeKind::Slice: name = "Slice"; break;
        case yal::ty::TypeKind::Array: name = "Array"; break;
        case yal::ty::TypeKind::Struct: name = "Struct"; break;
        case yal::ty::TypeKind::Func: name = "Func"; break;
        case yal::ty::TypeKind::Tuple: name = "Tuple"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::ty::TypeFlags>::format(yal::ty::TypeFlags const& p,
                                                format_context& ctx) const
    -> format_context ::iterator {
    fmt::format_to(ctx.out(), "Flags(");

    if (p.is_const()) fmt::format_to(ctx.out(), "Const");

    return fmt::format_to(ctx.out(), ")");
}

auto fmt::formatter<yal::ty::Type>::format(yal::ty::Type const& p,
                                           format_context&      ctx) const
    -> format_context ::iterator {
    yal::ty::to_repr(ctx, p);
    return ctx.out();
}
