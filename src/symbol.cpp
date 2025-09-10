#include "symbol.hpp"

#include <nlohmann/json.hpp>

#include "fmt/base.h"
#include "fmt/format.h"

namespace yal {
using nlohmann::json;

auto SymbolStore::new_sym(std::string_view name, Location name_loc, Value value,
                          bool is_local, bool is_const) -> Symbol* {
    auto d = sym_arena.create<Symbol>(
        Symbol{.name = sym_arena.alloc_string_view(name),
               .name_loc = name_loc,
               .value = value,
               .is_const = is_const,
               .is_local = is_local});
    all_syms.push_back(d);

    return d;
}

auto SymbolStore::dupe_string(std::string_view s) -> std::string_view {
    return string_arena.alloc_string_view(s);
}

void to_json(nlohmann::json& j, Value const& v) {
    j = json{
        {"type", v.type},
    };

    if (v.type.kind == ty::TypeKind::Type) {
        j["payload"] = v.as.type;
    }

    if ((v.type.kind == ty::TypeKind::Int ||
         v.type.kind == ty::TypeKind::ComptimeInt) &&
        v.as.integer.has_value) {
        // FIXME: handle different sizes and signness of integers correctly
        j["payload"] = v.as.integer.value;
    }
}

void to_json(nlohmann::json& j, Symbol const& d) {
    j = json{
        {"name_loc", fmt::to_string(d.name_loc)},
        {    "name",                     d.name},
        {   "value",                    d.value},
    };
}

}  // namespace yal

auto fmt::formatter<yal::Value>::format(yal::Value const& p,
                                        format_context&   ctx) const
    -> format_context ::iterator {
    fmt::format_to(ctx.out(), "(Value {}", p.type);
    if (p.type.kind == yal::ty::TypeKind::Type) {
        fmt::format_to(ctx.out(), " {}", p.as.type);
    }
    if ((p.type.kind == yal::ty::TypeKind::Int ||
         p.type.kind == yal::ty::TypeKind::ComptimeInt) &&
        p.as.integer.has_value) {
        // FIXME: handle different sizes and signness of integers correctly
        fmt::format_to(ctx.out(), " {}", p.as.integer.value);
    }

    return fmt::format_to(ctx.out(), ")");
}

auto fmt::formatter<yal::Symbol>::format(yal ::Symbol const& p,
                                         format_context&     ctx) const
    -> format_context ::iterator {
    fmt::format_to(ctx.out(), "(Symbol {:?}", p.name);

    if (p.is_local) fmt::format_to(ctx.out(), " local");
    if (p.is_const) fmt::format_to(ctx.out(), " const");

    return fmt::format_to(ctx.out(), " {})", p.value);
}
