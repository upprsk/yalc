#pragma once

#include <ankerl/unordered_dense.h>

#include <flat_map>
#include <string_view>

#include "arena.hpp"
#include "location.hpp"
#include "macros.hpp"
#include "types.hpp"

namespace yal {

namespace ast {
struct FuncDecl;
}

struct Value {
    ty::Type type;
    union {
        ty::Type       type;
        ast::FuncDecl* func_decl;
    } as;
};

struct Symbol {
    std::string_view name;
    Location         name_loc;
    Value            value;
};

class SymbolStore {
    std::vector<Symbol*> all_syms;

    mem::Arena string_arena;
    mem::Arena sym_arena;

public:
    SymbolStore() = default;

    auto new_sym(std::string_view name, Location name_loc, Value value)
        -> Symbol*;

    // ========================================================================

    struct Iter {
        std::vector<Symbol*>::const_iterator begin;
        std::vector<Symbol*>::const_iterator end;
    };

    auto iter() -> Iter {
        return {.begin = all_syms.cbegin(), .end = all_syms.cend()};
    }

private:
    auto dupe_string(std::string_view s) -> std::string_view;
};

void to_json(nlohmann::json& j, Value const& v);
void to_json(nlohmann::json& j, Symbol const& d);

}  // namespace yal

define_formatter_from_string_view(yal::Value);
define_formatter_from_string_view(yal::Symbol);
