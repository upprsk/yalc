#pragma once

// A High-Level Itermediate Representation.

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "types.hpp"

namespace yal::hlir {

struct Value {
    TypeHandle                                         type;
    std::variant<std::monostate, uint64_t, TypeHandle> value = std::monostate{};

    [[nodiscard]] constexpr auto value_type() const -> TypeHandle {
        return std::get<TypeHandle>(value);
    }

    [[nodiscard]] constexpr auto value_uint64() const -> uint64_t {
        return std::get<uint64_t>(value);
    }

    constexpr auto operator==(Value const&) const -> bool = default;
};

// NOTE: If we ever need more opcodes, we can use `a` and `b` of `Inst` as
// discriminators for instructions that don't need them.
enum class InstKind : uint8_t {
    Err,
    Const,
    LoadLocal,
    StoreLocal,

    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,

    Ret,

    // unconditionally jump to the block in `b`
    Jump,

    // conditionally jump to `a` (when condition is true) or `b` (when condition
    // is false).
    Branch,
};

struct Inst {
    InstKind kind = InstKind::Err;
    uint8_t  a = 0;
    uint8_t  b = 0;

    constexpr auto operator==(Inst const&) const -> bool = default;
};

struct Block {
    std::vector<Inst>  code;
    std::vector<Value> consts;
    std::vector<Span>  spans;
};

// idx is a uint8_t because that is the maximum an opcode can store for now.
struct Local {
    std::string name;
    TypeHandle  type;
    uint8_t     idx;

    constexpr auto operator==(Local const&) const -> bool = default;
};

struct Func {
    TypeHandle         type;
    std::string        name;
    std::vector<Block> blocks;
    std::vector<Local> locals;

    void disasm(FILE* f, TypeStore const& ts) const;
};

struct Module {
    std::vector<Func> funcs;
};

}  // namespace yal::hlir

template <>
struct fmt::formatter<yal::hlir::Value> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::hlir::Value n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::hlir::InstKind> : formatter<string_view> {
    auto format(yal::hlir::InstKind n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::hlir::Inst> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::hlir::Inst n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::hlir::Local> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::hlir::Local n, format_context& ctx) const
        -> format_context::iterator;
};
