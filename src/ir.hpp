#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "arena.hpp"
#include "file-store.hpp"

namespace yal::ir {

enum class TypeKind {
    Err,

    Uint64,
    Int64,
    Uint32,
    Int32,
    Uint16,
    Int16,
    Uint8,
    Int8,

    Usize,
    Isize,

    Float32,
    Float64,

    Ptr,
    Struct,
};

struct Type {
    TypeKind kind;
};

enum class OpCode {
    Err,
    IntConst,
    StrConst,
    Param,

    Call,
    CallVoid,

    GetLocal,

    Add,
    Sub,
    Div,
    Mul,
};

struct Inst {
    OpCode                                                   op{};
    uint32_t                                                 uid{};
    Type*                                                    type{};
    std::variant<std::monostate, uint64_t, std::string_view> value;
    std::variant<Inst*, std::span<Inst*>>                    args;

    [[nodiscard]] constexpr auto has_value() const -> bool {
        return !std::holds_alternative<std::monostate>(value);
    }

    [[nodiscard]] constexpr auto has_u64() const -> bool {
        return std::holds_alternative<uint64_t>(value);
    }

    [[nodiscard]] constexpr auto has_str() const -> bool {
        return std::holds_alternative<std::string_view>(value);
    }

    [[nodiscard]] constexpr auto get_value_u64() const -> uint64_t {
        return std::get<uint64_t>(value);
    }

    [[nodiscard]] constexpr auto get_value_str() const -> std::string_view {
        return std::get<std::string_view>(value);
    }

    [[nodiscard]] constexpr auto get_arg() const -> Inst* {
        return std::get<Inst*>(args);
    }

    [[nodiscard]] constexpr auto get_args() -> std::span<Inst*> {
        if (std::holds_alternative<Inst*>(args))
            return {&std::get<Inst*>(args), 1};

        return std::get<std::span<Inst*>>(args);
    }

    [[nodiscard]] constexpr auto get_args() const -> std::span<Inst* const> {
        if (std::holds_alternative<Inst*>(args))
            return {&std::get<Inst*>(args), 1};

        return std::get<std::span<Inst*>>(args);
    }

    [[nodiscard]] constexpr auto get_arg(size_t idx) const -> Inst* {
        return get_args()[idx];
    }
};

enum class BlockOp {
    Err,
    Ret,
    RetVoid,
};

struct Block {
    BlockOp           op{};
    uint32_t          uid{};
    Inst*             value{};  // just for returns
    std::span<Inst*>  body;
    std::span<Block*> next;
};

struct Func {
    enum Flags { FlagNone = 0, FlagExport = 1 << 0, FlagExtern = 1 << 1 };

    std::string_view link_name;

    std::span<Type*> params;
    std::span<Inst*> param_insts;
    // May be null when return type is void. In case of multiple returns, this
    // should become a tuple/struct.
    Type* ret{};

    Block*            body;
    std::span<Block*> all_blocks;

    Location loc;

    Flags flags;

    [[nodiscard]] constexpr auto is_export() const -> bool {
        return flags & FlagExport;
    }

    [[nodiscard]] constexpr auto is_extern() const -> bool {
        return flags & FlagExtern;
    }
};

struct Module {
public:
    [[nodiscard]] auto new_inst_int_const(Type* type, uint64_t value) -> Inst* {
        return new_inst(OpCode::IntConst, type, value, std::span<Inst*>{});
    }

    [[nodiscard]] auto new_inst_str_const(Type*            type,
                                          std::string_view raw_value) -> Inst* {
        auto value = add_string(raw_value);
        return new_inst(OpCode::StrConst, type, value, std::span<Inst*>{});
    }

    [[nodiscard]] auto new_inst_param(Type* type) -> Inst* {
        return new_inst(OpCode::Param, type, {}, std::span<Inst*>{});
    }

    [[nodiscard]] auto new_inst_call(Type* type, std::string_view symbol,
                                     std::span<Inst* const> args) -> Inst* {
        return new_inst(OpCode::Call, type, symbol, args);
    }

    [[nodiscard]] auto new_inst_call_void(std::string_view       symbol,
                                          std::span<Inst* const> args)
        -> Inst* {
        return new_inst(OpCode::CallVoid, nullptr, symbol, args);
    }

    [[nodiscard]] auto new_inst_get_local(Type* type, Inst* local) -> Inst* {
        return new_inst(OpCode::GetLocal, type, {}, local);
    }

    [[nodiscard]] auto new_inst_arith(OpCode op, Type* type, Inst* lhs,
                                      Inst* rhs) -> Inst* {
        return new_inst(op, type, {}, std::array{lhs, rhs});
    }

    [[nodiscard]] auto new_inst(
        OpCode op, Type* type,
        std::variant<std::monostate, uint64_t, std::string_view> value,
        std::span<Inst* const> args) -> Inst* {
        return insts.create<Inst>(op, next_inst_uid++, type, value,
                                  new_inst_span(args));
    }

    [[nodiscard]] auto new_inst(
        OpCode op, Type* type,
        std::variant<std::monostate, uint64_t, std::string_view> value,
        Inst*                                                    arg) -> Inst* {
        return insts.create<Inst>(op, next_inst_uid++, type, value, arg);
    }

    [[nodiscard]] auto new_inst_span(std::span<Inst* const> args)
        -> std::span<Inst*> {
        return insts.alloc<Inst*>(args);
    }

    [[nodiscard]] auto new_block(BlockOp op, Inst* value,
                                 std::span<Inst* const>  body,
                                 std::span<Block* const> next) -> Block* {
        return blocks.create<Block>(op, next_block_uid++, value,
                                    new_inst_span(body), new_block_span(next));
    }

    [[nodiscard]] auto new_block_span(std::span<Block* const> next)
        -> std::span<Block*> {
        return blocks.alloc<Block*>(next);
    }

    [[nodiscard]] auto new_type(TypeKind kind) -> Type* {
        return types.create<Type>(kind);
    }

    [[nodiscard]] auto new_type_span(std::span<Type* const> args)
        -> std::span<Type*> {
        return types.alloc<Type*>(args);
    }

    [[nodiscard]] auto new_type_span(size_t sz) -> std::span<Type*> {
        return types.alloc_size<Type*>(sz);
    }

    // ------------------------------------------------------------------------

    auto add_string(std::string_view str) -> size_t {
        auto s = string_buffer.alloc_string_view(str);
        auto id = strings.size();
        strings.push_back(s);

        return id;
    }

    [[nodiscard]] auto get_string(size_t id) const -> std::string_view {
        return strings.at(id);
    }

    [[nodiscard]] auto get_strings() const
        -> std::span<std::string_view const> {
        return strings;
    }

    void add_func(Func const& fn) { funcs.push_back(fn); }

    [[nodiscard]] auto get_funcs() const -> std::span<Func const> {
        return funcs;
    }

private:
    mem::Arena insts;
    mem::Arena blocks;
    mem::Arena types;
    mem::Arena string_buffer;

    std::vector<Func>             funcs;
    std::vector<std::string_view> strings;

    uint32_t next_inst_uid{};
    uint32_t next_block_uid{};
};

constexpr auto format_as(TypeKind kind) {
    std::string_view name;

    switch (kind) {
        case TypeKind::Err: name = "Err"; break;
        case TypeKind::Uint64: name = "Uint64"; break;
        case TypeKind::Int64: name = "Int64"; break;
        case TypeKind::Uint32: name = "Uint32"; break;
        case TypeKind::Int32: name = "Int32"; break;
        case TypeKind::Uint16: name = "Uint16"; break;
        case TypeKind::Int16: name = "Int16"; break;
        case TypeKind::Uint8: name = "Uint8"; break;
        case TypeKind::Int8: name = "Int8"; break;
        case TypeKind::Usize: name = "Usize"; break;
        case TypeKind::Isize: name = "Isize"; break;
        case TypeKind::Float32: name = "Float32"; break;
        case TypeKind::Float64: name = "Float64"; break;
        case TypeKind::Ptr: name = "Ptr"; break;
        case TypeKind::Struct: name = "Struct"; break;
    }

    return name;
}

constexpr auto format_as(BlockOp op) {
    std::string_view name;

    switch (op) {
        case BlockOp::Err: name = "Err"; break;
        case BlockOp::Ret: name = "Ret"; break;
        case BlockOp::RetVoid: name = "RetVoid"; break;
    }

    return name;
}

constexpr auto format_as(OpCode op) {
    std::string_view name;

    switch (op) {
        case OpCode::Err: name = "Err"; break;
        case OpCode::IntConst: name = "IntConst"; break;
        case OpCode::StrConst: name = "StrConst"; break;
        case OpCode::Param: name = "Param"; break;
        case OpCode::Call: name = "Call"; break;
        case OpCode::CallVoid: name = "CallVoid"; break;
        case OpCode::GetLocal: name = "GetLocal"; break;
        case OpCode::Add: name = "Add"; break;
        case OpCode::Sub: name = "Sub"; break;
        case OpCode::Div: name = "Div"; break;
        case OpCode::Mul: name = "Mul"; break;
    }

    return name;
}

void disasm_func(FILE* out, Func const& fn);
void disasm_module(FILE* out, Module const& mod);

}  // namespace yal::ir

template <>
struct fmt::formatter<yal::ir::Type> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ir::Type const& n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::ir::Inst> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ir::Inst const& inst, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::ir::Block> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ir::Block const& block, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::ir::Func> : formatter<string_view> {
    // parse is inherited from formatter<string_view>.

    auto format(yal::ir::Func const& fn, format_context& ctx) const
        -> format_context::iterator;
};
