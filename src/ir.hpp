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

enum class TypeKind : uint8_t {
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
    StrView,
    Array,
    Struct,
};

struct Type {
    TypeKind kind;

    [[nodiscard]] constexpr auto is_ptr() const -> bool {
        return kind == TypeKind::Ptr;
    }

    [[nodiscard]] constexpr auto is_strview() const -> bool {
        return kind == TypeKind::StrView;
    }

    [[nodiscard]] constexpr auto is_array() const -> bool {
        return kind == TypeKind::Array;
    }

    [[nodiscard]] constexpr auto is_struct() const -> bool {
        return kind == TypeKind::Struct;
    }

    [[nodiscard]] constexpr auto is_signed() const -> bool {
        bool r;
        switch (kind) {
            case TypeKind::Uint64:
            case TypeKind::Uint32:
            case TypeKind::Uint16:
            case TypeKind::Uint8:
            case TypeKind::Usize:
            case TypeKind::Ptr: r = false; break;

            case TypeKind::Int64:
            case TypeKind::Int32:
            case TypeKind::Int16:
            case TypeKind::Int8:
            case TypeKind::Isize: r = true; break;

            default: PANIC("invalid kind", kind);
        }

        return r;
    }

    [[nodiscard]] constexpr auto size() const -> size_t {
        size_t sz;
        switch (kind) {
            case TypeKind::Uint64: sz = sizeof(uint64_t); break;
            case TypeKind::Int64: sz = sizeof(int64_t); break;
            case TypeKind::Uint32: sz = sizeof(uint32_t); break;
            case TypeKind::Int32: sz = sizeof(int32_t); break;
            case TypeKind::Uint16: sz = sizeof(uint16_t); break;
            case TypeKind::Int16: sz = sizeof(int16_t); break;
            case TypeKind::Uint8: sz = sizeof(uint8_t); break;
            case TypeKind::Int8: sz = sizeof(int8_t); break;
            case TypeKind::Usize: sz = sizeof(uintptr_t); break;
            case TypeKind::Isize: sz = sizeof(uintptr_t); break;
            case TypeKind::Float32: sz = sizeof(float); break;
            case TypeKind::Float64: sz = sizeof(double); break;
            case TypeKind::Ptr: sz = sizeof(uintptr_t); break;
            default: PANIC("invalid kind", kind);
        }

        return sz;
    }

    [[nodiscard]] constexpr auto alignment() const -> size_t {
        size_t align;
        switch (kind) {
            case TypeKind::Uint64: align = alignof(uint64_t); break;
            case TypeKind::Int64: align = alignof(int64_t); break;
            case TypeKind::Uint32: align = alignof(uint32_t); break;
            case TypeKind::Int32: align = alignof(int32_t); break;
            case TypeKind::Uint16: align = alignof(uint16_t); break;
            case TypeKind::Int16: align = alignof(int16_t); break;
            case TypeKind::Uint8: align = alignof(uint8_t); break;
            case TypeKind::Int8: align = alignof(int8_t); break;
            case TypeKind::Usize: align = alignof(uintptr_t); break;
            case TypeKind::Isize: align = alignof(uintptr_t); break;
            case TypeKind::Float32: align = alignof(float); break;
            case TypeKind::Float64: align = alignof(double); break;
            case TypeKind::Ptr: align = alignof(uintptr_t); break;
            default: PANIC("invalid kind", kind);
        }

        return align;
    }

    constexpr auto operator==(Type const& o) const -> bool = default;
};

enum class OpCode : uint16_t {
    Err,
    IntConst,
    StrConst,
    Param,

    Copy,
    Alloca,

    Call,
    CallVoid,

    SetTmp,

    Load,
    Store,

    Ext,

    Add,
    Sub,
    Div,
    Mul,

    Eq,
    Neq,
    Lt,
    Le,
};

/// An instruction in the intermediate representation. It is meant to be heap
/// allocated in the module (internal arena).
///
/// - `op`: The opcode for the instuction. This says how to interpret the rest
/// of the data.
/// - `uid`: A unique identifier for the instruction. Used to show links between
/// instructions in textual form. And also on codegen.
/// - `type`: The concrete type the instruction operates on. This is not
/// intended to be used for safety, but mainly for size and alignment
/// calculations.
/// - `value`: An optional value attached to the instruction. This can be either
/// a string (allocated in the module storage) or a 64bit integer.
/// - `value_2`: Sometimes you need another value. For example, the `alloca`
/// instruction requires both the size an alignment. In this case, this stores
/// the alignment.
/// - `args`: arguments to the instruction. For instructions that require a
/// single argument, we use a pointer directly in the variant, otherwise a slice
/// allocated in the module storage.
struct Inst {
    OpCode                                                   op{};
    uint16_t                                                 value_2{};
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
    Jmp,
    Branch,
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
    struct TypeCache {
        Type* uint64;
        Type* int64;
        Type* uint32;
        Type* int32;
        Type* uint16;
        Type* int16;
        Type* uint8;
        Type* int8;

        Type* usize;
        Type* isize;

        Type* float32;
        Type* float64;

        Type* ptr;
    };

public:
    Module() {
        cache = {
            .uint64 = new_type_of(TypeKind::Uint64),
            .int64 = new_type_of(TypeKind::Int64),
            .uint32 = new_type_of(TypeKind::Uint32),
            .int32 = new_type_of(TypeKind::Int32),
            .uint16 = new_type_of(TypeKind::Uint16),
            .int16 = new_type_of(TypeKind::Int16),
            .uint8 = new_type_of(TypeKind::Uint8),
            .int8 = new_type_of(TypeKind::Int8),

            .usize = new_type_of(TypeKind::Usize),
            .isize = new_type_of(TypeKind::Isize),

            .float32 = new_type_of(TypeKind::Float32),
            .float64 = new_type_of(TypeKind::Float64),

            .ptr = new_type_of(TypeKind::Ptr),
        };
    }

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

    [[nodiscard]] auto new_inst_copy(Type* type, Inst* src) -> Inst* {
        return new_inst(OpCode::Copy, type, {}, src);
    }

    [[nodiscard]] auto new_inst_alloca(Type* type, uint16_t align, size_t size)
        -> Inst* {
        return new_inst(OpCode::Alloca, align, type, size, std::span<Inst*>{});
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

    [[nodiscard]] auto new_inst_settmp(Inst* lhs, Inst* rhs) -> Inst* {
        return new_inst(OpCode::SetTmp, nullptr, {}, std::array{lhs, rhs});
    }

    // NOTE: `type` should be the resulting type of the dereference
    [[nodiscard]] auto new_inst_load(Type* type, Inst* ptr) -> Inst* {
        return new_inst(OpCode::Load, type, {}, ptr);
    }

    // NOTE: `type` should be the resulting type of the dereference
    [[nodiscard]] auto new_inst_store(Inst* ptr, Inst* value) -> Inst* {
        return new_inst(OpCode::Store, nullptr, {}, std::array{ptr, value});
    }

    [[nodiscard]] auto new_inst_ext(Type* type, Inst* src) -> Inst* {
        return new_inst(OpCode::Ext, type, {}, src);
    }

    [[nodiscard]] auto new_inst_arith(OpCode op, Type* type, Inst* lhs,
                                      Inst* rhs) -> Inst* {
        return new_inst(op, type, {}, std::array{lhs, rhs});
    }

    [[nodiscard]] auto new_inst(
        OpCode op, Type* type,
        std::variant<std::monostate, uint64_t, std::string_view> value,
        std::span<Inst* const> args) -> Inst* {
        return insts.create<Inst>(op, 0, next_inst_uid++, type, value,
                                  new_inst_span(args));
    }

    [[nodiscard]] auto new_inst(
        OpCode op, Type* type,
        std::variant<std::monostate, uint64_t, std::string_view> value,
        Inst*                                                    arg) -> Inst* {
        return insts.create<Inst>(op, 0, next_inst_uid++, type, value, arg);
    }

    [[nodiscard]] auto new_inst(
        OpCode op, uint16_t value_2, Type* type,
        std::variant<std::monostate, uint64_t, std::string_view> value,
        std::span<Inst* const> args) -> Inst* {
        return insts.create<Inst>(op, value_2, next_inst_uid++, type, value,
                                  new_inst_span(args));
    }

    [[nodiscard]] auto new_inst(
        OpCode op, uint16_t value_2, Type* type,
        std::variant<std::monostate, uint64_t, std::string_view> value,
        Inst*                                                    arg) -> Inst* {
        return insts.create<Inst>(op, value_2, next_inst_uid++, type, value,
                                  arg);
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

    [[nodiscard]] auto new_type_of(TypeKind kind) -> Type* {
        return types.create<Type>(kind);
    }

    [[nodiscard]] auto new_type_span(std::span<Type* const> args)
        -> std::span<Type*> {
        return types.alloc<Type*>(args);
    }

    [[nodiscard]] auto new_type_span(size_t sz) -> std::span<Type*> {
        return types.alloc_size<Type*>(sz);
    }

    [[nodiscard]] constexpr auto get_type_uint64() const -> Type* {
        return cache.uint64;
    }

    [[nodiscard]] constexpr auto get_type_int64() const -> Type* {
        return cache.int64;
    }

    [[nodiscard]] constexpr auto get_type_uint32() const -> Type* {
        return cache.uint32;
    }

    [[nodiscard]] constexpr auto get_type_int32() const -> Type* {
        return cache.int32;
    }

    [[nodiscard]] constexpr auto get_type_uint16() const -> Type* {
        return cache.uint16;
    }

    [[nodiscard]] constexpr auto get_type_int16() const -> Type* {
        return cache.int16;
    }

    [[nodiscard]] constexpr auto get_type_uint8() const -> Type* {
        return cache.uint8;
    }

    [[nodiscard]] constexpr auto get_type_int8() const -> Type* {
        return cache.int8;
    }

    [[nodiscard]] constexpr auto get_type_usize() const -> Type* {
        return cache.usize;
    }

    [[nodiscard]] constexpr auto get_type_isize() const -> Type* {
        return cache.isize;
    }

    [[nodiscard]] constexpr auto get_type_float32() const -> Type* {
        return cache.float32;
    }

    [[nodiscard]] constexpr auto get_type_float64() const -> Type* {
        return cache.float64;
    }

    [[nodiscard]] constexpr auto get_type_ptr() const -> Type* {
        return cache.ptr;
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

    void reset_inst_uid_counter(uint32_t v = 0) { next_inst_uid = v; }
    void reset_block_uid_counter(uint32_t v = 0) { next_block_uid = v; }

private:
    mem::Arena insts;
    mem::Arena blocks;
    mem::Arena types;
    mem::Arena string_buffer;

    std::vector<Func>             funcs;
    std::vector<std::string_view> strings;

    uint32_t next_inst_uid{};
    uint32_t next_block_uid{};

    TypeCache cache{};
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
        case TypeKind::StrView: name = "StrView"; break;
        case TypeKind::Array: name = "Array"; break;
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
        case BlockOp::Jmp: name = "Jmp"; break;
        case BlockOp::Branch: name = "Branch"; break;
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
        case OpCode::Copy: name = "Copy"; break;
        case OpCode::Alloca: name = "Alloca"; break;
        case OpCode::Call: name = "Call"; break;
        case OpCode::CallVoid: name = "CallVoid"; break;
        case OpCode::SetTmp: name = "SetTmp"; break;
        case OpCode::Load: name = "Load"; break;
        case OpCode::Store: name = "Store"; break;
        case OpCode::Ext: name = "Ext"; break;
        case OpCode::Add: name = "Add"; break;
        case OpCode::Sub: name = "Sub"; break;
        case OpCode::Div: name = "Div"; break;
        case OpCode::Mul: name = "Mul"; break;
        case OpCode::Eq: name = "Eq"; break;
        case OpCode::Neq: name = "Neq"; break;
        case OpCode::Lt: name = "Lt"; break;
        case OpCode::Le: name = "Le"; break;
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
