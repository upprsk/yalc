#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

#include "error_reporter.hpp"
#include "fmt/format.h"
#include "libassert/assert.hpp"

namespace yal {

/// A handle to a type. 29 bits are used as index. Bit 31 is used as a check for
/// if the handle is valid. Bit 30 is used as a check for if the handle points
/// to an array. Bit 29 is used as a check for if the handle is a length pair. A
/// length pair is a pair of 10bit integer sizes.
struct TypeHandle {
    constexpr TypeHandle(uint32_t idx = 0xFFFF'FFFF) : idx{idx} {}
    constexpr TypeHandle(uint64_t) = delete;
    constexpr auto operator=(TypeHandle const&) -> TypeHandle& = default;
    constexpr auto operator=(TypeHandle&&) -> TypeHandle& = default;
    constexpr TypeHandle(TypeHandle&& o) = default;
    constexpr TypeHandle(TypeHandle const& o) = default;

    static constexpr auto from_size(size_t idx) -> TypeHandle {
        return TypeHandle{static_cast<uint32_t>(idx)};
    }

    static constexpr auto from_pair(size_t a, size_t b) -> TypeHandle {
        return TypeHandle{static_cast<uint32_t>(a & 0x3FF) |
                          static_cast<uint32_t>((b << 10) & 0x3FF)};
    }

    [[nodiscard]] constexpr auto as_idx() const -> uint32_t {
        return idx & 0x1FFF'FFFF;
    }

    [[nodiscard]] constexpr auto as_count() const -> uint32_t {
        return as_idx();
    }

    [[nodiscard]] constexpr auto as_first_pair() const -> uint32_t {
        return idx & 0x3FF;
    }

    [[nodiscard]] constexpr auto as_second_pair() const -> uint32_t {
        return (idx >> 10) & 0x3FF;
    }

    [[nodiscard]] constexpr auto as_pair() const
        -> std::pair<uint32_t, uint32_t> {
        return {as_first_pair(), as_second_pair()};
    }

    // check that this handle has not been invalidated
    [[nodiscard]] constexpr auto is_valid() const -> bool {
        return (idx & (1 << 31)) == 0;
    }

    // check that this handle refers to an array
    [[nodiscard]] constexpr auto is_array() const -> bool {
        return idx & (1 << 30);
    }

    // check that this handle refers to an array
    [[nodiscard]] constexpr auto is_pair() const -> bool {
        return idx & (1 << 29);
    }

    // return a copy of the handle marked as invalid
    [[nodiscard]] constexpr auto to_invalid() const -> TypeHandle {
        return TypeHandle{idx | (1 << 31)};
    }

    // return a copy of the handle marked as an array
    [[nodiscard]] constexpr auto to_array() const -> TypeHandle {
        return TypeHandle{idx | (1 << 30)};
    }

    // return a copy of the handle marked as a pair
    [[nodiscard]] constexpr auto to_pair() const -> TypeHandle {
        return TypeHandle{idx | (1 << 29)};
    }

    [[nodiscard]] constexpr auto value() const -> uint32_t { return idx; }

    constexpr auto operator==(TypeHandle const&) const -> bool = default;

private:
    uint32_t idx;
};

enum class TypeKind : uint16_t {
    Err,
    Type,
    Pack,  // ordered type pack, for function returns
    Void,
    Int64,
    Uint64,
    Int32,
    Uint32,
    Int16,
    Uint16,
    Int8,
    Uint8,
    Usize,
    Isize,
    Bool,
    Array,
    Ptr,
    MultiPtr,
    Func,
};

enum class TypeFlags : uint16_t {
    None = 0,
    IsConst = (1 << 0),
};

constexpr auto operator&(TypeFlags const& lhs, TypeFlags const& rhs)
    -> TypeFlags {
    return static_cast<TypeFlags>(fmt::underlying(lhs) & fmt::underlying(rhs));
}

constexpr auto operator|(TypeFlags const& lhs, TypeFlags const& rhs)
    -> TypeFlags {
    return static_cast<TypeFlags>(fmt::underlying(lhs) | fmt::underlying(rhs));
}

constexpr auto operator|=(TypeFlags& lhs, TypeFlags const& rhs) -> TypeFlags {
    return lhs = static_cast<TypeFlags>(fmt::underlying(lhs) |
                                        fmt::underlying(rhs));
}

struct TypeStore;

struct Type {
    TypeKind   kind = TypeKind::Err;
    TypeFlags  flags = TypeFlags::None;
    TypeHandle first;
    TypeHandle second;

    [[nodiscard]] constexpr auto children() const -> TypeHandle {
        return first;
    }

    [[nodiscard]] constexpr auto count() const -> uint32_t {
        return second.as_count();
    }

    [[nodiscard]] constexpr auto is_err() const -> bool {
        return kind == TypeKind::Err;
    }

    [[nodiscard]] constexpr auto is_bool() const -> bool {
        return kind == TypeKind::Bool;
    }

    [[nodiscard]] constexpr auto is_integral() const -> bool {
        return kind == TypeKind::Int64 || kind == TypeKind::Uint64 ||
               kind == TypeKind::Int32 || kind == TypeKind::Uint32 ||
               kind == TypeKind::Int16 || kind == TypeKind::Uint16 ||
               kind == TypeKind::Int8 || kind == TypeKind::Uint8 ||
               kind == TypeKind::Usize || kind == TypeKind::Isize;
    }

    [[nodiscard]] constexpr auto is_signed() const -> bool {
        return kind == TypeKind::Int64 || kind == TypeKind::Int32 ||
               kind == TypeKind::Int16 || kind == TypeKind::Int8 ||
               kind == TypeKind::Isize;
    }

    [[nodiscard]] constexpr auto is_unsigned() const -> bool {
        return kind == TypeKind::Uint64 || kind == TypeKind::Uint32 ||
               kind == TypeKind::Uint16 || kind == TypeKind::Uint8 ||
               kind == TypeKind::Usize;
    }

    [[nodiscard]] constexpr auto is_ptr() const -> bool {
        return kind == TypeKind::Ptr || kind == TypeKind::MultiPtr;
    }

    [[nodiscard]] constexpr auto is_array() const -> bool {
        return kind == TypeKind::Array;
    }

    [[nodiscard]] constexpr auto is_void() const -> bool {
        return kind == TypeKind::Void;
    }

    [[nodiscard]] constexpr auto is_type() const -> bool {
        return kind == TypeKind::Type;
    }

    [[nodiscard]] constexpr auto is_func() const -> bool {
        return kind == TypeKind::Func;
    }

    [[nodiscard]] constexpr auto is_pack() const -> bool {
        return kind == TypeKind::Pack;
    }

    [[nodiscard]] constexpr auto size(TypeStore const& ts) const -> size_t;

    [[nodiscard]] constexpr auto is_const() const -> bool {
        return (flags & TypeFlags::IsConst) != TypeFlags::None;
    }

    constexpr auto operator==(Type const&) const -> bool = default;

    struct Func {
        TypeHandle                  ret;
        std::span<TypeHandle const> args;
    };

    struct Array {
        TypeHandle inner;
        uint32_t   length;
    };
};

struct FatTypeHandle {
    TypeStore const* ts;
    TypeHandle       type;
};

struct TypeStore {
    static auto new_store() -> TypeStore;

    [[nodiscard]] auto type_as_func(TypeHandle h) const -> Type::Func;
    [[nodiscard]] auto type_as_array(TypeHandle h) const -> Type::Array;

    [[nodiscard]] auto get_type_err() const -> TypeHandle { return err_type; }
    [[nodiscard]] auto get_type_type() const -> TypeHandle { return type_type; }
    [[nodiscard]] auto get_type_void() const -> TypeHandle { return void_type; }

    [[nodiscard]] auto get_type_i64() const -> TypeHandle { return i64_type; }
    [[nodiscard]] auto get_type_u64() const -> TypeHandle { return u64_type; }
    [[nodiscard]] auto get_type_i32() const -> TypeHandle { return i32_type; }
    [[nodiscard]] auto get_type_u32() const -> TypeHandle { return u32_type; }
    [[nodiscard]] auto get_type_i16() const -> TypeHandle { return i16_type; }
    [[nodiscard]] auto get_type_u16() const -> TypeHandle { return u16_type; }
    [[nodiscard]] auto get_type_i8() const -> TypeHandle { return i8_type; }
    [[nodiscard]] auto get_type_u8() const -> TypeHandle { return u8_type; }
    [[nodiscard]] auto get_type_bool() const -> TypeHandle { return bool_type; }

    [[nodiscard]] auto get_type_usize() const -> TypeHandle {
        return usize_type;
    }

    [[nodiscard]] auto get_type_isize() const -> TypeHandle {
        return isize_type;
    }

    [[nodiscard]] auto get_type_array(TypeHandle inner, uint32_t size,
                                      TypeFlags flags) -> TypeHandle {
        auto t = find_type(Type{.kind = TypeKind::Array,
                                .flags = flags,
                                .first = inner,
                                .second = size});
        if (t.is_valid()) return t;

        return new_type(TypeKind::Array, flags, inner, size);
    }

    [[nodiscard]] auto get_type_ptr(TypeHandle child, TypeFlags flags)
        -> TypeHandle {
        auto t = find_type(Type{.kind = TypeKind::Ptr,
                                .flags = flags,
                                .first = child,
                                .second = {}});
        if (t.is_valid()) return t;

        return new_type(TypeKind::Ptr, flags, child);
    }

    [[nodiscard]] auto get_type_multi_ptr(TypeHandle child, TypeFlags flags)
        -> TypeHandle {
        auto t = find_type(Type{.kind = TypeKind::MultiPtr,
                                .flags = flags,
                                .first = child,
                                .second = {}});
        if (t.is_valid()) return t;

        return new_type(TypeKind::MultiPtr, flags, child);
    }

    [[nodiscard]] auto get_type_func(std::span<TypeHandle const> args,
                                     TypeHandle ret) -> TypeHandle {
        auto t = find_type_fn(args, ret);
        if (t.is_valid()) return t;

        return new_type(TypeKind::Func, TypeFlags::None,
                        new_array_plus_one(ret, args),
                        TypeHandle::from_size(args.size() + 1));
    }

    [[nodiscard]] auto get_type_pack(std::span<TypeHandle const> items)
        -> TypeHandle {
        auto t = find_type_pack(items);
        if (t.is_valid()) return t;

        return new_type(TypeKind::Pack, TypeFlags::None, new_array(items),
                        TypeHandle::from_size(items.size()));
    }

    [[nodiscard]] auto new_type(auto&&... args) -> TypeHandle {
        auto sz = types.size();
        types.emplace_back(std::forward<decltype(args)>(args)...);

        return TypeHandle::from_size(sz);
    }

    [[nodiscard]] auto new_array(std::span<TypeHandle const> handles)
        -> TypeHandle {
        auto sz = type_refs.size();
        type_refs.insert(type_refs.end(), handles.begin(), handles.end());

        return TypeHandle::from_size(sz).to_array();
    }

    [[nodiscard]] auto new_array(std::span<TypeHandle const> handles,
                                 std::span<TypeHandle const> others)
        -> TypeHandle {
        auto sz = type_refs.size();
        type_refs.insert(type_refs.end(), handles.begin(), handles.end());
        type_refs.insert(type_refs.end(), others.begin(), others.end());

        return TypeHandle::from_size(sz).to_array();
    }

    [[nodiscard]] auto new_array_plus_one(TypeHandle                  first,
                                          std::span<TypeHandle const> handles)
        -> TypeHandle {
        auto sz = type_refs.size();
        type_refs.push_back(first);
        type_refs.insert(type_refs.end(), handles.begin(), handles.end());

        return TypeHandle::from_size(sz).to_array();
    }

    [[nodiscard]] auto find_type_fn(std::span<TypeHandle const> args,
                                    TypeHandle ret) const -> TypeHandle {
        size_t i{};

        for (auto const& t : types) {
            if (t.is_func()) {
                auto f = type_as_func(TypeHandle::from_size(i));

                if (std::ranges::equal(args, f.args) && f.ret == ret)
                    return TypeHandle::from_size(i);
            }

            i++;
        }

        return TypeHandle{}.to_invalid();
    }

    [[nodiscard]] auto find_type_pack(std::span<TypeHandle const> args) const
        -> TypeHandle {
        size_t i{};

        for (auto const& t : types) {
            if (t.is_pack()) {
                if (std::ranges::equal(args, get_children(t)))
                    return TypeHandle::from_size(i);
            }

            i++;
        }

        return TypeHandle{}.to_invalid();
    }

    [[nodiscard]] auto find_type(Type const& needle) const -> TypeHandle {
        size_t i{};

        for (auto const& t : types) {
            if (t == needle) return TypeHandle::from_size(i);
            i++;
        }

        return TypeHandle{}.to_invalid();
    }

    // Get a reference to a type from it's handle. The pointer is invalid after
    // any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get(TypeHandle h) const -> Type const* {
        ASSERT(h.is_valid(), "invalid type handle", h.value());
        ASSERT(!h.is_array(), "type handle is an array", h.value());

        return &types.at(h.as_idx());
    }

    // Get a mutable reference to a type from it's handle. The pointer is
    // invalid after any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get_mut(TypeHandle h) -> Type* {
        ASSERT(h.is_valid(), "invalid type handle", h.value());
        ASSERT(!h.is_array(), "type handle is an array", h.value());

        return &types.at(h.as_idx());
    }

    // Get a reference to a type array from it's handle. The pointer is invalid
    // after any modification to the ast, do not hold on to it.
    [[nodiscard]] constexpr auto get_array(TypeHandle h, size_t count) const
        -> std::span<TypeHandle const> {
        ASSERT(h.is_valid(), "invalid type handle", h.value());
        ASSERT(h.is_array(), "type handle is not an array", h.value());

        std::span s = type_refs;
        return s.subspan(h.as_idx(), count);
    }

    [[nodiscard]] constexpr auto get_children(TypeHandle h) const
        -> std::span<TypeHandle const> {
        return get_children(get(h));
    }

    [[nodiscard]] constexpr auto get_children(Type const* h) const
        -> std::span<TypeHandle const> {
        return get_children(*h);
    }

    [[nodiscard]] constexpr auto get_children(Type const& h) const
        -> std::span<TypeHandle const> {
        return get_array(h.children(), h.count());
    }

    [[nodiscard]] constexpr auto fatten(TypeHandle h) const -> FatTypeHandle {
        return {.ts = this, .type = h};
    }

    // get the total number of types
    [[nodiscard]] constexpr auto size() const -> size_t { return types.size(); }

    // get the total number of type references (arrays)
    [[nodiscard]] constexpr auto refs_size() const -> size_t {
        return type_refs.size();
    }

    [[nodiscard]] constexpr auto ptr_size() const -> size_t {
        return sizeof(uintptr_t);
    }

    auto dump(fmt::format_context& ctx, TypeHandle n) const
        -> fmt::format_context::iterator;

private:
    TypeHandle void_type;
    TypeHandle type_type;
    TypeHandle err_type;
    TypeHandle i64_type;
    TypeHandle u64_type;
    TypeHandle i32_type;
    TypeHandle u32_type;
    TypeHandle i16_type;
    TypeHandle u16_type;
    TypeHandle i8_type;
    TypeHandle u8_type;
    TypeHandle bool_type;
    TypeHandle usize_type;
    TypeHandle isize_type;

    std::vector<Type>       types;
    std::vector<TypeHandle> type_refs;
};

}  // namespace yal

template <>
struct fmt::formatter<yal::TypeHandle> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::TypeHandle n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::TypeKind> : formatter<string_view> {
    auto format(yal::TypeKind n, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::Type> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::Type t, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::FatTypeHandle> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::FatTypeHandle n, format_context& ctx) const
        -> format_context::iterator;
};

namespace yal {

constexpr auto Type::size(TypeStore const& ts) const -> size_t {
    switch (kind) {
        case TypeKind::Err:
        case TypeKind::Type:
        case TypeKind::Pack:
        case TypeKind::Void:
        case TypeKind::Func: return 0;

        case TypeKind::Int64:
        case TypeKind::Uint64: return 8;
        case TypeKind::Int32:
        case TypeKind::Uint32: return 4;
        case TypeKind::Int16:
        case TypeKind::Uint16: return 2;
        case TypeKind::Int8:
        case TypeKind::Uint8:
        case TypeKind::Bool: return 1;

        case TypeKind::Usize:
        case TypeKind::Isize:
        case TypeKind::Ptr:
        case TypeKind::MultiPtr: return ts.ptr_size();

        case TypeKind::Array: return ts.get(first)->size(ts) * second.value();
    }

    return 0;
}

}  // namespace yal
