#include "types.hpp"

#include <algorithm>
#include <ranges>
#include <string_view>
#include <utility>

#include "fmt/base.h"
#include "fmt/format.h"
#include "fmt/ranges.h"
#include "libassert/assert.hpp"
#include "nlohmann/json.hpp"

namespace yal::types {
namespace rv = std::ranges::views;

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto Type::as_func() const -> Func {
    ASSERT(is_func());
    ASSERT(inner.size() == 2);
    ASSERT(inner[0]->kind == TypeKind::Pack);
    ASSERT(inner[1]->kind == TypeKind::Pack);

    return {
        .params = inner[0],
        .ret = inner[1],
        .is_var_args = kind == TypeKind::FuncWithVarArgs,
        .is_bound = kind == TypeKind::BoundFunc,
    };
}

auto Type::as_struct_get_fields() const
    -> std::unordered_map<std::string_view, StructField> {
    ASSERT(is_struct());

    std::unordered_map<std::string_view, StructField> fields;

    size_t offset{};

    for (auto it : inner) {
        auto align = it->alignment();
        offset = (offset + (align - 1)) & -align;

        fields[it->id] = {
            .offset = offset,
            .type = it->inner[0],
        };

        offset += it->size();
    }

    return fields;
}

auto TypeStore::new_pack(std::span<Type *const> inner) -> Type * {
    std::vector<Type *> expanded;
    for (auto it : inner) {
        if (it->is_pack()) {
            for (auto itt : it->inner) expanded.push_back(itt);
        } else {
            expanded.push_back(it);
        }
    }

    return new_type(TypeKind::Pack, expanded);
}

void TypeStore::add_function_to_type(Type const &ty, std::string_view name,
                                     Decl *d) {
    // fmt::println(stderr, "add method {:?} to {}", name, ty);
    namespaced[ty][name] = d;
}

auto TypeStore::get_function_from_type(Type const      &ty,
                                       std::string_view name) const -> Decl * {
    // fmt::println(stderr, "find in {}", ty);
    if (auto it = namespaced.find(ty); it != std::end(namespaced)) {
        if (auto fn = it->second.find(name); fn != std::end(it->second))
            return fn->second;
    }

    return nullptr;
}

auto TypeStore::new_bound_from(Type const *ty) -> Type * {
    auto data = ty->as_func();
    ASSERT(ty->kind != TypeKind::FuncWithVarArgs);

    return new_type(TypeKind::BoundFunc, std::array{data.params, data.ret});
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto TypeStore::coerce(Type *dst, Type *src) -> Type * {
    ASSERT(dst != nullptr);
    ASSERT(src != nullptr);

    // errors are forwarded
    if (dst->is_err()) return dst;

    // unpack single sized packs
    if (dst->is_pack() && !src->is_pack() && dst->inner.size() == 1)
        dst = dst->inner[0];
    if (src->is_pack() && !dst->is_pack() && src->inner.size() == 1)
        src = src->inner[0];

    if (dst->is_integral()) {
        if (!src->is_integral()) return nullptr;
        if (src->is_untyped_int()) return dst;

        if (dst->is_untyped_int()) {
            ASSERT(!src->is_untyped_int());
            return src;
        }

        if (dst->is_signed() && src->is_signed()) {
            if (dst->size() < src->size()) return nullptr;
            return dst;
        }

        if (dst->is_signed() && src->is_unsigned()) {
            if (dst->size() <= src->size()) return nullptr;
            return dst;
        }

        if (dst->is_unsigned() && src->is_signed()) return nullptr;

        if (dst->is_unsigned() && src->is_unsigned()) {
            if (dst->size() < src->size()) return nullptr;
            return dst;
        }

        UNREACHABLE("all cases should be handled for integers", *src, *dst);
    }

    if (dst->is_bool()) {
        if (src->is_bool()) return dst;
        return nullptr;
    }

    if (dst->kind == TypeKind::StrView) {
        if (src->kind == TypeKind::StrView) return dst;
        return nullptr;
    }

    if (dst->kind == TypeKind::MultiPtr) {
        if (src->kind == TypeKind::MultiPtr) return dst;
        return nullptr;
    }

    if (dst->kind == TypeKind::MultiPtrConst) {
        if (src->kind == TypeKind::MultiPtr ||
            src->kind == TypeKind::MultiPtrConst)
            return dst;
        return nullptr;
    }

    if (dst->kind == TypeKind::Pack) {
        if (src->kind != TypeKind::Pack) return nullptr;

        std::vector<Type *> res;
        for (auto [s, d] : rv::zip(src->inner, dst->inner)) {
            auto r = coerce(d, s);

            // TODO: do we want to abort completally here or keep the nullptr
            if (r == nullptr) return nullptr;
            res.push_back(r);
        }

        return new_pack(res);
    }

    if (dst->is_distinct() && src->is_lit()) {
        auto original_dst = dst;
        while (dst->is_distinct()) dst = dst->inner[0];

        auto expected_items = dst->as_struct_get_fields();

        std::vector<Type *> results;
        for (auto recv : src->inner) {
            if (recv->is_lit_field()) {
                auto it = expected_items.find(recv->id);
                if (it == expected_items.end()) return nullptr;

                auto ty = coerce(it->second.type, recv->inner[0]);
                // TODO: do we want to abort completally here or keep the
                // nullptr
                if (!ty) return nullptr;

                results.push_back(new_struct_field(recv->id, ty));
            }

            else {
                // TODO: add support for positional arguments
                return nullptr;
            }
        }

        if (results.size() != expected_items.size()) return nullptr;

        auto result = new_struct(results);
        auto dst = original_dst;
        while (dst->is_distinct()) {
            result = new_distinct_of(dst->id, result);
            dst = dst->inner[0];
        }

        return result;
    }

    return nullptr;
}

auto TypeStore::cast(Type *dst, Type *src) -> Type * {
    ASSERT(dst != nullptr);
    ASSERT(src != nullptr);

    // errors are forwarded
    if (dst->is_err()) return dst;
    if (*dst == *src) return dst;

    if (dst->is_integral() && src->is_integral()) return dst;

    if (src->is_distinct()) return cast(dst, src->inner[0]);
    if (dst->is_distinct()) {
        return new_distinct_of(dst->id, cast(dst->inner[0], src));
    }

    return nullptr;
}

void to_json(json &j, Type const &t) {
    auto arr = json::array();
    for (auto inner : t.inner) {
        if (inner)
            arr.push_back(*inner);
        else
            arr.push_back({});
    }

    j = json{
        { "kind", fmt::to_string(t.kind)},
        {"inner",                    arr},
    };

    if (t.is_distinct()) {
        j["id"] = t.id;
    }
}

void to_json(json &j, TypeStore const &ts) {
    auto arr = json::array();
    for (auto const &t : ts) {
        arr.push_back(t);
    }

    j = json{
        {"types", arr},
    };
}

}  // namespace yal::types

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto fmt::formatter<yal::types::Type>::format(yal::types::Type ty,
                                              format_context  &ctx) const
    -> format_context::iterator {
    using namespace yal::types;
    namespace rv = std::ranges::views;

    auto format_no_inner = [&](TypeKind ty) {
        std::string_view result = "<missing>";

        switch (ty) {
            case TypeKind::Err: result = "<error>"; break;
            case TypeKind::Type: result = "type"; break;
            case TypeKind::Void: result = "void"; break;
            case TypeKind::UntypedInt: result = "untyped_int"; break;
            case TypeKind::Uint64: result = "u64"; break;
            case TypeKind::Int64: result = "i64"; break;
            case TypeKind::Uint32: result = "u32"; break;
            case TypeKind::Int32: result = "i32"; break;
            case TypeKind::Uint16: result = "u16"; break;
            case TypeKind::Int16: result = "i16"; break;
            case TypeKind::Uint8: result = "u8"; break;
            case TypeKind::Int8: result = "i8"; break;
            case TypeKind::Usize: result = "usize"; break;
            case TypeKind::Isize: result = "isize"; break;
            case TypeKind::Bool: result = "bool"; break;
            case TypeKind::Float32: result = "f32"; break;
            case TypeKind::Float64: result = "f64"; break;
            case TypeKind::StrView: result = "string_view"; break;
            case TypeKind::Nil: result = "nil"; break;
            default: break;
        }

        return fmt::format_to(ctx.out(), "{}", result);
    };

    auto comma_separate = [](std::span<Type *const> t) {
        return fmt::join(
            rv::transform(
                t, [](Type *t) { return static_cast<Type const &>(*t); }),
            ", ");
    };

    switch (ty.kind) {
        case TypeKind::Err:
        case TypeKind::Type:
        case TypeKind::Void:
        case TypeKind::UntypedInt:
        case TypeKind::Uint64:
        case TypeKind::Int64:
        case TypeKind::Uint32:
        case TypeKind::Int32:
        case TypeKind::Uint16:
        case TypeKind::Int16:
        case TypeKind::Uint8:
        case TypeKind::Int8:
        case TypeKind::Usize:
        case TypeKind::Isize:
        case TypeKind::Bool:
        case TypeKind::Float32:
        case TypeKind::Float64:
        case TypeKind::StrView:
        case TypeKind::Nil: return format_no_inner(ty.kind);

        case TypeKind::Ptr:
            ASSERT(ty.inner.size() == 1);
            return fmt::format_to(ctx.out(), "*{}", *ty.inner[0]);
        case TypeKind::PtrConst:
            ASSERT(ty.inner.size() == 1);
            return fmt::format_to(ctx.out(), "*const {}", *ty.inner[0]);

        case TypeKind::MultiPtr:
            ASSERT(ty.inner.size() == 1);
            return fmt::format_to(ctx.out(), "[*]{}", *ty.inner[0]);
        case TypeKind::MultiPtrConst:
            ASSERT(ty.inner.size() == 1);
            return fmt::format_to(ctx.out(), "[*]const {}", *ty.inner[0]);

        case TypeKind::Slice:
            return fmt::format_to(ctx.out(), "[]{}", *ty.inner[0]);
        case TypeKind::SliceConst:
            return fmt::format_to(ctx.out(), "[]const {}", *ty.inner[0]);

        case TypeKind::Array:
            return fmt::format_to(ctx.out(), "[{}]{}", ty.count, *ty.inner[0]);

        case TypeKind::Struct:
            return fmt::format_to(
                ctx.out(), "struct {{{}}}",
                fmt::join(
                    ty.inner | rv::transform([](Type *it) -> Type const & {
                        return *it;
                    }),
                    ", "));
        case TypeKind::StructField:
            return fmt::format_to(ctx.out(), "{}: {}", ty.id, *ty.inner[0]);

        case TypeKind::Lit:
            return fmt::format_to(
                ctx.out(), ".{{{}}}",
                fmt::join(
                    ty.inner | rv::transform([](Type *it) -> Type const & {
                        return *it;
                    }),
                    ", "));
        case TypeKind::LitField:
            return fmt::format_to(ctx.out(), ".{}={}", ty.id, *ty.inner[0]);

        case TypeKind::Func:
            ASSERT(ty.inner.size() == 2);
            ASSERT(ty.inner[0]->kind == TypeKind::Pack);
            ASSERT(ty.inner[1]->kind == TypeKind::Pack);

            if (ty.inner[1]->inner.size() == 1)
                return fmt::format_to(ctx.out(), "func({}) {}",
                                      comma_separate(ty.inner[0]->inner),
                                      *ty.inner[1]->inner[0]);

            return fmt::format_to(ctx.out(), "func({}) ({})",
                                  comma_separate(ty.inner[0]->inner),
                                  comma_separate(ty.inner[1]->inner));

        case TypeKind::FuncWithVarArgs:
            ASSERT(ty.inner.size() == 2);
            ASSERT(ty.inner[0]->kind == TypeKind::Pack);
            ASSERT(ty.inner[1]->kind == TypeKind::Pack);

            if (ty.inner[1]->inner.size() == 1)
                return fmt::format_to(ctx.out(), "func({}, ...) {}",
                                      comma_separate(ty.inner[0]->inner),
                                      *ty.inner[1]->inner[0]);

            return fmt::format_to(ctx.out(), "func({}, ...) ({})",
                                  comma_separate(ty.inner[0]->inner),
                                  comma_separate(ty.inner[1]->inner));

        case TypeKind::BoundFunc:
            ASSERT(ty.inner.size() == 2);
            ASSERT(ty.inner[0]->kind == TypeKind::Pack);
            ASSERT(ty.inner[1]->kind == TypeKind::Pack);

            if (ty.inner[1]->inner.size() == 1)
                return fmt::format_to(ctx.out(), "func<bound>({}) {}",
                                      comma_separate(ty.inner[0]->inner),
                                      *ty.inner[1]->inner[0]);

            return fmt::format_to(ctx.out(), "func<bound>({}) ({})",
                                  comma_separate(ty.inner[0]->inner),
                                  comma_separate(ty.inner[1]->inner));

        case TypeKind::Pack:
            return fmt::format_to(ctx.out(), "({})", comma_separate(ty.inner));

        case yal::types::TypeKind::Distinct:
            return fmt::format_to(ctx.out(), "#{}({})", ty.id,
                                  comma_separate(ty.inner));
    }

    return fmt::format_to(ctx.out(), "<invalid kind>");
}
