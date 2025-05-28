#include "sema.hpp"

#include <ranges>
#include <string_view>
#include <utility>
#include <vector>

#include "ast-node-conv.hpp"
#include "ast-node-visitor.hpp"
#include "ast-node.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "fmt/color.h"
#include "fmt/format.h"
#include "libassert/assert.hpp"
#include "types.hpp"
#include "value.hpp"
#include "yal.hpp"

namespace yal::sema {
using ast::Ast;
using ast::Node;
using ast::NodeKind;
namespace conv = ast::conv;

namespace rv = std::ranges::views;

struct State {
    types::TypeStore* ts;
    ErrorReporter*    er;
    Options const*    opt;
};

struct Context {
    Node* current_module;
    Node* current_loop;
    Node* current_function;

    [[nodiscard]] constexpr auto is_main_module() const -> bool {
        if (current_module)
            return conv::flat_module(*current_module).name == "main";
        return false;
    }

    [[nodiscard]] constexpr auto current_function_type() const -> types::Type* {
        if (current_function) return current_function->get_type();
        return nullptr;
    }

    [[nodiscard]] constexpr auto current_function_return_type() const
        -> types::Type* {
        auto ty = current_function_type();
        return ty ? ty->as_func().ret : nullptr;
    }

    [[nodiscard]] constexpr auto with_module(Node* current_module) const
        -> Context {
        return {
            .current_module = current_module,
            .current_loop = current_loop,
            .current_function = current_function,
        };
    }

    [[nodiscard]] constexpr auto with_loop(Node* current_loop) const
        -> Context {
        return {
            .current_module = current_module,
            .current_loop = current_loop,
            .current_function = current_function,
        };
    }

    [[nodiscard]] constexpr auto with_function(Node* current_function) const
        -> Context {
        return {
            .current_module = current_module,
            .current_loop = current_loop,
            .current_function = current_function,
        };
    }
};

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void fixup_untyped(Ast& ast, types::Type* desired_type, Node* node,
                   State& state) {
    if (node == nullptr) return;
    if (desired_type->is_err()) return;  // nothing we can do

    auto& ts = *state.ts;

    ASSERT(!desired_type->contains_untyped());
    auto node_type = node->get_type();

#if SHOW_DESIRED_TYPES
    state.er->report_debug(node->get_loc(), "desired type for {} is {}",
                           node->get_kind(), *desired_type);
#endif

    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);

        ASSERT(node_type->is_pack());
        ASSERT(desired_type->is_pack());
        ASSERT(data.items.size() == node_type->inner.size());

        std::vector<types::Type*> new_types;
        new_types.reserve(data.items.size());
        for (auto [node, desired_type] :
             rv::zip(data.items, desired_type->inner)) {
            fixup_untyped(ast, desired_type, node, state);
            new_types.push_back(node->get_type());
        }

        node->set_type(ts.new_pack(new_types));
        return;
    }

    if (node->is_oneof(ast::NodeKind::Add, ast::NodeKind::Sub,
                       ast::NodeKind::Mul, ast::NodeKind::Div,
                       ast::NodeKind::Mod)) {
        auto data = conv::binary(*node);
        auto lhs = data.lhs->get_type()->unpacked();
        auto rhs = data.rhs->get_type()->unpacked();

        // both sides are untyped integers, use the desired type
        if (lhs->is_untyped_int() && rhs->is_untyped_int()) {
            fixup_untyped(ast, desired_type, data.lhs, state);
            fixup_untyped(ast, desired_type, data.rhs, state);

            node->set_type(desired_type);
            return;
        }

        // lhs is untyped, use the type of rhs
        if (lhs->is_untyped_int()) {
            fixup_untyped(ast, rhs, data.lhs, state);
            node->set_type(rhs);
            return;
        }

        // rhs is untyped, use the type of lhs
        if (rhs->is_untyped_int()) {
            fixup_untyped(ast, lhs, data.rhs, state);
            node->set_type(lhs);
            return;
        }

        // no untyped things here, but there might be at lower levels
        fixup_untyped(ast, lhs, data.lhs, state);
        fixup_untyped(ast, rhs, data.rhs, state);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Equal, ast::NodeKind::NotEqual,
                       ast::NodeKind::Less, ast::NodeKind::Greater,
                       ast::NodeKind::LessEqual, ast::NodeKind::GreaterEqual)) {
        auto data = conv::binary(*node);
        auto lhs = data.lhs->get_type()->unpacked();
        auto rhs = data.rhs->get_type()->unpacked();

        // both sides are untyped integers, should default them
        if (lhs->is_untyped_int() && rhs->is_untyped_int()) {
            fixup_untyped(ast, ts.get_default_int(), data.lhs, state);
            fixup_untyped(ast, ts.get_default_int(), data.rhs, state);
            return;
        }

        // lhs is untyped, use the type of rhs
        if (lhs->is_untyped_int()) {
            fixup_untyped(ast, rhs, data.lhs, state);
            return;
        }

        // rhs is untyped, use the type of lhs
        if (rhs->is_untyped_int()) {
            fixup_untyped(ast, lhs, data.rhs, state);
            return;
        }

        // no untyped things here, but there might be at lower levels
        fixup_untyped(ast, lhs, data.lhs, state);
        fixup_untyped(ast, rhs, data.rhs, state);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Lit)) {
        desired_type = desired_type->undistinct();
        ASSERT(desired_type->is_struct());

        // NOTE: this should already have been validated before fixup
        node->set_type(desired_type);

        auto expected_fields = desired_type->as_struct_get_fields();

        auto data = conv::lit(*node);
        for (auto node : data.items) {
            auto ty = node->get_type();

            if (node->is_oneof(ast::NodeKind::LitParam)) {
                auto data = conv::lit_param(*node);
                ASSERT(ty->is_lit_field());

                auto ex_it = expected_fields.find(data.key);
                if (ex_it == expected_fields.end()) continue;
                auto ex = ex_it->second.type;

                node->set_type(ts.new_lit_field(ty->id, ex));

                fixup_untyped(ast, ex, data.init, state);
            }

            else {
                fixup_untyped(ast, ts.get_default_for(ty), node, state);
            }
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        ASSERT(desired_type->is_integral(), *desired_type);
        node->set_type(desired_type);
    }
}

void check_no_untyped(Ast& ast, Node* node, ErrorReporter& er) {
    if (node == nullptr) return;

    auto type = node->get_type();
    if (type == nullptr) return;

    if (type->contains_untyped()) {
        er.report_bug(node->get_loc(),
                      "node has untyped integer type, this should not have "
                      "happended ({})",
                      node->get_kind());
    }

    ast::visit_children(
        ast, node,
        [](Ast& ast, Node* node, auto&&, ErrorReporter& er) {
            check_no_untyped(ast, node, er);
        },
        er);
}

// ============================================================================

enum class CoerceResult {
    Direct,
    RequiresCast,
};

auto coerce_types(types::Type* target, types::Type* source, Node* target_node,
                  Node* source_node, State& state)
    -> std::pair<types::Type*, CoerceResult>;

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto coerce_cast_lit_to_struct(types::Type* target, types::Type* source,
                               Node* source_node, State& state) {
    auto& er = *state.er;

    auto expected_fields = target->undistinct()->as_struct_get_fields();
    std::unordered_map<std::string_view, Node*> initialized_fields;

    auto lit_node = source_node->is_oneof(ast::NodeKind::Lit)
                        ? conv::lit(*source_node)
                        : conv::Lit{};

    for (auto [idx, source_field] : rv::enumerate(source->inner)) {
        // TODO: support positional initialization of structs
        if (!source_field->is_lit_field()) {
            auto loc = static_cast<size_t>(idx) < lit_node.items.size()
                           ? lit_node.items[idx]->get_loc()
                           : source_node->get_loc();

            er.report_error(loc,
                            "can not initialize field of struct positionally.");
            continue;
        }

        auto it = expected_fields.find(source_field->id);
        if (it == expected_fields.end()) {
            auto loc = static_cast<size_t>(idx) < lit_node.items.size()
                           ? lit_node.items[idx]->get_loc()
                           : source_node->get_loc();

            er.report_error(loc, "no field named {:?} found in type {}",
                            source_field->id, *target);
            continue;
        }

        if (auto in = initialized_fields.find(it->first);
            in != initialized_fields.end()) {
            auto loc = static_cast<size_t>(idx) < lit_node.items.size()
                           ? lit_node.items[idx]->get_loc()
                           : source_node->get_loc();

            er.report_error(loc, "field {:?} has already been initialized",
                            it->first);
            if (in->second) {
                er.report_note(in->second->get_loc(), "initialized here");
            }
        }

        initialized_fields[it->first] =
            static_cast<size_t>(idx) < lit_node.items.size()
                ? lit_node.items[idx]
                : nullptr;

        auto sn = static_cast<size_t>(idx) < lit_node.items.size()
                      ? lit_node.items[idx]
                      : source_node;

        // NOTE: getting rid of the target_node here as it might cause
        // confusion because it points to the wrong place after this
        // recursion
        coerce_types(it->second.type, source_field->inner[0], nullptr, sn,
                     state);
    }

    for (auto [field_name, ex] : expected_fields) {
        if (!initialized_fields.contains(field_name)) {
            er.report_error(source_node->get_loc(),
                            "missing initializer for field {:?} of type {} in "
                            "struct initialization",
                            field_name, *ex.type);
        }
    }

    // NOTE: just returning the target type after reporting any errors.
    // **Not** returning and error type on failure (as that would
    // require deep checking), this should improve things.
    return target;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto coerce_types(types::Type* target, types::Type* source, Node* target_node,
                  Node* source_node, State& state)
    -> std::pair<types::Type*, CoerceResult> {
    auto& er = *state.er;
    auto& ts = *state.ts;

#if SHOW_COERCE_TYPES
    er.report_debug(source_node->get_loc(),
                    "coerce_types(target={}, source={})", *target, *source);
#endif

    // in case the target is not a pack, but the source is a pack with a single
    // item, unpack the source
    if (!target->is_pack() && source->is_pack() && source->inner.size() == 1) {
        source = source->inner[0];
    }

    // same type, nothing to do
    if (*source == *target) return {target, CoerceResult::Direct};

    // in case the source is void, then it is not compatible with anything but
    // void
    if (source->is_void()) {
        er.report_error(source_node->get_loc(),
                        "can not use void where value of type {} is expected",
                        *target);

        return {ts.get_error(), CoerceResult::Direct};
    }

    if (source->is_lit()) {
        // lit can coerce to any struct, even if it is distinct
        if (target->is_distinct() && target->undistinct()->is_struct()) {
            auto r =
                coerce_cast_lit_to_struct(target, source, source_node, state);

            // NOTE: just returning the target type after reporting any errors.
            // **Not** returning and error type on failure (as that would
            // require deep checking), this should improve things.
            return {r, CoerceResult::Direct};
        }

        PANIC("not implemented (coerce lit)", *target, *source);
    }

    if (target->is_pack()) {
        // both are packs, coerce each inner type
        if (source->is_pack()) {
            // different sizes of packs, incompatible
            if (target->inner.size() != source->inner.size()) {
                er.report_error(source_node->get_loc(),
                                "can not use type of {} where {} is expected. "
                                "{} has {} elements but {} has {} elements",
                                *source, *target, *target, target->inner.size(),
                                *source, source->inner.size());
                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "required type {} from here", *target);
                }

                return {ts.get_error(), CoerceResult::Direct};
            }

            std::vector<types::Type*> cp;
            cp.reserve(target->inner.size());

            auto res = CoerceResult::Direct;
            for (auto [t, s] : rv::zip(target->inner, source->inner)) {
                auto [r, rr] =
                    coerce_types(t, s, target_node, source_node, state);
                if (rr == CoerceResult::RequiresCast) res = rr;

                cp.push_back(r);
            }

            return {ts.new_pack(cp), res};
        }

        // the target is a pack, but the source is not, error
        er.report_error(
            source_node->get_loc(),
            "can not use type of {} where a pack of type {} is expected",
            *source, *target);
        if (target_node != nullptr) {
            er.report_note(target_node->get_loc(), "required type {} from here",
                           *target);
        }

        return {ts.get_error(), CoerceResult::Direct};
    }

    if (target->is_integral()) {
        if (!source->is_integral()) {
            if (!source->is_err()) {
                er.report_error(source_node->get_loc(),
                                "can not use type non-integral type {} when "
                                "integer of type {} is expected",
                                *source, *target);
                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "required type {} from here", *target);
                }
            }

            return {ts.get_error(), CoerceResult::Direct};
        }

        // in case the source is an untyped int, it is compatible with
        // everything. Just return the target. It is not a problem if the target
        // is also an untyped int.
        if (source->is_untyped_int()) return {target, CoerceResult::Direct};

        // Both integers are signed or both are unsigned, they are compatible
        // when the target size is larger or equal to the source.
        if ((target->is_signed() && source->is_signed()) ||
            (target->is_unsigned() && source->is_unsigned())) {
            if (target->size() >= source->size())
                return {target, CoerceResult::RequiresCast};

            er.report_error(source_node->get_loc(),
                            "can not implicitly cast integer of type {} to {}",
                            *source, *target);
            if (target_node != nullptr) {
                er.report_note(target_node->get_loc(),
                               "required type {} from here", *target);
            }

            return {ts.get_error(), CoerceResult::Direct};
        }

        // Target is signed but source is unsigned (and vice-versa). This can
        // not be done.
        er.report_error(source_node->get_loc(),
                        "can not implicitly cast integers of different "
                        "signness: {} to {}",
                        *source, *target);
        if (target_node != nullptr) {
            er.report_note(target_node->get_loc(), "required type {} from here",
                           *target);
        }

        return {ts.get_error(), CoerceResult::Direct};
    }

    if (target->is_mptr()) {
        if (source->is_rawptr()) return {target, CoerceResult::Direct};
        if (source->is_mptr()) {
            if (*target->inner[0] != *source->inner[0]) {
                er.report_error(
                    source_node->get_loc(),
                    "can not coerce multi-pointer of {} to multi-pointer of {}",
                    *target->inner[0], *source->inner[0]);

                // NOTE: could return target type here to avoid further ghost
                // errors
                return {ts.get_error(), CoerceResult::Direct};
            }

            if (target->is_mptr_mut() && source->is_mptr_mut())
                return {target, CoerceResult::Direct};
            if (target->is_mptr_const() && source->is_mptr_mut())
                return {target, CoerceResult::Direct};
            if (target->is_mptr_const() && source->is_mptr_const())
                return {target, CoerceResult::Direct};
            if (target->is_mptr_mut() && source->is_mptr_const()) {
                er.report_error(
                    source_node->get_loc(),
                    "can not implicitly cast away constness of {} to {}",
                    *source, *target);
                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "this requires mutable {}", *target);
                }

                // NOTE: could return target type here to avoid further ghost
                // errors
                return {ts.get_error(), CoerceResult::Direct};
            }
        }
    }

    if (target->is_ptr()) {
        if (source->is_rawptr()) return {target, CoerceResult::Direct};
        if (source->is_ptr()) {
            if (*target->inner[0] != *source->inner[0]) {
                er.report_error(source_node->get_loc(),
                                "can not coerce pointer of {} to pointer of {}",
                                *target->inner[0], *source->inner[0]);

                // NOTE: could return target type here to avoid further ghost
                // errors
                return {ts.get_error(), CoerceResult::Direct};
            }

            if (target->is_ptr_mut() && source->is_ptr_mut())
                return {target, CoerceResult::Direct};
            if (target->is_ptr_const() && source->is_ptr_mut())
                return {target, CoerceResult::Direct};
            if (target->is_ptr_const() && source->is_ptr_const())
                return {target, CoerceResult::Direct};
            if (target->is_ptr_mut() && source->is_ptr_const()) {
                er.report_error(
                    source_node->get_loc(),
                    "can not implicitly cast away constness of {} to {}",
                    *source, *target);
                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "this requires mutable {}", *target);
                }

                // NOTE: could return target type here to avoid further ghost
                // errors
                return {ts.get_error(), CoerceResult::Direct};
            }
        }
    }

    if (target->is_rawptr()) {
        if (source->is_ptr() || source->is_mptr())
            return {target, CoerceResult::Direct};
    }

    if (target->is_slice() && source->is_slice()) {
        if (*target->inner[0] != *source->inner[0]) {
            er.report_error(source_node->get_loc(),
                            "can not coerce slice of {} to slice of {}",
                            *target->inner[0], *source->inner[0]);

            // NOTE: could return target type here to avoid further ghost errors
            return {ts.get_error(), CoerceResult::Direct};
        }

        if (target->is_slice_mut() && source->is_slice_mut())
            return {target, CoerceResult::Direct};
        if (target->is_slice_const() && source->is_slice_mut())
            return {target, CoerceResult::Direct};
        if (target->is_slice_const() && source->is_slice_const())
            return {target, CoerceResult::Direct};
        if (target->is_slice_mut() && source->is_slice_const()) {
            er.report_error(
                source_node->get_loc(),
                "can not implicitly cast away constness of {} to {}", *source,
                *target);
            if (target_node != nullptr) {
                er.report_note(target_node->get_loc(),
                               "this requires mutable {}", *target);
            }

            // NOTE: could return target type here to avoid further ghost errors
            return {ts.get_error(), CoerceResult::Direct};
        }
    }

    er.report_error(source_node->get_loc(),
                    "can not use value of type {} where {} is expected",
                    *source, *target);
    if (target_node != nullptr) {
        er.report_note(target_node->get_loc(), "this requires {}", *target);
    }

    // NOTE: could return target type here to avoid further ghost errors
    return {ts.get_error(), CoerceResult::Direct};
}

auto coerce_types_and_magic(Ast& ast, types::Type* target, types::Type* source,
                            Node* target_node, Node*& source_node, State& state)
    -> types::Type* {
    auto [r, rr] =
        coerce_types(target, source, target_node, source_node, state);

    if (rr == CoerceResult::RequiresCast) {
#if SHOW_IMPLICIT_COERCIONS
        state.er->report_debug(source_node->get_loc(),
                               "replacing source_node with coerce ({} to {})",
                               *source_node->get_type(), *r);
#endif

        source_node = ast.new_coerce(source_node->get_loc(), source_node, r);
    }

    return r;
}

auto coerce_types_and_fixup_untyped(Ast& ast, types::Type* target,
                                    types::Type* source, Node* target_node,
                                    Node*& source_node, State& state)
    -> types::Type* {
    auto r = coerce_types_and_magic(ast, target, source, target_node,
                                    source_node, state);
    fixup_untyped(ast, r, source_node, state);

    return r;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto cast_types(types::Type* target, types::Type* source, Node* target_node,
                Node* source_node, State& state) -> types::Type* {
    auto& er = *state.er;
    auto& ts = *state.ts;

#if SHOW_COERCE_TYPES
    er.report_debug(source_node->get_loc(), "cast_types(target={}, source={})",
                    *target, *source);
#endif

    if (!target->is_pack() && source->is_pack() && source->inner.size() == 1) {
        source = source->inner[0];
    }

    if (*source == *target) {
        // TODO: there should be a way to remove this warning...
        er.report_warn(source_node->get_loc(),
                       "unecessary cast, casting from {} to {}", *source,
                       *target);
        if (target_node != nullptr) {
            er.report_note(target_node->get_loc(), "type {} required from here",
                           *target);
        }

        return target;
    }

    // can just cast out of distinct, so add this at the start
    auto osource = source;
    source = source->undistinct();

    if (source->is_void()) {
        er.report_error(source_node->get_loc(),
                        "can not use void where value of type {} is expected",
                        *target);

        return ts.get_error();
    }

    if (target->is_integral()) {
        // NOTE: when casting pointers to integers, we lose const info

        if (source->is_mptr()) {
            if (!target->is_size()) {
                er.report_error(
                    source_node->get_loc(),
                    "can not cast multi-pointer {} to integers "
                    "other than `isize` and `usize` directly. Found {}",
                    *osource, *target);

                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "required {} from here", *target);
                }
            }

            return target;
        }

        if (source->is_ptr()) {
            if (!target->is_size()) {
                er.report_error(source_node->get_loc(),
                                "can not cast pointer {} to integers other "
                                "than `isize` and `usize` directly. Found {}",
                                *osource, *target);

                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "required {} from here", *target);
                }
            }

            return target;
        }

        // when using explicit casts any int can go to any int
        if (source->is_integral()) return target;
    }

    if (target->is_ptr()) {
        if (source->is_integral()) {
            if (!source->is_size()) {
                er.report_error(
                    source_node->get_loc(),
                    "can not cast integer type {} to pointer type {} directly. "
                    "Can only convert from `usize` and `isize` to pointer",
                    *osource, *target);

                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "required {} from here", *target);
                }
            }

            return target;
        }

        if (source->is_ptr()) return target;
        if (source->is_mptr()) return target;
        if (source->is_rawptr()) return target;
    }

    if (target->is_mptr()) {
        if (source->is_integral()) {
            if (!source->is_size()) {
                er.report_error(source_node->get_loc(),
                                "can not cast integer type {} to multi-pointer "
                                "type {} directly. Can only convert from "
                                "`usize` and `isize` to pointer",
                                *osource, *target);

                if (target_node != nullptr) {
                    er.report_note(target_node->get_loc(),
                                   "required {} from here", *target);
                }
            }

            return target;
        }

        if (source->is_ptr()) return target;
        if (source->is_mptr()) return target;
        if (source->is_rawptr()) return target;
    }

    if (target->is_strview()) {
        if (source->is_slice() && source->inner[0]->is_u8()) return target;
    }

    if (target->is_slice()) {
        if (target->is_slice_const() && target->inner[0]->is_u8() &&
            source->is_strview())
            return target;
    }

    if (target->is_struct()) {
        if (source->is_lit()) {
            return coerce_cast_lit_to_struct(target, source, source_node,
                                             state);
        }
    }

    if (target->is_distinct()) {
        auto tinner = target->inner[0];
        if (*tinner == *source) return target;

        if (tinner->is_struct() && source->is_lit()) {
            auto inner =
                cast_types(tinner, source, target_node, source_node, state);
            if (inner->is_err()) return inner;

            return ts.new_distinct_of(target->id, inner);
        }

        PANIC("not implemented (cast to distinct)", *target, *source);
    }

    // TODO: to struct

    er.report_error(source_node->get_loc(),
                    "can not cast from type {} to type {}", *osource, *target);
    return target;
}

auto cast_types_and_fixup_untyped(Ast& ast, types::Type* target,
                                  types::Type* source, Node* target_node,
                                  Node*& source_node, State& state)
    -> types::Type* {
    auto r = cast_types(target, source, target_node, source_node, state);
    fixup_untyped(ast, r, source_node, state);

    return r;
}

// ----------------------------------------------------------------------------

auto check_types_arith_and_coerce_integers_magic(Ast& ast, Node* comp,
                                                 types::Type* lhs_type,
                                                 types::Type* rhs_type,
                                                 Node*& lhs, Node*& rhs,
                                                 State& state) -> types::Type* {
    auto& er = *state.er;
    auto& ts = *state.ts;

    if (rhs_type->is_integral()) {
        // both sides are integers, use the larges one as result
        auto lhs_larger = lhs_type->size() > rhs_type->size();

        auto result_type = lhs_larger ? lhs_type : rhs_type;
        auto other_type = lhs_larger ? rhs_type : lhs_type;

        auto& result_node = lhs_larger ? lhs : rhs;
        auto& other_node = lhs_larger ? rhs : lhs;

        return coerce_types_and_fixup_untyped(ast, result_type, other_type,
                                              result_node, other_node, state);
    }

    // rhs is not an integer, error
    er.report_error(comp->get_loc(),
                    "can not add integer of type {} to value of type {}",
                    *lhs_type, *rhs_type);
    if (lhs) {
        er.report_note(lhs->get_loc(), "this expression has type {}",
                       *lhs_type);
    }
    if (rhs) {
        er.report_note(rhs->get_loc(), "this expression has type {}",
                       *rhs_type);
    }

    return ts.get_error();
}

auto check_types_arith_and_coerce_magic(Ast& ast, Node* comp, Node*& lhs,
                                        Node*& rhs, State& state)
    -> types::Type* {
    auto lhs_type = lhs->get_type()->unpacked();
    auto rhs_type = rhs->get_type()->unpacked();

    // if the types are the same, nothing to do
    if (*lhs_type == *rhs_type) return lhs_type;

    if (lhs_type->is_integral()) {
        return check_types_arith_and_coerce_integers_magic(
            ast, comp, lhs_type, rhs_type, lhs, rhs, state);
    }

    if (rhs_type->is_integral()) {
        // call again with lhs and rhs swapped
        // NOLINTNEXTLINE(readability-suspicious-call-argument)
        return check_types_arith_and_coerce_integers_magic(
            ast, comp, rhs_type, lhs_type, rhs, lhs, state);
    }

    PANIC("not implemented", *lhs_type, *rhs_type);
}

// ----------------------------------------------------------------------------

auto check_types_comparable(Ast& ast, Node* comp, Node* lhs, Node* rhs,
                            State& state) -> bool {
    auto& er = *state.er;
    auto  lhs_type = lhs->get_type()->unpacked();
    auto  rhs_type = rhs->get_type()->unpacked();

    // integers support everything
    if (lhs_type->is_integral()) {
        if (rhs_type->is_integral()) {
            check_types_arith_and_coerce_magic(ast, comp, lhs, rhs, state);
            return true;
        }

        er.report_error(
            comp->get_loc(),
            "can not compare integer of type {} to non-integral type {}",
            *lhs_type, *rhs_type);
        if (lhs) {
            er.report_note(lhs->get_loc(), "this expression has type {}",
                           *lhs_type);
        }
        if (rhs) {
            er.report_note(rhs->get_loc(), "this expression has type {}",
                           *rhs_type);
        }

        return false;
    }

    // booleans only support == and !=
    if (lhs_type->is_bool()) {
        if (rhs_type->is_bool()) {
            auto op = comp->get_kind();
            if (op == ast::NodeKind::Equal || op == ast::NodeKind::NotEqual)
                return true;

            er.report_error(comp->get_loc(),
                            "can not use operator '{}' on booleans", op);
            return false;
        }

        er.report_error(comp->get_loc(),
                        "can not compare boolean to value of type {}",
                        *rhs_type);
        if (lhs) {
            er.report_note(lhs->get_loc(), "this expression has type {}",
                           *lhs_type);
        }
        if (rhs) {
            er.report_note(rhs->get_loc(), "this expression has type {}",
                           *rhs_type);
        }

        return false;
    }

    PANIC("not implemented", *lhs_type, *rhs_type);
}

// ============================================================================

auto eval_expr_to_type(Node* node, State& state, Context& ctx) -> types::Type*;

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto eval_func_params(Node* node, State& state, Context& ctx) -> Value {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::func_params(*node);

    types::Type* last_type = nullptr;

    std::vector<types::Type*> types;
    types.reserve(data.params.size());

    for (auto node : data.params) {
        auto data = conv::func_param(*node);

        types::Type* type = nullptr;
        if (!data.type) {
            if (!last_type) {
                er.report_error(node->get_loc(),
                                "missing type for function parameter {:?}",
                                data.name);
                type = ts.get_error();
            } else {
                type = last_type;
            }
        } else {
            type = eval_expr_to_type(data.type, state, ctx);
        }

        auto d = node->get_decl();
        ASSERT(d != nullptr);
        ASSERT(d->get_type() == nullptr);

        d->value = {.type = type};
        types.push_back(type);

        last_type = type;
    }

    return {.type = ts.get_type(), .data = ts.new_pack(types)};
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto eval_func_ret_pack(Node* node, State& state, Context& ctx) -> Value {
    auto& ts = *state.ts;
    auto  data = conv::func_ret_pack(*node);

    std::vector<types::Type*> types;
    types.reserve(data.ret.size());

    for (auto node : data.ret) {
        if (!node->is_oneof(ast::NodeKind::NamedRet)) {
            auto type = eval_expr_to_type(node, state, ctx);
            types.push_back(type);
            continue;
        }

        auto data = conv::named_ret(*node);
        ASSERT(data.type != nullptr);

        auto type = eval_expr_to_type(data.type, state, ctx);

        auto d = node->get_decl();
        ASSERT(d != nullptr);
        ASSERT(d->get_type() == nullptr);

        d->value = {.type = type};
        types.push_back(type);
    }

    return {.type = ts.get_type(), .data = ts.new_pack(types)};
}

auto eval_id(Node* node, State& state, Context& /*unused*/) -> Value {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto data = conv::id(*node);
    if (!data.to) {
        return {.type = ts.get_error()};
    }

    auto ty = data.to->get_type();
    if (!ty) {
        ASSERT(data.to->node != nullptr);

        // FIXME: should we report this here? Is it possible to get here?
        er.report_debug(node->get_loc(), "no type found in {:?}", data.name);
        er.report_note(data.to->node->get_loc(), "defined here ({})",
                       data.to->full_name);
        return {.type = ts.get_error()};
    }

    return data.to->value;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
auto eval_expr(Node* node, State& state, Context& ctx) -> Value {
    ASSERT(node != nullptr);

    auto& ts = *state.ts;
    auto& er = *state.er;

    if (node->is_oneof(ast::NodeKind::Ptr, ast::NodeKind::PtrConst)) {
        auto data = conv::ptr(*node);
        auto inner = eval_expr_to_type(data.inner, state, ctx);
        return {.type = ts.get_type(),
                .data = ts.new_ptr(inner, data.is_const)};
    }

    if (node->is_oneof(ast::NodeKind::MultiPtr, ast::NodeKind::MultiPtrConst)) {
        auto data = conv::mptr(*node);
        auto inner = eval_expr_to_type(data.inner, state, ctx);
        return {.type = ts.get_type(),
                .data = ts.new_mptr(inner, data.is_const)};
    }

    if (node->is_oneof(ast::NodeKind::Slice, ast::NodeKind::SliceConst)) {
        auto data = conv::slice(*node);
        auto inner = eval_expr_to_type(data.inner, state, ctx);
        return {.type = ts.get_type(),
                .data = ts.new_slice(inner, data.is_const)};
    }

    if (node->is_oneof(ast::NodeKind::StructType)) {
        auto data = conv::struct_type(*node);

        std::vector<types::Type*> fields;
        for (auto field : data.fields) {
            auto type = eval_expr_to_type(field, state, ctx);
            ASSERT(type->kind == types::TypeKind::StructField);
            fields.push_back(type);
        }

        return {.type = ts.get_type(), .data = ts.new_struct(fields)};
    }

    if (node->is_oneof(ast::NodeKind::StructField)) {
        auto data = conv::struct_field(*node);
        auto type = eval_expr_to_type(data.type, state, ctx);

        if (data.init) {
            auto v = eval_expr(data.init, state, ctx);
            er.report_bug(
                data.init->get_loc(),
                "default initializer for fields has not been implemented");
            er.report_note(data.init->get_loc(), "got value: {}", v);
        }

        return {
            .type = ts.get_type(),
            .data = ts.new_struct_field(data.name, type),
        };
    }

    if (node->is_oneof(ast::NodeKind::FuncParams)) {
        return eval_func_params(node, state, ctx);
    }

    if (node->is_oneof(ast::NodeKind::FuncRetPack)) {
        return eval_func_ret_pack(node, state, ctx);
    }

    if (node->is_oneof(ast::NodeKind::Str)) {
        auto data = conv::str(*node);
        return {.type = ts.get_strview(), .data = data.value};
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        auto data = conv::integers(*node);
        return {.type = node->get_type(), .data = data.value};
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        return eval_id(node, state, ctx);
    }

    PANIC("can not eval node of kind and type",
          fmt::to_string(node->get_kind()),
          node->get_type() ? *node->get_type() : *ts.get_error());
}

auto eval_expr_to_type(Node* node, State& state, Context& ctx) -> types::Type* {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto v = eval_expr(node, state, ctx);
    if (!v.type->is_type()) {
        er.report_error(node->get_loc(),
                        "can not use value of type {} as type in expression",
                        *v.type);
        return ts.get_error();
    }

    ASSERT(v.has_data());
    return v.get_data_type();
}

auto eval_function_decl_type(Node* node, State& state, Context& ctx)
    -> types::Type* {
    auto& ts = *state.ts;
    auto  data = conv::func_decl(*node);

    auto params = eval_expr_to_type(data.args, state, ctx);
    auto ret = data.ret ? eval_expr_to_type(data.ret, state, ctx)
                        : ts.new_pack(std::array{ts.get_void()});

    return ts.new_func(params, ret,
                       node->get_kind() == ast::NodeKind::FuncDeclWithCVarArgs);
}

// ============================================================================

struct ValueSide {
    enum Side {
        Right,      // rvalue
        LeftMut,    // mutable lvalue
        LeftConst,  // const lvalue
    };

    [[nodiscard]] constexpr auto is_rvalue() const -> bool {
        return side == Right;
    }

    [[nodiscard]] constexpr auto is_lvalue() const -> bool {
        return !is_rvalue();
    }

    [[nodiscard]] constexpr auto is_mut() const -> bool {
        return side == LeftMut;
    }

    [[nodiscard]] constexpr auto is_const() const -> bool {
        return side == LeftConst;
    }

    Side side;
};

auto calc_expr_side(Node* node) -> ValueSide {
    if (node->is_oneof(ast::NodeKind::Id)) {
        auto d = node->get_decl();
        if (d == nullptr) return {ValueSide::Right};

        if (d->is_const()) return {ValueSide::LeftConst};
        return {ValueSide::LeftMut};
    }

    return {ValueSide::Right};
}

// ============================================================================

void sema_expr(Ast& ast, Node* node, State& state, Context& ctx);
void sema_stmt(Ast& ast, Node* node, State& state, Context& ctx);

// ----------------------------------------------------------------------------

// FIXME: this needs a lot of rework to be ok
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void sema_field(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::field(*node);
    sema_expr(ast, data.receiver, state, ctx);

    auto rty = data.receiver->get_type();
    auto unw = rty->unpacked()->undistinct();

    if (unw->is_type()) {
        auto ty = eval_expr_to_type(data.receiver, state, ctx);
        auto fn = ts.get_function_from_type(*ty, data.name);
        if (fn) {
            node->set_type(fn->get_type());
            return;
        }
    }

    else if (unw->is_strview()) {
        if (data.name == "ptr") {
            node->set_type(ts.new_mptr(ts.get_u8(), true));
            return;
        }

        if (data.name == "len") {
            node->set_type(ts.get_usize());
            return;
        }
    }

    else if (unw->is_slice()) {
        if (data.name == "ptr") {
            node->set_type(ts.new_mptr(unw->inner[0], unw->is_slice_const()));
            return;
        }

        if (data.name == "len") {
            node->set_type(ts.get_usize());
            return;
        }
    }

    else if (unw->is_array()) {
        if (data.name == "ptr") {
            node->set_type(ts.new_mptr(ts.get_u8(), true));
            return;
        }

        if (data.name == "len") {
            node->set_type(ts.get_usize());
            return;
        }
    }

    else if (unw->is_struct()) {
        auto fields = unw->as_struct_get_fields();
        auto it = fields.find(data.name);
        if (it != fields.end()) {
            node->set_type(it->second.type);
            return;
        }
    }

    else if (unw->is_ptr() && unw->inner[0]->undistinct()->is_struct()) {
        auto inner = unw->inner[0]->undistinct();
        auto fields = inner->as_struct_get_fields();
        auto it = fields.find(data.name);
        if (it != fields.end()) {
            node->set_type(it->second.type);
            return;
        }
    }

    auto d = ts.get_function_from_type(*rty, data.name);
    if (d != nullptr) {
        node->set_type(ts.new_bound_from(d->value.type));

        ASSERT(node->get_decl() == nullptr);
        node->set_decl(d);
        return;
    }

    er.report_error(node->get_loc(),
                    "type {} does not have a field (or namespaced "
                    "function) named {:?}",
                    *rty, data.name);
    node->set_type(ts.get_error());
}

void sema_index(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::index(*node);

    sema_expr(ast, data.receiver, state, ctx);
    sema_expr(ast, data.index, state, ctx);

    auto rty = data.receiver->get_type()->unpacked()->undistinct();
    auto ity = data.index->get_type()->unpacked()->undistinct();

    if (!ity->is_integral()) {
        er.report_error(node->get_loc(),
                        "can not use non-integral type {} as index", *ity);
        node->set_type(ts.get_error());
        return;
    }

    fixup_untyped(ast, ts.get_usize(), data.index, state);

    auto unw = rty->unpacked()->undistinct();
    if (unw->is_mptr()) {
        node->set_type(unw->inner[0]);
        return;
    }

    if (unw->is_slice()) {
        node->set_type(unw->inner[0]);
        return;
    }

    if (unw->is_strview()) {
        node->set_type(ts.get_u8());
        return;
    }

    if (unw->is_array()) {
        node->set_type(unw->inner[0]);
        return;
    }

    er.report_error(node->get_loc(), "type {} does not support indexing", *rty);
    node->set_type(ts.get_error());
}

void sema_slicing(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::slicing(*node);

    sema_expr(ast, data.receiver, state, ctx);
    sema_expr(ast, data.start, state, ctx);
    sema_expr(ast, data.end, state, ctx);

    auto rty = data.receiver->get_type()->unpacked()->undistinct();

    if (data.start) {
        auto sity = data.start->get_type();
        if (!sity->unpacked()->undistinct()->is_integral()) {
            er.report_error(
                node->get_loc(),
                "can not use non-integral type {} as index in slice operation",
                *sity);
            node->set_type(ts.get_error());
            return;
        }

        fixup_untyped(ast, ts.get_usize(), data.start, state);
    }

    if (data.end) {
        auto sity = data.end->get_type();
        if (!sity->unpacked()->undistinct()->is_integral()) {
            er.report_error(
                node->get_loc(),
                "can not use non-integral type {} as index in slice operation",
                *sity);
            node->set_type(ts.get_error());
            return;
        }

        fixup_untyped(ast, ts.get_usize(), data.end, state);
    }

    auto unw = rty->unpacked()->undistinct();
    if (unw->is_mptr()) {
        node->set_type(ts.new_slice(unw->inner[0], unw->is_mptr_const()));

        // when slicing a multi-pointer, the end is necessary
        if (data.end == nullptr) {
            er.report_error(node->get_loc(),
                            "missing end of slice for multi-pointer: {}", *rty);
            er.report_note(
                node->get_loc(),
                "when slicing a multi-pointer, the end of slice is mandatory");
        }

        return;
    }

    if (unw->is_slice()) {
        node->set_type(ts.new_slice(unw->inner[0], unw->is_slice_const()));
        return;
    }

    if (unw->is_strview()) {
        node->set_type(ts.get_strview());
        return;
    }

    if (unw->is_array()) {
        // FIXME: is the array constant or not? Need a type for contant arrays
        node->set_type(ts.new_slice(unw->inner[0], false));
        return;
    }

    er.report_error(node->get_loc(), "type {} does not support slicing", *rty);
    node->set_type(ts.get_error());
}

void sema_call(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::call(*node);

    sema_expr(ast, data.callee, state, ctx);
    for (auto node : data.args) sema_expr(ast, node, state, ctx);

    auto callee = data.callee->get_type();
    if (!callee->is_func()) {
        if (!callee->is_err()) {
            er.report_error(node->get_loc(),
                            "can not call non-function value of type {}",
                            *callee);
            er.report_note(data.callee->get_loc(),
                           "callee expression has type {}", *callee);
        }

        node->set_type(ts.get_error());
        return;
    }

    auto fn = callee->as_func();
    node->set_type(fn.ret);

    if (fn.is_var_args) {
        if (data.args.size() < fn.get_params().size()) {
            er.report_error(node->get_loc(),
                            "wrong number of arguments in call. Expected "
                            "at least {} arguments, got {}",
                            fn.get_params().size(), data.args.size());
        }

        // ok
    }

    else if (fn.get_params().size() != data.args.size()) {
        er.report_error(node->get_loc(),
                        "wrong number of arguments in call. Expected "
                        "{} arguments, got {}",
                        fn.get_params().size(), data.args.size());
    }

    for (auto const& [i, param, arg] :
         rv::zip(rv::iota(1), fn.get_params(), data.args)) {
        // NOTE: we access the node directly from it's children list and not
        // the correct place (which would be `data`) because we need to be
        // able to modify where the pointer points to
        coerce_types_and_fixup_untyped(ast, param, arg->get_type(), nullptr,
                                       node->get_child_ptrref(i), state);
    }

    // handle calls to `sizeof()`
    if (data.callee->is_oneof(ast::NodeKind::Id)) {
        if (auto d = data.callee->get_decl();
            d && d->link_name == "sizeof" && data.args.size() > 0) {
            auto arg = eval_expr_to_type(data.args[0], state, ctx);
            auto size = arg->size();
            node->transmute_to_const_int(ts.get_usize(), size);
        }
    }
}

void sema_array(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::array(*node);

    sema_expr(ast, data.inner, state, ctx);
    sema_expr(ast, data.size, state, ctx);

    auto inner_ty = eval_expr_to_type(data.inner, state, ctx);

    uint32_t count;
    if (data.size) {
        auto c = eval_expr(data.size, state, ctx);
        if (!c.type->is_integral()) {
            er.report_error(data.size->get_loc(),
                            "can not use non-integral type {} for array size",
                            *c.type);
            count = data.items.size();
        }

        else {
            fixup_untyped(ast, ts.get_usize(), data.size, state);

            count = c.get_data_uint();
        }

        if (count != data.items.size()) {
            er.report_error(node->get_loc(),
                            "array expects {} items, but received {}", count,
                            data.items.size());
            er.report_note(data.size->get_loc(), "{} items here", count);
        }
    }

    else {
        count = data.items.size();
    }

    for (auto it : data.items) {
        sema_expr(ast, it, state, ctx);

        coerce_types_and_fixup_untyped(ast, inner_ty, it->get_type(),
                                       data.inner, it, state);
    }

    auto ty = ts.new_array_type(inner_ty, count);
    node->set_type(ty);
}

void sema_lit(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    // auto& er = *state.er;
    auto data = conv::lit(*node);

    std::vector<types::Type*> types;
    for (auto node : data.items) {
        if (node->is_oneof(ast::NodeKind::LitParam)) {
            auto data = conv::lit_param(*node);
            sema_expr(ast, data.init, state, ctx);

            auto type = data.init->get_type();
            node->set_type(ts.new_lit_field(data.key, type));
        }

        else {
            sema_expr(ast, node, state, ctx);
        }

        types.push_back(node->get_type());
    }

    node->set_type(ts.new_lit(types));
}

// ----------------------------------------------------------------------------

void sema_expr_children(Ast& ast, Node* node, State& state, Context& ctx) {
    ast::visit_children(
        ast, node,
        [](Ast& ast, Node* node, auto&&, State& state, Context& ctx) {
            sema_expr(ast, node, state, ctx);
        },
        state, ctx);
}

void sema_stmt_children(Ast& ast, Node* node, State& state, Context& ctx) {
    ast::visit_children(
        ast, node,
        [](Ast& ast, Node* node, auto&&, State& state, Context& ctx) {
            sema_stmt(ast, node, state, ctx);
        },
        state, ctx);
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void sema_expr(Ast& ast, Node* node, State& state, Context& ctx) {
    if (node == nullptr) return;

    auto& ts = *state.ts;
    auto& er = *state.er;

    if (node->is_oneof(ast::NodeKind::Ptr, ast::NodeKind::PtrConst,
                       ast::NodeKind::MultiPtr, ast::NodeKind::MultiPtrConst,
                       ast::NodeKind::StructType, ast::NodeKind::Slice,
                       ast::NodeKind::SliceConst)) {
        node->set_type(ts.get_type());
        sema_expr_children(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::StructField)) {
        auto data = conv::struct_field_ref(*node);
        sema_expr(ast, data.type, state, ctx);
        sema_expr(ast, data.init, state, ctx);

        auto type = eval_expr_to_type(data.type, state, ctx);
        node->set_type(type);

        if (data.init) {
            coerce_types_and_fixup_untyped(ast, type, data.init->get_type(),
                                           data.type, data.init, state);
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::Cast)) {
        auto data = conv::binary(*node);

        sema_expr(ast, data.lhs, state, ctx);
        sema_expr(ast, data.rhs, state, ctx);

        auto target = eval_expr_to_type(data.rhs, state, ctx);
        auto r = cast_types_and_fixup_untyped(ast, target, data.lhs->get_type(),
                                              data.rhs, data.lhs, state);

        node->set_type(r);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Field)) {
        sema_field(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Index)) {
        sema_index(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Slicing)) {
        sema_slicing(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::AddrOf)) {
        auto data = conv::unary(*node);
        sema_expr(ast, data.child, state, ctx);

        auto side = calc_expr_side(data.child);
        if (side.is_rvalue()) {
            er.report_error(node->get_loc(),
                            "can not take address of r-value expression {}",
                            data.child->get_kind());
            node->set_type(ts.get_error());
            return;
        }

        // in case our child is an id, mark that it's backing declaration needs
        // to be allocated at the stack. This should only be done when the type
        // is not already stored on the stack by default (like structs and
        // arrays).
        if (data.child->is_oneof(ast::NodeKind::Id)) {
            if (auto d = data.child->get_decl();
                d && !d->get_type()->undistinct()->is_always_stack()) {
                d->flags.set_stack_var();
            }
        }

        auto cty = data.child->get_type();
        node->set_type(ts.new_ptr(cty, side.is_const()));
        return;
    }

    if (node->is_oneof(ast::NodeKind::Deref)) {
        auto data = conv::unary(*node);
        sema_expr(ast, data.child, state, ctx);

        auto cty = data.child->get_type()->unpacked();
        if (!cty->is_ptr()) {
            er.report_error(node->get_loc(),
                            "can not dereference non-pointer type {}", *cty);
            node->set_type(ts.get_error());
            return;
        }

        node->set_type(cty->inner[0]);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Equal, ast::NodeKind::NotEqual,
                       ast::NodeKind::Less, ast::NodeKind::Greater,
                       ast::NodeKind::LessEqual, ast::NodeKind::GreaterEqual)) {
        auto data = conv::binary(*node);
        node->set_type(ts.get_bool());

        sema_expr(ast, data.lhs, state, ctx);
        sema_expr(ast, data.rhs, state, ctx);

        check_types_comparable(ast, node, data.lhs, data.rhs, state);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Add, ast::NodeKind::Sub,
                       ast::NodeKind::Mul, ast::NodeKind::Div,
                       ast::NodeKind::Mod)) {
        auto data = conv::binary_ref(*node);

        sema_expr(ast, data.lhs, state, ctx);
        sema_expr(ast, data.rhs, state, ctx);

        auto r = check_types_arith_and_coerce_magic(ast, node, data.lhs,
                                                    data.rhs, state);
        node->set_type(r);
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprPack)) {
        auto data = conv::expr_pack(*node);

        std::vector<types::Type*> types;
        types.reserve(data.items.size());
        for (auto node : data.items) {
            sema_expr(ast, node, state, ctx);
            types.push_back(node->get_type());
        }

        node->set_type(ts.new_pack(types));
        return;
    }

    if (node->is_oneof(ast::NodeKind::Call)) {
        sema_call(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Array)) {
        sema_array(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Lit)) {
        sema_lit(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Str)) {
        node->set_type(ts.get_strview());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Int)) {
        node->set_type(ts.get_untyped_int());
        return;
    }

    if (node->is_oneof(ast::NodeKind::Id)) {
        auto data = conv::id(*node);
        if (!data.to) {
            node->set_type(ts.get_error());
            return;
        }

        auto ty = data.to->get_type();
        if (!ty) {
            ASSERT(data.to->node != nullptr);

            er.report_debug(node->get_loc(), "no type found in {:?}",
                            data.name);
            er.report_note(data.to->node->get_loc(), "defined here ({})",
                           data.to->full_name);
        }

        node->set_type(data.to->get_type());
        return;
    }

    PANIC("not implemented", node->get_kind(),
          fmt::to_string(node->get_kind()));
}

// ============================================================================

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void set_type_of_id(Node* id_node, types::Type* type, State& state) {
    ASSERT(type != nullptr);
    ASSERT(id_node != nullptr);

    auto& er = *state.er;

    ASSERT(id_node->get_type() == nullptr);
    id_node->set_type(type);

    auto name = conv::id(*id_node);
    if (name.to) {
        ASSERT(name.to->get_type() == nullptr);
        name.to->value = {.type = type};
    } else {
        er.report_bug(id_node->get_loc(), "id has no decl: {:?}", name.name);
    }
}

// ----------------------------------------------------------------------------

auto count_init_exprs(conv::ExprPack const& pack) -> size_t {
    size_t expr_count{};
    for (auto init : pack.items) {
        auto ty = init->get_type();
        ASSERT(ty != nullptr);

        if (ty->is_pack()) {
            // multiple returns of some function
            expr_count += ty->inner.size();
        } else {
            expr_count++;
        }
    }

    return expr_count;
}

void fixup_expr_pack(types::TypeStore& ts, Node* pack) {
    if (pack == nullptr) return;

    auto inits = conv::expr_pack(*pack).items;

    std::vector<types::Type*> types;
    for (auto n : inits) {
        ASSERT(n != nullptr);
        if (n->get_type() == nullptr) {
            types.push_back(ts.get_error());
        } else {
            types.push_back(n->get_type());
        }
    }

    pack->set_type(ts.new_pack(types));
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void sema_decl_with_types_and_inits(Ast& ast, Node* ids_node, Node* types_node,
                                    Node* inits_node, State& state,
                                    Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto types = conv::expr_pack(*types_node).items;
    auto inits = conv::expr_pack(*inits_node).items;

    auto expr_count = count_init_exprs(conv::expr_pack(*inits_node));

    if (ids.size() != expr_count) {
        er.report_error(inits_node->get_loc(),
                        "wrong number of initializers for declaration, "
                        "expected {} but got {}",
                        ids.size(), expr_count);
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    if (ids.size() != types.size()) {
        er.report_error(types_node->get_loc(),
                        "wrong number of types for declaration, "
                        "expected {} but got {}",
                        ids.size(), types.size());
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    for (size_t id_idx{}, ty_idx{};
         auto [init_idx, init] : rv::enumerate(inits)) {
        if (id_idx >= ids.size() || ty_idx >= types.size()) break;

        auto init_type = init->get_type();

        // result of calling a function
        if (init_type->unpacked()->is_pack()) {
            std::vector<types::Type*> coercions;
            coercions.reserve(init_type->inner.size());

            auto requires_pack_coercion = false;

            for (auto inner_init_type : init_type->inner) {
                // a function should not be able to return this
                ASSERT(!inner_init_type->is_untyped_int());

                if (id_idx >= ids.size() || ty_idx >= types.size()) break;
                auto type = eval_expr_to_type(types[ty_idx], state, ctx);
                auto [r, rc] = coerce_types(type, inner_init_type,
                                            types[ty_idx], init, state);

                coercions.push_back(r);
                if (rc == CoerceResult::RequiresCast)
                    requires_pack_coercion = true;

                if (!conv::id(*ids[id_idx]).is_discard()) {
                    set_type_of_id(ids[id_idx], r, state);
                }

                id_idx++;
                ty_idx++;
            }

            if (requires_pack_coercion) {
                auto c = ast.new_coerce(init->get_loc(), init,
                                        ts.new_pack(coercions));
                types_node->set_child(init_idx, c);
            }
        }

        // a simple initializer value
        else {
            auto id = ids[id_idx];
            auto type = eval_expr_to_type(types[ty_idx], state, ctx);
            auto r = coerce_types_and_fixup_untyped(ast, type, init_type,
                                                    types[ty_idx], init, state);

            if (!conv::id(*id).is_discard()) {
                set_type_of_id(id, r, state);
            }

            id_idx++;
            ty_idx++;
        }
    }
}

void sema_decl_with_types(Ast& /*unused*/, Node* ids_node, Node* types_node,
                          State& state, Context& ctx) {
    auto& er = *state.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto types = conv::expr_pack(*types_node).items;

    if (ids.size() != types.size()) {
        er.report_error(types_node->get_loc(),
                        "wrong number of types for declaration, "
                        "expected {} but got {}",
                        ids.size(), types.size());
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    for (auto const& [id, ty] : rv::zip(ids, types)) {
        auto name = conv::id(*id);
        auto type = eval_expr_to_type(ty, state, ctx);
        if (name.is_discard()) continue;

        set_type_of_id(id, type, state);
    }
}

void sema_decl_with_inits(Ast& ast, Node* ids_node, Node* inits_node,
                          State& state, Context& /*unused*/) {
    auto& ts = *state.ts;
    auto& er = *state.er;

    auto ids = conv::id_pack(*ids_node).ids;
    auto inits = conv::expr_pack(*inits_node).items;

    auto expr_count = count_init_exprs(conv::expr_pack(*inits_node));

    if (ids.size() != expr_count) {
        er.report_error(inits_node->get_loc(),
                        "wrong number of initializers for declaration, "
                        "expected {} but got {}",
                        ids.size(), expr_count);
        er.report_note(ids_node->get_loc(), "{} identifiers declared here",
                       ids.size());
    }

    for (size_t id_idx{}; auto [init_idx, init] : rv::enumerate(inits)) {
        if (id_idx >= ids.size()) break;

        fixup_untyped(ast, ts.get_default_for(init->get_type()), init, state);

        auto init_type = init->get_type();

        // result of calling some function
        if (init_type->is_pack()) {
            for (auto i : init_type->inner) {
                if (conv::id(*ids[id_idx]).is_discard()) {
                    id_idx++;
                    continue;
                }

                set_type_of_id(ids[id_idx], i, state);

                id_idx++;
            }
        }

        // a simple initializer expression
        else {
            auto id = ids[id_idx];
            if (!conv::id(*id).is_discard()) {
                set_type_of_id(id, init_type, state);
            }

            id_idx++;
        }
    }
}

void sema_var_decl(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::var_decl(*node);
    node->set_type(ts.get_void());

    sema_expr(ast, data.types, state, ctx);
    sema_expr(ast, data.inits, state, ctx);

    // variable declaration with explicit types and initializer
    //
    //     var x: i32 = 10;
    if (data.types && data.inits) {
        sema_decl_with_types_and_inits(ast, data.ids, data.types, data.inits,
                                       state, ctx);
    }

    // variable declaration with explicit types and no initializer
    //
    //     var x: i32;
    else if (data.types) {
        sema_decl_with_types(ast, data.ids, data.types, state, ctx);
    }

    // variable declaration with not explicit types but with initializer
    //
    //     var x = 0;
    else if (data.inits) {
        sema_decl_with_inits(ast, data.ids, data.inits, state, ctx);
    }

    // Nothing given
    //
    //     var x;
    else {
        er.report_error(node->get_loc(),
                        "missing types or initializer on variable declaration");

        auto ids = data.get_ids();
        for (auto id : ids.ids) {
            set_type_of_id(id, ts.get_error(), state);
        }
    }

    // fixup the type of the init expr pack
    fixup_expr_pack(ts, data.inits);

#if CHECK_UNTYPED
    check_no_untyped(ast, node, er);
#endif
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void sema_assign(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::assign(*node);
    node->set_type(ts.get_void());

    auto lhs = data.get_lhs().items;
    auto rhs = data.get_rhs().items;

    // lhs will be analyzed later as needed
    sema_expr(ast, data.rhs, state, ctx);

    auto expr_count = count_init_exprs(data.get_rhs());
    if (lhs.size() != expr_count) {
        er.report_error(node->get_loc(),
                        "wrong number of values in right hand side of "
                        "assignment, expected {} but got {}",
                        lhs.size(), expr_count);

        // NOTE: maybe some more reporting of each size?
    }

    for (size_t lhs_idx{}; auto [rhs_idx, rhs_item] : rv::enumerate(rhs)) {
        if (lhs_idx >= lhs.size()) break;

        auto rhs_type = rhs_item->get_type();

        // result of calling some function
        if (rhs_type->is_pack()) {
            std::vector<types::Type*> coercions;
            coercions.reserve(rhs_type->inner.size());

            auto requires_pack_coercion = false;

            for (auto inner_rhs_type : rhs_type->inner) {
                auto lhs_item = lhs[lhs_idx];
                if (lhs_item->is_oneof(ast::NodeKind::Id)) {
                    if (conv::id(*lhs_item).is_discard()) {
                        lhs_item->set_type(ts.get_void());
                        coercions.push_back(ts.get_void());
                        lhs_idx++;

                        fixup_untyped(ast,
                                      ts.get_default_for(rhs_item->get_type()),
                                      rhs_item, state);
                        continue;
                    }

                    // TODO: implement check of lvalues and constness for this
                    // to work correctly
                    sema_expr(ast, lhs_item, state, ctx);
                    auto d = lhs_item->get_decl();

                    // NOTE: do we need to use `get_children_ptrref`?
                    auto [r, rc] = coerce_types(d->get_type(), inner_rhs_type,
                                                lhs_item, rhs_item, state);

                    coercions.push_back(r);
                    if (rc == CoerceResult::RequiresCast)
                        requires_pack_coercion = true;

                    lhs_idx++;
                }

                else {
                    PANIC("not implemented", lhs_item->get_kind());
                }
            }

            if (requires_pack_coercion) {
                auto c = ast.new_coerce(rhs_item->get_loc(), rhs_item,
                                        ts.new_pack(coercions));
                data.rhs->set_child(rhs_idx, c);
            }
        }

        else {
            auto lhs_item = lhs[lhs_idx];
            if (lhs_item->is_oneof(ast::NodeKind::Id)) {
                if (conv::id(*lhs_item).is_discard()) {
                    lhs_item->set_type(ts.get_void());
                    lhs_idx++;

                    fixup_untyped(ast, ts.get_default_for(rhs_item->get_type()),
                                  rhs_item, state);

                    continue;
                }

                sema_expr(ast, lhs_item, state, ctx);

                auto d = lhs_item->get_decl();
                if (d->is_const()) {
                    er.report_error(
                        lhs_item->get_loc(),
                        "can not assign to constant value of type {}",
                        *lhs_item->get_type());
                    if (d->node) {
                        er.report_note(d->node->get_loc(), "defined here");
                    }
                }

                // NOTE: do we need to use `get_children_ptrref`?
                coerce_types_and_fixup_untyped(ast, d->get_type(),
                                               rhs_item->get_type(), lhs_item,
                                               rhs_item, state);
            }

            else if (lhs_item->is_oneof(ast::NodeKind::Deref)) {
                sema_expr(ast, lhs_item, state, ctx);

                auto inner = conv::unary(*lhs_item).child;
                auto inner_type = inner->get_type();
                ASSERT(inner_type->is_ptr());

                if (inner_type->is_const_ref()) {
                    er.report_error(
                        lhs_item->get_loc(),
                        "can not assign to constant value of type {}",
                        *lhs_item->get_type());
                    er.report_note(inner->get_loc(),
                                   "dereferenced pointer has type {}",
                                   *inner_type);
                }

                coerce_types_and_fixup_untyped(ast, lhs_item->get_type(),
                                               rhs_item->get_type(), lhs_item,
                                               rhs_item, state);
            }

            else if (lhs_item->is_oneof(ast::NodeKind::Index)) {
                sema_expr(ast, lhs_item, state, ctx);

                auto inner = conv::index(*lhs_item).receiver;
                auto inner_type = inner->get_type();

                if (inner_type->is_const_ref()) {
                    er.report_error(
                        lhs_item->get_loc(),
                        "can not assign to constant value of type {}",
                        *lhs_item->get_type());

                    if (inner_type->is_mptr()) {
                        er.report_note(
                            inner->get_loc(),
                            "multi-pointer inner element has type {}",
                            *inner_type);
                    }

                    else if (inner_type->is_slice()) {
                        er.report_note(inner->get_loc(),
                                       "slice inner element has type {}",
                                       *inner_type);
                    }

                    else
                        PANIC("not implemented", *inner_type);
                }

                coerce_types_and_fixup_untyped(ast, lhs_item->get_type(),
                                               rhs_item->get_type(), lhs_item,
                                               rhs_item, state);
            }

            else if (lhs_item->is_oneof(ast::NodeKind::Field)) {
                sema_expr(ast, lhs_item, state, ctx);

                coerce_types_and_fixup_untyped(ast, lhs_item->get_type(),
                                               rhs_item->get_type(), lhs_item,
                                               rhs_item, state);
            }

            else {
                er.report_error(lhs_item->get_loc(),
                                "can not assign to r-value expression {}",
                                lhs_item->get_kind());
            }

            lhs_idx++;
        }
    }

    // fixup the type of the rhs expr pack in case it had any untyped integers
    fixup_expr_pack(ts, data.lhs);
    fixup_expr_pack(ts, data.rhs);
}

// ============================================================================

void sema_return_stmt(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::unary(*node);
    node->set_type(ts.get_void());

    sema_expr(ast, data.child, state, ctx);

    if (ctx.current_function == nullptr) {
        er.report_error(node->get_loc(),
                        "can not use return statement outside of function");
        return;
    }

    auto ret_type = ctx.current_function_return_type();
    ASSERT(ret_type != nullptr);

    // bare return
    if (data.child == nullptr) {
        if (ret_type->inner.size() == 1 && ret_type->inner[0]->is_void()) {
            // void function, returning nothing. Ok
            return;
        }

        er.report_error(node->get_loc(),
                        "function expects return value of type {}, but got "
                        "none in return statement",
                        *ret_type);
        return;
    }

    auto rref = conv::return_stmt_ref(*node);
    coerce_types_and_fixup_untyped(ast, ret_type, data.child->get_type(), node,
                                   rref.child, state);
}

void sema_while_stmt(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::while_stmt(*node);
    node->set_type(ts.get_void());

    sema_expr(ast, data.cond, state, ctx);

    auto cond_type = data.cond->get_type();
    auto r = coerce_types_and_fixup_untyped(ast, ts.get_bool(), cond_type, node,
                                            data.cond, state);
    if (!r->is_bool()) {
        er.report_error(data.cond->get_loc(),
                        "can not use type {} as boolean in condition",
                        *cond_type);
        if (cond_type->is_integral()) {
            er.report_note(
                data.cond->get_loc(),
                "can not use integer as boolean directly, maybe add !=");
        }
    }

    auto sctx = ctx.with_loop(node);
    sema_stmt(ast, data.body, state, sctx);
}

void sema_if_stmt(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    auto  data = conv::if_stmt(*node);
    node->set_type(ts.get_void());

    sema_expr(ast, data.cond, state, ctx);

    auto cond_type = data.cond->get_type();
    auto r = coerce_types_and_fixup_untyped(ast, ts.get_bool(), cond_type, node,
                                            data.cond, state);
    if (!r->is_bool()) {
        er.report_error(data.cond->get_loc(),
                        "can not use type {} as boolean in condition",
                        *cond_type);
        if (cond_type->is_integral()) {
            er.report_note(
                data.cond->get_loc(),
                "can not use integer as boolean directly, maybe add !=");
        }
    }

    sema_stmt(ast, data.wt, state, ctx);
    sema_stmt(ast, data.wf, state, ctx);
}

void sema_stmt(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto& er = *state.er;
    if (node == nullptr) return;

    if (node->is_oneof(ast::NodeKind::Block)) {
        node->set_type(ts.get_void());
        sema_stmt_children(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::ExprStmt)) {
        auto data = conv::unary(*node);
        node->set_type(ts.get_void());

        sema_expr(ast, data.child, state, ctx);

        if (auto ty = data.child->get_type()->unpacked();
            !ty->is_void() && !ty->is_err()) {
            er.report_warn(node->get_loc(),
                           "discard of non-void expression of type {}",
                           *data.child->get_type());
        }

        return;
    }

    if (node->is_oneof(ast::NodeKind::ReturnStmt)) {
        sema_return_stmt(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::DeferStmt)) {
        auto data = conv::defer_stmt(*node);
        node->set_type(ts.get_void());

        sema_stmt(ast, data.stmt, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::IfStmt)) {
        sema_if_stmt(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::WhileStmt)) {
        sema_while_stmt(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::VarDecl)) {
        sema_var_decl(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::Assign)) {
        sema_assign(ast, node, state, ctx);
        return;
    }

    PANIC("not implemented", node->get_kind(),
          fmt::to_string(node->get_kind()));
}

// ============================================================================

void eval_extern_decorator(Ast& /*unused*/, Node* node, Decl* decl,
                           State& state, Context& ctx) {
    auto& er = *state.er;
    auto  data = conv::decorator(*node);

    // mark declaration as extern
    decl->flags.set_extern();

    // no parameters given, set link name to the local name
    if (data.params.size() == 0) {
        decl->link_name = decl->local_name;
        return;
    }

    auto link_name_set = false;

    // got parameters to check
    for (auto param : data.params) {
        // keyed parameter
        if (param->is_oneof(ast::NodeKind::DecoratorParam)) {
            auto data = conv::decorator_param(*param);
            if (data.key == "link_name") {
                if (link_name_set) {
                    er.report_error(param->get_loc(), "link_name already set");
                    continue;
                }

                link_name_set = true;
                auto v = eval_expr(data.value, state, ctx);
                if (v.type->is_strview()) {
                    decl->link_name = v.get_data_strv();
                } else {
                    er.report_error(param->get_loc(),
                                    "invalid type por 'link_name' parameter, "
                                    "expected string_view but got {}",
                                    *v.type);
                }
            } else {
                er.report_error(param->get_loc(),
                                "unknown parameter {:?} for 'extern'",
                                data.key);
            }
        }

        // positional parameter
        else {
            er.report_error(param->get_loc(),
                            "'extern' expects no positional arguments");
        }
    }
}

void eval_export_decorator(Ast& /*unused*/, Node* node, Decl* decl,
                           State& state, Context& ctx) {
    auto& er = *state.er;
    auto  data = conv::decorator(*node);

    // mark declaration as exported
    decl->flags.set_export();

    // no parameters given, set link name to the local name
    if (data.params.size() == 0) {
        decl->link_name = decl->local_name;
        return;
    }

    auto link_name_set = false;

    // got parameters to check
    for (auto param : data.params) {
        // keyed parameter
        if (param->is_oneof(ast::NodeKind::DecoratorParam)) {
            auto data = conv::decorator_param(*param);
            if (data.key == "link_name") {
                if (link_name_set) {
                    er.report_error(param->get_loc(), "link_name already set");
                    continue;
                }

                link_name_set = true;
                auto v = eval_expr(data.value, state, ctx);
                if (v.type->is_strview()) {
                    decl->link_name = v.get_data_strv();
                } else {
                    er.report_error(param->get_loc(),
                                    "invalid type por 'link_name' parameter, "
                                    "expected string_view but got {}",
                                    *v.type);
                }
            } else {
                er.report_error(param->get_loc(),
                                "unknown parameter {:?} for 'export'",
                                data.key);
            }
        }

        // positional parameter
        else {
            er.report_error(param->get_loc(),
                            "'export' expects no positional arguments");
        }
    }
}

void eval_decorators(Ast& ast, Node* node, Decl* decl, State& state,
                     Context& ctx) {
    auto& er = *state.er;
    auto  data = conv::decorators(*node);
    for (auto node : data.items) {
        auto data = conv::decorator(*node);

        if (data.name == "extern") {
            eval_extern_decorator(ast, node, decl, state, ctx);
        } else if (data.name == "export") {
            eval_export_decorator(ast, node, decl, state, ctx);
        } else if (data.name == "private") {
            PANIC("@private decorator has not been implemented");
        } else {
            er.report_error(node->get_loc(), "unknown decorator: {:?}",
                            data.name);
            er.report_note(node->get_loc(),
                           "custom decorators have not been implemented yet");
        }
    }
}

void sema_decorators(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto  data = conv::decorators(*node);
    node->set_type(ts.get_void());

    for (auto node : data.items) {
        auto data = conv::decorator(*node);
        node->set_type(ts.get_void());

        for (auto node : data.params) {
            auto data = conv::decorator_param(*node);

            sema_expr(ast, data.value, state, ctx);
            node->set_type(data.value ? data.value->get_type() : ts.get_void());
        }
    }
}

void sema_gargs(Ast& /*unused*/, Node* node, State& state,
                Context& /*unused*/) {
    auto& ts = *state.ts;
    auto  data = conv::func_params(*node);
    node->set_type(ts.get_void());

    if (data.params.size() > 0) {
        PANIC("generic parameters not implemented");
    }
}

void sema_args(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& ts = *state.ts;
    auto  data = conv::func_params(*node);
    node->set_type(ts.get_void());

    for (auto node : data.params) {
        auto data = conv::func_param(*node);
        sema_expr(ast, data.type, state, ctx);

        auto type = eval_expr_to_type(data.type, state, ctx);
        node->set_type(type);
    }
}

void sema_ret(Ast& ast, Node* node, State& state, Context& ctx) {
    if (node == nullptr) return;

    auto& ts = *state.ts;
    auto  data = conv::func_ret_pack(*node);
    node->set_type(ts.get_void());

    for (auto node : data.ret) {
        if (!node->is_oneof(ast::NodeKind::NamedRet)) {
            sema_expr(ast, node, state, ctx);
            continue;
        }

        auto data = conv::named_ret(*node);
        sema_expr(ast, data.type, state, ctx);
        node->set_type(ts.get_void());
    }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void namespace_func_decl(Node* ids_node, Decl* func_decl, State& state) {
    auto& er = *state.er;
    auto& ts = *state.ts;

    auto ids = conv::id_pack(*ids_node).ids;
    if (ids.size() != 2) {
        er.report_bug(ids_node->get_loc(),
                      "arbitrary nesting of namespacing on types has not "
                      "been implemented");
    }

    else {
        auto ty_node = ids[0];
        auto ty_decl = ty_node->get_decl();
        ASSERT(ty_decl != nullptr);
        ASSERT(ty_decl->value.type != nullptr);
        ASSERT(ty_decl->value.type->is_type());

        auto ty = ty_decl->value.get_data_type();
        auto name = conv::id(*ids[1]).name;
        ts.add_function_to_type(*ty, name, func_decl);

        // er.report_debug(ty_node->get_loc(), "{}.{}",
        //                 *ty_decl->value.get_data_type(), name);
    }

    // fmt::println(stderr, "all_namespaced_functions:");
    // for (auto const& [k, v] : ts.get_namespaced_functions()) {
    //     fmt::println(stderr, "- k={:?}", k);
    //
    //     for (auto const& [k, fn] : v) {
    //         fmt::println(stderr, "  - k={:?}, fn={}", k, *fn);
    //     }
    // }
}

// Perform semantic analysis of just the function header/type declaration.
//
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void sema_func_decl_header(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& er = *state.er;
    auto  data = conv::func_decl(*node);
    auto  decl = node->get_decl();
    ASSERT(decl != nullptr);

    sema_decorators(ast, data.decorators, state, ctx);
    sema_gargs(ast, data.gargs, state, ctx);
    sema_args(ast, data.args, state, ctx);
    sema_ret(ast, data.ret, state, ctx);

    eval_decorators(ast, data.decorators, decl, state, ctx);

    // functions are always const
    decl->flags.set_const();

    // handle main function. It should be exported with name `main` even
    // without
    // `@export`
    if (ctx.is_main_module() && decl->local_name == "main") {
        decl->flags.set_export();
        decl->link_name = decl->local_name;
    }

    // extern should not have body
    if (decl->flags.has_extern() && data.body != nullptr) {
        er.report_error(node->get_loc(),
                        "extern function should not have a body");
    }

    // no-extern should have body
    if (!decl->flags.has_extern() && data.body == nullptr) {
        er.report_error(node->get_loc(), "missing body for function");
    }

    auto type = eval_function_decl_type(node, state, ctx);
    node->set_type(type);

    auto d = node->get_decl();
    ASSERT(d != nullptr);
    ASSERT(d->get_type() == nullptr);

    // TODO: save the function as value. Somehow
    d->value = {.type = type};

    // if the function is namespaced, add it to the type it is namespacing under
    auto ids = data.get_name().ids;
    if (ids.size() > 1) {
        namespace_func_decl(data.name, d, state);
    }
}

// ----------------------------------------------------------------------------

void sema_func_decl(Ast& ast, Node* node, State& state, Context& sctx) {
    // auto& er = *state.er;
    auto data = conv::func_decl(*node);
    auto ctx = sctx.with_function(node);

    sema_stmt(ast, data.body, state, ctx);

#if CHECK_UNTYPED
    check_no_untyped(ast, node, er);
#endif
}

// ----------------------------------------------------------------------------

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void eval_defined_values(Node* decl, std::span<Node*> ids,
                         std::span<Node*> inits, State& state, Context& ctx) {
    auto& er = *state.er;
    auto& ts = *state.ts;
    if (ids.size() != inits.size()) {
        er.report_bug(decl->get_loc(),
                      "using multiple returns in def has not been implemented");
        return;
    }

    for (auto [id, init] : rv::zip(ids, inits)) {
        auto v = eval_expr(init, state, ctx);
        auto d = id->get_decl();
        ASSERT(d != nullptr);
        ASSERT(d->value.has_data() == false);
        ASSERT(d->value.type != nullptr);
        ASSERT(*d->value.type == *v.type);

        // wen we eval and define a type, create a distinct copy
        if (v.type->is_type()) {
            v.data = ts.new_distinct_of(d->full_name, v.get_data_type());
        }

        d->value = v;

        // set the decl to const (as it was made with `def`)
        d->flags.set_const();
    }
}

void sema_top_def_decl(Ast& ast, Node* node, State& state, Context& ctx) {
    auto& er = *state.er;
    auto& ts = *state.ts;
    auto  top_data = conv::top_def_decl(*node);

    if (!top_data.get_decorators().items.empty()) {
        er.report_bug(top_data.decorators->get_loc(),
                      "decorators on `def` not implemented");
    }

    auto data = top_data.get_decl();
    node->set_type(ts.get_void());

    sema_expr(ast, data.types, state, ctx);
    sema_expr(ast, data.inits, state, ctx);

    // we have explicit types
    if (data.types && data.inits) {
        sema_decl_with_types_and_inits(ast, data.ids, data.types, data.inits,
                                       state, ctx);
        eval_defined_values(top_data.decl, data.get_ids().ids,
                            data.get_inits().items, state, ctx);
    }

    // just types, not inits
    else if (data.types) {
        er.report_error(top_data.decl->get_loc(),
                        "declaration is missing initializers");
        er.report_note(
            top_data.decl->get_loc(),
            "initializers may be ommited on variables, but not constants");
    }

    // no explicit types but got inits
    else if (data.inits) {
        sema_decl_with_inits(ast, data.ids, data.inits, state, ctx);
        eval_defined_values(top_data.decl, data.get_ids().ids,
                            data.get_inits().items, state, ctx);
    }

    // nothing given
    else {
        er.report_error(node->get_loc(), "no types in declaration");

        auto ids = data.get_ids();
        for (auto const& id : ids.ids) {
            set_type_of_id(id, ts.get_error(), state);
        }
    }

    // fixup the type of the init expr pack
    fixup_expr_pack(ts, data.inits);

#if CHECK_UNTYPED
    check_no_untyped(ast, node, er);
#endif
}

// ----------------------------------------------------------------------------

void sema_decl(Ast& ast, Node* node, State& state, Context& ctx) {
    if (node->is_oneof(ast::NodeKind::FuncDecl,
                       ast::NodeKind::FuncDeclWithCVarArgs)) {
        sema_func_decl(ast, node, state, ctx);
        return;
    }

    if (node->is_oneof(ast::NodeKind::TopDefDecl)) {
        sema_top_def_decl(ast, node, state, ctx);
        return;
    }

    PANIC("not implemented", node->get_kind(),
          fmt::to_string(node->get_kind()));
}

// ----------------------------------------------------------------------------

void sema_module(Ast& ast, Node* node, State& state, Context& sctx) {
    ASSERT(node->get_kind() == ast::NodeKind::FlatModule);

    node->set_type(state.ts->get_void());

    auto data = conv::flat_module(*node);
    auto ctx = sctx.with_module(node);

    std::vector<Node*> pending_functions;

    for (auto child : data.children) {
        if (child->is_oneof(ast::NodeKind::FuncDecl,
                            ast::NodeKind::FuncDeclWithCVarArgs)) {
            sema_func_decl_header(ast, child, state, ctx);
            pending_functions.push_back(child);
        } else {
            sema_decl(ast, child, state, ctx);
        }
    }

    for (auto child : pending_functions) {
        sema_decl(ast, child, state, ctx);
    }
}

// ----------------------------------------------------------------------------

void sema_ast(Ast& ast, Node* root, ErrorReporter& er, types::TypeStore& ts,
              Options const& opt) {
    auto state = State{.ts = &ts, .er = &er, .opt = &opt};
    auto ctx = Context{};

    sema_module(ast, root, state, ctx);
}

}  // namespace yal::sema
