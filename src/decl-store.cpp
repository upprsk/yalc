#include "decl-store.hpp"

namespace yal {}

auto fmt::formatter<yal::DeclId>::format(yal::DeclId     did,
                                         format_context &ctx) const
    -> format_context::iterator {
    if (!did.is_valid()) return fmt::format_to(ctx.out(), "DeclId(<invalid>)");
    return fmt::format_to(ctx.out(), "DeclId({})", did.value());
}
