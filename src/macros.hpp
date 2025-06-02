#pragma once

#define define_formatter_from_string_view(T)               \
    template <>                                            \
    struct fmt::formatter<T> : formatter<string_view> {    \
        auto format(T const& p, format_context& ctx) const \
            -> format_context::iterator;                   \
    }
