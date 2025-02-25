#pragma once

#include <string_view>
#include <vector>

#include "error_reporter.hpp"
#include "span.hpp"

namespace yal {

enum class TokenType : uint8_t {
    Err,
    Equal,
    EqualEqual,
    EqualGreater,
    Less,
    LessLess,
    LessLessEqual,
    LessEqual,
    Greater,
    GreaterGreater,
    GreaterEqual,
    GreaterGreaterEqual,
    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Star,
    StarStar,
    StarEqual,
    Slash,
    SlashEqual,
    Bang,
    BangEqual,
    Ampersand,
    AmpersandEqual,
    Pipe,
    PipeEqual,
    Carrot,
    CarrotEqual,
    Tilde,
    TildeEqual,
    Semi,
    Colon,
    Comma,
    Dot,
    DotDot,
    DotStar,
    DotEqual,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Id,
    Int,
    Hex,
    Str,
    Comment,
    Eof,
};

struct Token {
    TokenType type;
    Span      span;

    [[nodiscard]] constexpr auto is_eof() const -> bool {
        return type == TokenType::Eof;
    }

    constexpr auto operator==(Token const& o) const -> bool = default;
};

auto tokenize(std::string_view source, ErrorReporter& er) -> std::vector<Token>;
}  // namespace yal

template <>
struct fmt::formatter<yal::TokenType> : formatter<string_view> {
    auto format(yal::TokenType t, format_context& ctx) const
        -> format_context::iterator;
};

template <>
struct fmt::formatter<yal::Token> {
    constexpr auto parse(format_parse_context& ctx)
        -> format_parse_context::iterator {
        return ctx.begin();
    }

    auto format(yal::Token t, format_context& ctx) const
        -> format_context::iterator;
};
