#pragma once

#include <string_view>
#include <vector>

#include "error_reporter.hpp"
#include "location.hpp"
#include "macros.hpp"

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
    StarEqual,
    Slash,
    SlashEqual,
    Bang,
    BangEqual,
    Percent,
    PercentEqual,
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
    DotDotDot,
    DotStar,
    DotEqual,
    DotLbrace,
    Question,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Id,
    Decorator,
    Int,
    Hex,
    Float,
    Str,
    Char,
    Comment,
    Eof,
};

struct Token {
    TokenType type;
    Span      span;

    [[nodiscard]] constexpr auto is_eof() const -> bool {
        return type == TokenType::Eof;
    }

    [[nodiscard]] constexpr auto is_int() const -> bool {
        return type == TokenType::Int;
    }

    [[nodiscard]] constexpr auto is_hex() const -> bool {
        return type == TokenType::Hex;
    }

    [[nodiscard]] constexpr auto is_comment() const -> bool {
        return type == TokenType::Comment;
    }

    [[nodiscard]] constexpr auto is_id() const -> bool {
        return type == TokenType::Id;
    }

    [[nodiscard]] constexpr auto is_kw(std::string_view src,
                                       std::string_view kw) const -> bool {
        return is_id() && span.str(src) == kw;
    }

    constexpr auto operator==(Token const& o) const -> bool = default;
};

auto tokenize(std::string_view source, LocalErrorReporter& er)
    -> std::vector<Token>;

void to_json(nlohmann::json& j, TokenType const& n);
void to_json(nlohmann::json& j, Token const& t);
}  // namespace yal

define_formatter_from_string_view(yal::TokenType);
define_formatter_from_string_view(yal::Token);
