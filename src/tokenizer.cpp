#include "tokenizer.hpp"

#include <cstdint>

#include "error_reporter.hpp"
#include "nlohmann/json.hpp"

using json = nlohmann::json;

namespace yal {

struct Tokenizer {
    [[nodiscard]] constexpr auto is_at_end() const -> bool {
        return current == source.length();
    }

    constexpr void advance() {
        if (!is_at_end()) current++;
    }

    [[nodiscard]] constexpr auto peek() const -> uint8_t {
        if (is_at_end()) return 0;
        return source.at(current);
    }

    [[nodiscard]] constexpr auto peek_next() const -> uint8_t {
        if (current >= source.length() - 1) return 0;
        return source.at(current + 1);
    }

    constexpr auto peek_and_advance() -> uint8_t {
        auto c = peek();
        advance();
        return c;
    }

    constexpr auto match(uint8_t c) -> bool {
        if (peek() == c) {
            advance();
            return true;
        }

        return false;
    }

    [[nodiscard]] constexpr auto span() const -> Span {
        return {.begin = start, .end = current};
    }

    [[nodiscard]] constexpr auto mkt(TokenType t) const -> Token {
        return {.type = t, .span = span()};
    }

    // ------------------------------------------------------------------------

    auto tokenize_all() -> std::vector<Token> {
        std::vector<Token> tokens;

        skip_shbang();

        while (true) {
            auto t = tokenize_one();
            if (t.is_lparen()) {
                balancing_stack.push_back({TokenType::Rparen, t.span});
            } else if (t.is_lbrace()) {
                balancing_stack.push_back({TokenType::Rbrace, t.span});
            } else if (t.is_lbracket()) {
                balancing_stack.push_back({TokenType::Rbracket, t.span});
            } else if (t.is_rparen() || t.is_rbrace() || t.is_rbracket()) {
                while (!balancing_stack.empty()) {
                    auto top = *(balancing_stack.end() - 1);
                    balancing_stack.pop_back();
                    if (top.type == t.type) break;

                    er->report_error(t.span, "unbalanced '{}'",
                                     top.span.str(source));
                    er->report_note(top.span, "'{}' opened here and not closed",
                                    top.span.str(source));

                    tokens.push_back({top.type, t.span});
                }
            }

            tokens.push_back(t);

            if (t.is_eof()) break;
        }

        return tokens;
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    auto tokenize_one() -> Token {
        skip_whitespace();

        start = current;
        if (is_at_end()) return mkt(TokenType::Eof);

        auto c = peek_and_advance();
        switch (c) {
            case '=':
                if (match('=')) return mkt(TokenType::EqualEqual);
                if (match('>')) return mkt(TokenType::EqualGreater);
                return mkt(TokenType::Equal);
            case '<':
                if (match('=')) return mkt(TokenType::LessEqual);
                if (match('<')) {
                    if (match('=')) return mkt(TokenType::LessLessEqual);
                    return mkt(TokenType::LessLess);
                }
                return mkt(TokenType::Less);
            case '>':
                if (match('=')) return mkt(TokenType::GreaterEqual);
                if (match('>')) {
                    if (match('=')) return mkt(TokenType::GreaterGreaterEqual);
                    return mkt(TokenType::GreaterGreater);
                }
                return mkt(TokenType::Greater);
            case '+':
                if (match('=')) return mkt(TokenType::PlusEqual);
                if (match('+')) return mkt(TokenType::PlusPlus);
                return mkt(TokenType::Plus);
            case '-':
                if (match('=')) return mkt(TokenType::MinusEqual);
                if (match('-')) return mkt(TokenType::MinusMinus);
                return mkt(TokenType::Minus);
            case '*':
                if (match('=')) return mkt(TokenType::StarEqual);
                return mkt(TokenType::Star);
            case '/':
                if (match('=')) return mkt(TokenType::SlashEqual);
                if (match('/')) return tokenize_comment();
                return mkt(TokenType::Slash);
            case '!':
                if (match('=')) return mkt(TokenType::BangEqual);
                return mkt(TokenType::Bang);
            case '%':
                if (match('=')) return mkt(TokenType::PercentEqual);
                return mkt(TokenType::Percent);
            case '&':
                if (match('=')) return mkt(TokenType::AmpersandEqual);
                return mkt(TokenType::Ampersand);
            case '|':
                if (match('=')) return mkt(TokenType::PipeEqual);
                return mkt(TokenType::Pipe);
            case '^':
                if (match('=')) return mkt(TokenType::CarrotEqual);
                return mkt(TokenType::Carrot);
            case '~':
                if (match('=')) return mkt(TokenType::TildeEqual);
                return mkt(TokenType::Tilde);
            case ':': return mkt(TokenType::Colon);
            case ';': return mkt(TokenType::Semi);
            case ',': return mkt(TokenType::Comma);
            case '.':
                if (match('.')) {
                    if (match('.')) return mkt(TokenType::DotDotDot);
                    return mkt(TokenType::DotDot);
                }
                if (match('=')) return mkt(TokenType::DotEqual);
                if (match('*')) return mkt(TokenType::DotStar);
                if (match('{')) return mkt(TokenType::DotLbrace);
                return mkt(TokenType::Dot);
            case '?': return mkt(TokenType::Question);
            case '(': return mkt(TokenType::Lparen);
            case ')': return mkt(TokenType::Rparen);
            case '{': return mkt(TokenType::Lbrace);
            case '}': return mkt(TokenType::Rbrace);
            case '[': return mkt(TokenType::Lbracket);
            case ']': return mkt(TokenType::Rbracket);
            case 'a' ... 'z':
            case 'A' ... 'Z':
            case '_': return tokenize_id();
            case '@': return tokenize_decorator();
            case '0' ... '9': return tokenize_number();
            case '"': return tokenize_string();
            case '\'': return tokenize_char();
            default:
                er->report_error(span(), "invalid character found '{:#c}'", c);
                return mkt(TokenType::Err);
        }
    }

    constexpr auto tokenize_number() -> Token {
        if (match('x')) {
            while (is_hex_digit(peek()) || peek() == '_') advance();

            return mkt(TokenType::Hex);
        }

        while (is_digit(peek()) || peek() == '_') advance();

        if (peek() == '.' && is_digit(peek_next())) {
            // eat the .
            advance();

            while (is_digit(peek()) || peek() == '_') advance();
            return mkt(TokenType::Float);
        }

        return mkt(TokenType::Int);
    }

    constexpr auto tokenize_id() -> Token {
        while (is_alpha(peek()) || is_digit(peek()) || peek() == '_') advance();

        return mkt(TokenType::Id);
    }

    constexpr auto tokenize_decorator() -> Token {
        if (!is_alpha(peek()) && !is_digit(peek()) && peek() != '_') {
            // a lonely '@'
            er->report_error(span(), "invalid attribute, missing identifier");

            return mkt(TokenType::Err);
        }

        while (is_alpha(peek()) || is_digit(peek()) || peek() == '_') advance();

        return mkt(TokenType::Attribute);
    }

    constexpr auto tokenize_string() -> Token {
        while (!is_at_end() && peek() != '"') {
            if (peek() == '\\') advance();
            advance();

            if (peek() == '\n') break;
        }

        if (!match('"')) {
            er->report_error(span(), "unterminated string");
        }

        return mkt(TokenType::Str);
    }

    constexpr auto tokenize_char() -> Token {
        while (!is_at_end() && peek() != '\'') {
            if (peek() == '\\') advance();
            advance();
        }

        if (!match('\'')) {
            er->report_error(span(), "unterminated character");
        }

        return mkt(TokenType::Char);
    }

    constexpr auto tokenize_comment() -> Token {
        while (!is_at_end() && peek() != '\n') advance();

        return mkt(TokenType::Comment);
    }

    constexpr void skip_whitespace() {
        while (is_whitespace(peek())) advance();
    }

    constexpr void skip_shbang() {
        skip_whitespace();
        if (source.substr(current).starts_with("#!")) {
            while (!is_at_end() && peek() != '\n') advance();
        }
    }

    // ------------------------------------------------------------------------

    constexpr static auto is_digit(uint8_t c) -> bool {
        return c >= '0' && c <= '9';
    }

    constexpr static auto is_hex_digit(uint8_t c) -> bool {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
               (c >= 'A' && c <= 'F');
    }

    constexpr static auto is_alpha(uint8_t c) -> bool {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    constexpr static auto is_whitespace(uint8_t c) -> bool {
        return c == '\n' || c == '\r' || c == '\t' || c == ' ';
    }

    // ------------------------------------------------------------------------

    std::string_view          source;
    LocalErrorReporter const* er;
    uint32_t                  start{};
    uint32_t                  current{};

    std::vector<Token> balancing_stack;
};

// ------------------------------------------------------------------------

auto tokenize(LocalErrorReporter const& er) -> std::vector<Token> {
    auto tokenizer = Tokenizer{.source = er.get_source(), .er = &er};
    return tokenizer.tokenize_all();
}

void to_json(json& j, TokenType const& n) { j = fmt::to_string(n); }

void to_json(json& j, Token const& t) {
    j = json{
        {"type", t.type},
        {"span", t.span},
    };
}

}  // namespace yal

auto fmt::formatter<yal::TokenType>::format(yal::TokenType const& p,
                                            format_context&       ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (p) {
        case yal::TokenType::Err: name = "Err"; break;
        case yal::TokenType::Equal: name = "Equal"; break;
        case yal::TokenType::EqualEqual: name = "EqualEqual"; break;
        case yal::TokenType::EqualGreater: name = "EqualGreater"; break;
        case yal::TokenType::Less: name = "Less"; break;
        case yal::TokenType::LessLess: name = "LessLess"; break;
        case yal::TokenType::LessLessEqual: name = "LessLessEqual"; break;
        case yal::TokenType::LessEqual: name = "LessEqual"; break;
        case yal::TokenType::Greater: name = "Greater"; break;
        case yal::TokenType::GreaterGreater: name = "GreaterGreater"; break;
        case yal::TokenType::GreaterEqual: name = "GreaterEqual"; break;
        case yal::TokenType::GreaterGreaterEqual:
            name = "GreaterGreaterEqual";
            break;
        case yal::TokenType::Plus: name = "Plus"; break;
        case yal::TokenType::PlusPlus: name = "PlusPlus"; break;
        case yal::TokenType::PlusEqual: name = "PlusEqual"; break;
        case yal::TokenType::Minus: name = "Minus"; break;
        case yal::TokenType::MinusMinus: name = "MinusMinus"; break;
        case yal::TokenType::MinusEqual: name = "MinusEqual"; break;
        case yal::TokenType::Star: name = "Star"; break;
        case yal::TokenType::StarEqual: name = "StarEqual"; break;
        case yal::TokenType::Slash: name = "Slash"; break;
        case yal::TokenType::SlashEqual: name = "SlashEqual"; break;
        case yal::TokenType::Bang: name = "Bang"; break;
        case yal::TokenType::BangEqual: name = "BangEqual"; break;
        case yal::TokenType::Percent: name = "Percent"; break;
        case yal::TokenType::PercentEqual: name = "PercentEqual"; break;
        case yal::TokenType::Ampersand: name = "Ampersand"; break;
        case yal::TokenType::AmpersandEqual: name = "AmpersandEqual"; break;
        case yal::TokenType::Pipe: name = "Pipe"; break;
        case yal::TokenType::PipeEqual: name = "PipeEqual"; break;
        case yal::TokenType::Carrot: name = "Carrot"; break;
        case yal::TokenType::CarrotEqual: name = "CarrotEqual"; break;
        case yal::TokenType::Tilde: name = "Tilde"; break;
        case yal::TokenType::TildeEqual: name = "TildeEqual"; break;
        case yal::TokenType::Semi: name = "Semi"; break;
        case yal::TokenType::Colon: name = "Colon"; break;
        case yal::TokenType::Comma: name = "Comma"; break;
        case yal::TokenType::Dot: name = "Dot"; break;
        case yal::TokenType::DotDot: name = "DotDot"; break;
        case yal::TokenType::DotDotDot: name = "DotDotDot"; break;
        case yal::TokenType::DotStar: name = "DotStar"; break;
        case yal::TokenType::DotEqual: name = "DotEqual"; break;
        case yal::TokenType::DotLbrace: name = "DotLbrace"; break;
        case yal::TokenType::Question: name = "Question"; break;
        case yal::TokenType::Lparen: name = "Lparen"; break;
        case yal::TokenType::Rparen: name = "Rparen"; break;
        case yal::TokenType::Lbrace: name = "Lbrace"; break;
        case yal::TokenType::Rbrace: name = "Rbrace"; break;
        case yal::TokenType::Lbracket: name = "Lbracket"; break;
        case yal::TokenType::Rbracket: name = "Rbracket"; break;
        case yal::TokenType::Id: name = "Id"; break;
        case yal::TokenType::Attribute: name = "Attribute"; break;
        case yal::TokenType::Int: name = "Int"; break;
        case yal::TokenType::Float: name = "Float"; break;
        case yal::TokenType::Hex: name = "Hex"; break;
        case yal::TokenType::Str: name = "Str"; break;
        case yal::TokenType::Char: name = "Char"; break;
        case yal::TokenType::Comment: name = "Comment"; break;
        case yal::TokenType::Eof: name = "EOF"; break;
    }
    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yal::Token>::format(yal::Token const& p,
                                        format_context&   ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}}}", p.type, p.span);
}
