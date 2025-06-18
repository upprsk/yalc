#include "parser.hpp"

#include <charconv>
#include <cstddef>
#include <libassert/assert.hpp>
#include <span>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "location.hpp"
#include "node.hpp"
#include "tokenizer.hpp"

namespace yal {

constexpr auto is_hex_char(char c) -> bool {
    return (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ||
           (c >= '0' && c <= '9');
}

constexpr auto parse_hex_escape(char first, char second) -> uint8_t {
    std::array digits{first, second};

    uint8_t byte{};
    auto [ptr, ec] =
        std::from_chars(digits.data(), digits.data() + digits.size(), byte, 16);
    ASSERT(ec == std::errc{});
    ASSERT(ptr == (digits.data() + digits.size()));

    return byte;
}

auto escape_string(LocalErrorReporter& er, Span span, std::string_view s)
    -> std::string {
    std::string result;

    for (size_t i = 0; i < s.size(); i++) {
        if (s[i] == '\\') {
            i++;

            if (!(i < s.size())) {
                er.report_error(span, "unterminated escape sequence");
                return result;
            }

            switch (s[i]) {
                case '0': result.push_back('\0'); break;
                case 'a': result.push_back('\a'); break;
                case 'b': result.push_back('\b'); break;
                case 'e': result.push_back('\e'); break;
                case 'f': result.push_back('\f'); break;
                case 'n': result.push_back('\n'); break;
                case 'r': result.push_back('\r'); break;
                case 't': result.push_back('\t'); break;
                case 'v': result.push_back('\v'); break;
                case '\\': result.push_back('\\'); break;
                case '\'': result.push_back('\''); break;
                case '"': result.push_back('"'); break;

                case 'x': {
                    if (i + 2 < s.size() && is_hex_char(s[i + 1]) &&
                        is_hex_char(s[i + 2])) {
                        result.push_back(parse_hex_escape(s[i + 1], s[i + 2]));
                        i += 2;
                    } else {
                        result.push_back('\\');
                        result.push_back('x');
                    }
                } break;

                default:
                    er.report_warn(span.offset(i).trim_to_size(2),
                                   "unknown escape sequence: '\\{:c}'", s[i]);
                    result.push_back(s[i]);
            }
        } else {
            result.push_back(s[i]);
        }
    }

    return result;
}

auto escape_char(LocalErrorReporter& er, Span span, std::string_view s)
    -> uint32_t {
    uint32_t value = 0;

    if (s[0] == '\\') {
        ASSUME(s.size() >= 2);

        switch (s[1]) {
            case '0': value = '\0'; break;
            case 'a': value = '\a'; break;
            case 'b': value = '\b'; break;
            case 'e': value = '\e'; break;
            case 'f': value = '\f'; break;
            case 'n': value = '\n'; break;
            case 'r': value = '\r'; break;
            case 't': value = '\t'; break;
            case 'v': value = '\v'; break;
            case '\\': value = '\\'; break;
            case '\'': value = '\''; break;
            case '"': value = '"'; break;

            case 'x': {
                auto xs = s.substr(2);
                if (xs.size() == 2 && is_hex_char(xs[0]) &&
                    is_hex_char(xs[1])) {
                    value = parse_hex_escape(xs[0], xs[1]);
                } else {
                    er.report_error(
                        span, "invalid hexadecimal character literal: {:?}", s);
                }
            } break;

            default:
                er.report_warn(span, "unknown escape sequence: '\\{:c}'", s[1]);
        }
    } else {
        value = s[0];
    }

    return value;
}

class Parser {
    std::span<Token const> tokens;
    size_t                 current_token{};

    std::string_view          source;
    LocalErrorReporter const& er;
    ast::Ast&                 ast;

public:
    Parser(std::span<Token const> tokens, LocalErrorReporter const& er,
           ast::Ast& ast)
        : tokens{tokens}, source{er.get_source()}, er{er}, ast{ast} {}

    auto parse_source_file() -> ast::NodeFile* {
        // get rid of comments at the start of the file
        skip_comments();

        auto start_span = span();
        auto module_name = parse_module_decl();

        std::vector<ast::Node*> children;
        while (!is_at_end()) children.push_back(parse_top_decl());

        (void)consume(TokenType::Eof);

        return ast.new_node_file(to_loc(start_span.extend(prev_span())),
                                 children, module_name);
    }

    auto parse_module_decl() -> std::string_view {
#define consume_and_recover(...)     \
    if (!consume(__VA_ARGS__)) {     \
        recover_parse_module_decl(); \
        return "";                   \
    }

        // TODO: include more info in the error message
        consume_and_recover("module");
        auto id = peek();
        consume_and_recover(TokenType::Id);
        consume_and_recover(TokenType::Semi);

        return id.span.str(source);

#undef consume_and_recover
    }

    // ------------------------------------------------------------------------

    auto parse_top_decl() -> ast::Node* {
        // er.report_debug(span(), "parse_top_decl() got '{}'",
        // span().str(source));

        if (check("var")) return parse_top_var();
        if (check("def")) return parse_top_def();
        if (check("func")) return parse_top_func();

        er.report_error(span(), "expected top-level declaration but got '{}'",
                        span().str(source));

        return ast.new_node_err(to_loc(span()));
    }

    auto parse_top_var() -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'var' here every time
        advance();

        auto ids = parse_decl_ids();

    types_label:
        auto types =
            match(TokenType::Colon) ? parse_decl_types_or_inits() : nullptr;
    inits_label:
        auto inits =
            match(TokenType::Equal) ? parse_decl_types_or_inits() : nullptr;

        if (!consume(TokenType::Semi)) {
            while (!check_oneof(TokenType::Eof, TokenType::Colon,
                                TokenType::Equal, TokenType::Semi,
                                TokenType::Attribute, "var", "def", "func"))
                advance();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (match(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        return ast.new_node_top_var(to_loc(start_span.extend(prev_span())), ids,
                                    types, inits);
    }

    auto parse_top_def() -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'def' here every time
        advance();

        auto ids = parse_decl_ids();

    types_label:
        auto types =
            match(TokenType::Colon) ? parse_decl_types_or_inits() : nullptr;
    inits_label:
        auto inits =
            match(TokenType::Equal) ? parse_decl_types_or_inits() : nullptr;

        if (!consume(TokenType::Semi)) {
            while (!check_oneof(TokenType::Eof, TokenType::Colon,
                                TokenType::Equal, TokenType::Semi,
                                TokenType::Attribute, "var", "def", "func"))
                advance();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (match(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        return ast.new_node_top_def(to_loc(start_span.extend(prev_span())), ids,
                                    types, inits);
    }

    auto parse_top_func() -> ast::Node* { PANIC("NOT IMPLEMENTED"); }

    auto parse_decl_ids() -> ast::Node* {
        auto                    start_span = span();
        std::vector<ast::Node*> ids;

        do {
            auto ident = span();
            if (!check(TokenType::Id)) break;

            if (auto s = ident.str(source); s == "var") {
                er.report_error(start_span,
                                "can not use keyword '{}' as identifier", s);
                break;
            }

            advance();

            ids.push_back(ast.new_node_id(to_loc(ident), ident.str(source)));
            if (!match(TokenType::Comma)) break;
        } while (true);

        if (ids.empty()) return nullptr;
        return ast.new_node_pack(to_loc(start_span.extend(prev_span())), ids);
    }

    auto parse_decl_types_or_inits() -> ast::Node* {
        auto                    start_span = span();
        std::vector<ast::Node*> types;

        do {
            auto expr = parse_expr_without_recover();
            if (!expr) break;

            types.push_back(expr);
            if (!match(TokenType::Comma)) break;
        } while (true);

        if (types.empty()) return nullptr;
        return ast.new_node_pack(to_loc(start_span.extend(prev_span())), types);
    }
    // ========================================================================

    auto parse_expr_without_recover() -> ast::Node* {
        auto start_span = span();
        if (check(TokenType::Id)) {
            // make sure that we are not trying to do something stupid
            if (auto s = start_span.str(source); s == "var") {
                er.report_error(start_span,
                                "can not use keyword '{}' as identifier", s);

                return nullptr;
            }

            advance();

            return ast.new_node_id(to_loc(start_span), start_span.str(source));
        }

        if (match(TokenType::Int)) {
            // TODO: do not use replace and a dynamic string here
            auto s = std::string{start_span.str(source)};
            s.erase(begin(std::ranges::remove(s, '_')), s.end());

            uint64_t v;
            auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), v);
            if (ec != std::errc{} || ptr != s.data() + s.size()) {
                er.report_bug(start_span,
                              "invalid integer found in parser: '{}'", s);
                return ast.new_node_err(to_loc(start_span));
            }

            return ast.new_node_int(to_loc(start_span), v);
        }

        er.report_error(start_span, "expected expression, but got '{}'",
                        start_span.str(source));
        return nullptr;
    }

    auto parse_expr() -> ast::Node* {
        auto start_span = span();
        auto expr = parse_expr_without_recover();
        if (expr) return expr;

        while (!check_oneof(TokenType::Eof, TokenType::Semi)) advance();

        if (check(TokenType::Semi)) advance();

        return ast.new_node_err(to_loc(start_span));
    }

    // ========================================================================

    // Recover from parsing a module decl. Search for:
    //
    // - EOF
    // - ';' (not consumed)
    // - 'func'
    // - 'var'
    // - 'def'
    // - attribute
    void recover_parse_module_decl() {
        while (!check_oneof(TokenType::Eof, TokenType::Semi,
                            TokenType::Attribute, "func", "var", "def")) {
            advance();
        }

        if (check(TokenType::Semi)) advance();
    }

    void recover_parse_decl_ids() {
        while (!check_oneof(TokenType::Eof, TokenType::Semi, TokenType::Colon,
                            TokenType::Equal, TokenType::Attribute, "func",
                            "var", "def")) {
            advance();
        }

        if (check(TokenType::Semi)) advance();
    }

    // ========================================================================

    void skip_comments() {
        while (peek().is_comment()) advance();
    }

    // ========================================================================

    [[nodiscard]] constexpr auto peek() const -> Token {
        return tokens[current_token];
    }

    [[nodiscard]] constexpr auto peek_prev() const -> Token {
        // in case we are at the first token, return EOF. This is safe because
        // we will always have at least one token in `tokens`, the EOF token.
        if (current_token == 0) return tokens[tokens.size() - 1];
        return tokens[current_token - 1];
    }

    [[nodiscard]] constexpr auto check(TokenType tt) const -> bool {
        return peek().type == tt;
    }

    [[nodiscard]] constexpr auto check(std::string_view kw) const -> bool {
        return peek().is_kw(source, kw);
    }

    [[nodiscard]] constexpr auto check_oneof(auto... tt) -> bool {
        return (check(tt) || ...);
    }

    [[nodiscard]] constexpr auto match(TokenType tt) -> bool {
        if (!check(tt)) return false;

        advance();
        return true;
    }

    [[nodiscard]] constexpr auto match(std::string_view kw) -> bool {
        if (!check(kw)) return false;

        advance();
        return true;
    }

    [[nodiscard]] constexpr auto match_oneof(auto... tt) -> bool {
        auto c = (check(tt) || ...);
        if (!c) return false;

        advance();
        return true;
    }

    template <typename... T>
    [[nodiscard]] constexpr auto consume(TokenType                tt,
                                         fmt::format_string<T...> fmt,
                                         T&&... args) -> bool {
        return consume_impl(tt, fmt, fmt::make_format_args(args...));
    }

    [[nodiscard]] constexpr auto consume(TokenType tt) -> bool {
        if (match(tt)) return true;

        if (peek().has_chars())
            er.report_error(span(), "expected {}, but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected {}, but got {}", tt, peek().type);

        return false;
    }

    [[nodiscard]] constexpr auto consume(std::string_view tt) -> bool {
        if (match(tt)) return true;

        if (peek().has_chars())
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected '{}', but got {}", tt,
                            peek().type);

        return false;
    }

    [[nodiscard]] constexpr auto consume_impl(TokenType        tt,
                                              fmt::string_view fmt,
                                              fmt::format_args args) -> bool {
        if (match(tt)) return true;

        if (peek().has_chars())
            er.report_error(span(), "expected {}, but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected {}, but got {}", tt, peek().type);

        er.vreport_note(span(), fmt, args);

        return false;
    }

    [[nodiscard]] constexpr auto span() const -> Span { return peek().span; }
    [[nodiscard]] constexpr auto prev_span() const -> Span {
        return peek_prev().span;
    }

    [[nodiscard]] constexpr auto to_loc(Span s) const -> Location {
        return {.fileid = er.get_fileid(), .span = s};
    }

    [[nodiscard]] constexpr auto is_at_end() const -> bool {
        return peek().is_eof();
    }

    constexpr void advance() {
        do {
            if (!is_at_end()) current_token++;
        } while (peek().is_comment());
    }
};

auto parse_into_ast(std::span<Token const> tokens, ast::Ast& ast,
                    LocalErrorReporter const& er) -> ast::NodeFile* {
    auto p = Parser{tokens, er, ast};
    return p.parse_source_file();
}

}  // namespace yal
