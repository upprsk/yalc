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

auto escape_string(LocalErrorReporter const& er, Span span, std::string_view s)
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

    ParseOptions const& opt;

public:
    Parser(std::span<Token const> tokens, LocalErrorReporter const& er,
           ParseOptions const& opt, ast::Ast& ast)
        : tokens{tokens}, source{er.get_source()}, er{er}, ast{ast}, opt{opt} {}

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
        if (opt.verbose) {
            er.report_debug(span(), "parse_top_decl() got '{}'",
                            span().str(source));
        }

        auto attributes =
            check(TokenType::Attribute) ? parse_attributes() : nullptr;

        if (check("var")) return parse_top_var(attributes);
        if (check("def")) return parse_top_def(attributes);
        if (check("func")) return parse_func(attributes);

        er.report_error(span(), "expected top-level declaration but got '{}'",
                        span().str(source));

        auto err = ast.new_node_err(to_loc(span()));
        recover_parse_top_decl();

        return err;
    }

    // ------------------------------------------------------------------------

    auto parse_attributes() -> ast::Node* {
        auto start_span = span();

        std::vector<ast::Node*> attrs;
        while (check(TokenType::Attribute)) {
            auto attr = parse_attribute();
            attrs.push_back(attr);
        }

        return ast.new_node_pack(to_loc(start_span.extend(prev_span())), attrs);
    }

    auto parse_attribute() -> ast::Node* {
        auto start_span = span();

        // name of the attribute without the leading '@'
        auto attribute_name = start_span.str(source).substr(1);
        // NOTE: we have an unconsumed '@something' here every time
        advance();

        std::vector<ast::Node*> args;

        // we have arguments for the attribute
        if (match(TokenType::Lparen)) {
            do {
                if (check(TokenType::Rparen)) break;

                if (check(TokenType::Id) && check_next(TokenType::Equal)) {
                    // this is a key-value pair
                    auto key = span();

                    // advance the key and the =
                    advance();
                    advance();

                    auto value = parse_expr_without_recover();
                    if (!value) {
                        value = ast.new_node_err(to_loc(prev_span()));
                        recover_parse_attribute_value();
                    }

                    args.push_back(
                        ast.new_attributekv(to_loc(key.extend(prev_span())),
                                            key.str(source), value));
                }

                else {
                    // this is a lonely value
                    auto value = parse_expr_without_recover();
                    if (!value) {
                        recover_parse_attribute_value();
                    } else {
                        args.push_back(value);
                    }
                }
            } while (match(TokenType::Comma));

            if (!consume(TokenType::Rparen)) {
                // FIXME: recover missing parenthesis
            }
        }

        return ast.new_attribute(to_loc(start_span), attribute_name, args);
    }

    // ------------------------------------------------------------------------

    auto parse_top_var(ast::Node* attributes) -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'var' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types =
            match(TokenType::Colon) ? parse_decl_types_or_inits() : nullptr;
    inits_label:
        auto inits =
            match(TokenType::Equal) ? parse_decl_types_or_inits() : nullptr;

        if (!consume(TokenType::Semi)) {
            recover_parse_top_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        return ast.new_node_top_var(to_loc(start_span.extend(prev_span())),
                                    attributes, names, types, inits);
    }

    auto parse_top_def(ast::Node* attributes) -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'def' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types =
            match(TokenType::Colon) ? parse_decl_types_or_inits() : nullptr;
    inits_label:
        auto inits =
            match(TokenType::Equal) ? parse_decl_types_or_inits() : nullptr;

        if (!consume(TokenType::Semi)) {
            recover_parse_top_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        return ast.new_node_top_def(to_loc(start_span.extend(prev_span())),
                                    attributes, names, types, inits);
    }

    auto parse_decl_ids() -> ast::Node* {
        auto                    start_span = span();
        std::vector<ast::Node*> ids;

        do {
            auto ident = span();
            if (!check(TokenType::Id)) break;

            if (is_kw_and_report(ident)) break;

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

    // ------------------------------------------------------------------------

    auto parse_func(ast::Node* attributes) -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'func' here every time
        advance();

        std::string_view name;
        auto             name_span = span();
        if (!consume_id_non_kw()) {
            if (auto r = recover_parse_func_name(start_span)) return r;
        } else {
            name = name_span.str(source);
        }

        std::string_view attached_type;
        if (match(TokenType::Dot)) {
            attached_type = name;

            name_span = span();
            if (!consume_id_non_kw()) {
                if (auto r = recover_parse_func_name_with_attached_type(
                        start_span, attributes, name, attached_type))
                    return r;
            } else {
                name = name_span.str(source);
            }
        }

        ast::Node* gargs = nullptr;
        if (check(TokenType::Lbracket)) gargs = parse_func_gargs();

        auto [args, is_c_varargs] = parse_func_args();

        ast::Node* ret = nullptr;
        if (!check(TokenType::Lbrace) && !check(TokenType::Semi) &&
            !check(TokenType::Eof))
            ret = parse_func_ret();

        ast::Node* body = nullptr;
        if (check(TokenType::Lbrace))
            body = parse_block();
        else
            (void)consume(TokenType::Semi,
                          "expected either a function body or a ';'");

        return ast.new_node_func(to_loc(start_span.extend(prev_span())),
                                 attributes, name, attached_type, gargs, args,
                                 ret, body, is_c_varargs);
    }

    auto parse_func_gargs() -> ast::Node* {
        auto start_span = span();
        auto had_error = false;

        if (!consume(TokenType::Lbracket))
            return ast.new_node_err(to_loc(start_span.extend(prev_span())));

        std::vector<ast::Node*> args;
        while (!check(TokenType::Rbracket)) {
            auto arg = parse_func_arg();
            if (arg) args.push_back(arg);

            if (check(TokenType::Rbracket)) break;
            if (!consume(
                    TokenType::Comma,
                    "expected ',' to separate generic function arguments")) {
                had_error = true;

                recover_parse_func_garg();
                if (!match(TokenType::Comma)) break;
            }
        }

        // NOTE: we want to do something when this fails?
        (void)consume(TokenType::Rbracket,
                      "expected ']' after generic function arguments");

        auto s = start_span.extend(prev_span());
        if (!had_error && args.empty()) {
            er.report_error(s, "generic argument list is empty");
        }

        return ast.new_node_pack(to_loc(s), args);
    }

    auto parse_func_args() -> std::pair<ast::Node*, bool> {
        auto start_span = span();

        if (!consume(TokenType::Lparen))
            return std::make_pair(
                ast.new_node_err(to_loc(start_span.extend(prev_span()))),
                false);

        std::vector<ast::Node*> args;
        while (!check(TokenType::Rparen)) {
            if (check(TokenType::DotDotDot)) break;

            auto arg = parse_func_arg();
            if (arg) args.push_back(arg);

            if (check(TokenType::Rparen)) break;
            if (!consume(TokenType::Comma,
                         "expected ',' to separate function arguments")) {
                recover_parse_func_arg();
                if (!match(TokenType::Comma)) break;
            }
        }

        auto is_c_varargs = false;
        if (match(TokenType::DotDotDot)) {
            is_c_varargs = true;

            // allow a trailing comma after the dot, but we don't care about it
            (void)match(TokenType::Comma);
        }

        // NOTE: we want to do something when this fails?
        (void)consume(TokenType::Rparen,
                      "expected ')' after function arguments");

        return std::make_pair(
            ast.new_node_pack(to_loc(start_span.extend(prev_span())), args),
            is_c_varargs);
    }

    auto parse_func_arg() -> ast::Node* {
        auto start_span = span();
        if (is_kw_and_report(start_span) ||
            !consume(TokenType::Id, "expected argument name"))
            return nullptr;

        auto       name = start_span.str(source);
        ast::Node* type{};
        if (match(TokenType::Colon)) {
            type = parse_expr_without_recover();
        }

        return ast.new_node_func_arg(to_loc(start_span.extend(prev_span())),
                                     name, type);
    }

    auto parse_func_ret() -> ast::Node* {
        if (match(TokenType::Lparen)) {
            auto start_span = prev_span();
            auto had_error = false;

            std::vector<ast::Node*> rets;
            while (!check(TokenType::Rparen)) {
                auto ret = parse_func_multi_ret_item();
                if (ret) rets.push_back(ret);

                if (check(TokenType::Rparen)) break;
                if (!consume(TokenType::Comma,
                             "expected ',' to separate return value types")) {
                    had_error = true;
                    recover_parse_func_ret();
                    if (!match(TokenType::Comma)) break;
                }
            }

            // NOTE: we want to do something when this fails?
            (void)consume(TokenType::Rparen,
                          "expected ')' after function return value types");

            auto s = start_span.extend(prev_span());
            if (!had_error && rets.empty()) {
                er.report_error(s, "return list is empty");
            }

            return ast.new_node_pack(to_loc(s), rets);
        }

        // NOTE: may want custom error handling here?
        auto ret = parse_expr_without_recover();
        if (ret)
            ret = ast.new_node_pack(ret ? ret->get_loc() : to_loc(prev_span()),
                                    std::array{ret});
        return ret;
    }

    auto parse_func_multi_ret_item() -> ast::Node* {
        // this is a named return, we don't have that yet
        if (check(TokenType::Id) && check_next(TokenType::Colon)) {
            PANIC("NOT IMPLEMENTED");
        }

        return parse_expr_without_recover();
    }

    // ========================================================================

    auto parse_block() -> ast::Node* {
        auto start_span = span();

        if (!consume(TokenType::Lbrace)) PANIC("handle missing '{' in block");

        std::vector<ast::Node*> children;
        while (!is_at_end() && !check(TokenType::Rbrace)) {
            auto stmt = parse_stmt();
            children.push_back(stmt);
        }

        (void)consume(TokenType::Rbrace);

        return ast.new_node_block(to_loc(start_span.extend(prev_span())),
                                  children);
    }

    // ========================================================================

    auto parse_expr_without_recover() -> ast::Node* {
        auto start_span = span();
        if (check(TokenType::Id)) {
            // make sure that we are not trying to do something stupid
            if (is_kw_and_report(start_span)) return nullptr;

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

        if (match(TokenType::Str)) {
            auto s = start_span.str(source);
            s = s.substr(1, s.size() - 2);

            auto result = escape_string(er, start_span, s);
            return ast.new_node_string(to_loc(start_span), result);
        }

        er.report_error(start_span, "expected expression, but got '{}'",
                        start_span.str(source));
        return nullptr;
    }

    auto parse_expr() -> ast::Node* {
        auto start_span = span();
        auto expr = parse_expr_without_recover();
        if (expr) return expr;

        recover_parse_expr();
        return ast.new_node_err(to_loc(start_span.extend(prev_span())));
    }

    // ========================================================================

    auto parse_stmt() -> ast::Node* {
        if (opt.verbose) {
            er.report_debug(span(), "parse_stmt() got '{}'",
                            span().str(source));
        }

        auto start_span = span();
        if (check("var")) return parse_var();
        if (check("def")) return parse_def();
        if (check("return")) return parse_return_stmt();

        auto expr = parse_expr();
        (void)!consume(TokenType::Semi);

        auto s = start_span.extend(prev_span());
        return ast.new_node_expr_stmt(to_loc(s), expr);
    }

    auto parse_var() -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'var' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types =
            match(TokenType::Colon) ? parse_decl_types_or_inits() : nullptr;
    inits_label:
        auto inits =
            match(TokenType::Equal) ? parse_decl_types_or_inits() : nullptr;

        if (!consume(TokenType::Semi)) {
            recover_parse_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        return ast.new_node_var(to_loc(start_span.extend(prev_span())), names,
                                types, inits);
    }

    auto parse_def() -> ast::Node* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'def' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types =
            match(TokenType::Colon) ? parse_decl_types_or_inits() : nullptr;
    inits_label:
        auto inits =
            match(TokenType::Equal) ? parse_decl_types_or_inits() : nullptr;

        if (!consume(TokenType::Semi)) {
            recover_parse_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        return ast.new_node_def(to_loc(start_span.extend(prev_span())), names,
                                types, inits);
    }

    auto parse_return_stmt() -> ast::Node* {
        auto start_span = span();

        // skip over the 'return'
        advance();

        std::vector<ast::Node*> rets;
        while (!check(TokenType::Semi)) {
            auto ret = parse_expr_without_recover();
            if (ret) rets.push_back(ret);

            if (check(TokenType::Semi)) break;
            if (!consume(TokenType::Comma,
                         "expected ',' to separate return values"))
                break;
        }

        // NOTE: we want to do something when this fails?
        (void)consume(TokenType::Semi,
                      "expected ';' after function return values");

        return ast.new_node_return(to_loc(start_span.extend(prev_span())),
                                   rets);
    }

    // ========================================================================

    void recover_parse_module_decl() {
        skip_while_not(TokenType::Eof, TokenType::Semi, TokenType::Attribute,
                       "func", "var", "def");

        if (check(TokenType::Semi)) advance();
    }

    void recover_parse_top_decl() {
        skip_while_not(TokenType::Eof, TokenType::Attribute, "var", "def",
                       "func");
    }

    void recover_parse_attribute_value() {
        skip_while_not(TokenType::Eof, TokenType::Semi, TokenType::Rparen,
                       TokenType::Comma, TokenType::Attribute, "func", "var",
                       "def");
    }

    void recover_parse_top_def_or_var() {
        skip_while_not(TokenType::Eof, TokenType::Colon, TokenType::Equal,
                       TokenType::Semi, TokenType::Attribute, "var", "def",
                       "func");
    }

    void recover_parse_def_or_var() {
        skip_while_not(TokenType::Eof, TokenType::Colon, TokenType::Equal,
                       TokenType::Semi, TokenType::Attribute, "var", "def",
                       "func", "return");
    }

    void recover_parse_func_garg() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Rbracket,
                       TokenType::Lbrace, TokenType::Lparen, TokenType::Semi,
                       TokenType::Attribute, "var", "def", "func");
    }

    void recover_parse_func_arg() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Rparen,
                       TokenType::Lbrace, TokenType::Semi, TokenType::Attribute,
                       "var", "def", "func");
    }

    void recover_parse_func_ret() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Rparen,
                       TokenType::Semi, TokenType::Lbrace, TokenType::Attribute,
                       "var", "def", "func");
    }

    void recover_parse_expr() {
        skip_while_not(TokenType::Eof, TokenType::Semi, "var", "def", "func",
                       "return");
        if (check(TokenType::Semi)) advance();
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto recover_parse_func_name(Span const& start_span)
        -> ast::Node* {
        skip_while_not(TokenType::Eof, TokenType::Dot, TokenType::Lbracket,
                       TokenType::Lparen, TokenType::Semi, TokenType::Attribute,
                       "var", "def", "func");

        // in case we are at the end, just abort
        if (is_at_end())
            return ast.new_node_err(to_loc(start_span.extend(prev_span())));

        // too far, we can not recover this
        if (check(TokenType::Semi) || is_kw(span())) {
            (void)match(TokenType::Semi);
            return ast.new_node_err(to_loc(start_span.extend(prev_span())));
        }

        return nullptr;
    }

    [[nodiscard]] auto recover_parse_func_name_with_attached_type(
        Span const& start_span, ast::Node* attributes, std::string_view name,
        std::string_view attached_type) -> ast::Node* {
        skip_while_not(TokenType::Eof, TokenType::Lbracket, TokenType::Lparen,
                       TokenType::Semi, TokenType::Attribute, "var", "def",
                       "func");

        auto s = start_span.extend(prev_span());

        // in case we are at the end, just abort
        if (is_at_end())
            return ast.new_node_func(to_loc(s), attributes, name, attached_type,
                                     nullptr, nullptr, nullptr, nullptr, false);

        // too far, we can not recover this
        if (check(TokenType::Semi) || is_kw(span())) {
            (void)match(TokenType::Semi);
            return ast.new_node_func(to_loc(s), attributes, name, attached_type,
                                     nullptr, nullptr, nullptr, nullptr, false);
        }

        return nullptr;
    }

    // ========================================================================

    void skip_comments() {
        while (peek().is_comment()) advance();
    }

    void skip_while_not(auto&&... args) {
        while (!check_oneof(std::forward<decltype(args)>(args)...)) advance();
    }

    // ========================================================================

    [[nodiscard]] constexpr auto is_kw(Token const& t) const -> bool {
        return is_kw(t.span);
    }

    [[nodiscard]] constexpr auto is_kw(Span const& s) const -> bool {
        return is_kw(s.str(source));
    }

    [[nodiscard]] constexpr auto is_kw(std::string_view s) const -> bool {
        return s == "var" || s == "def" || s == "func" || s == "return";
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

    [[nodiscard]] constexpr auto check_next(TokenType tt) const -> bool {
        if (is_at_end()) return false;

        return tokens[current_token + 1].type == tt;
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
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            peek().type);

        return false;
    }

    [[nodiscard]] constexpr auto consume(std::string_view tt) -> bool {
        if (match(tt)) return true;

        if (peek().has_chars())
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            peek().type);

        return false;
    }

    [[nodiscard]] constexpr auto consume_impl(TokenType        tt,
                                              fmt::string_view fmt,
                                              fmt::format_args args) -> bool {
        if (match(tt)) return true;

        if (peek().has_chars())
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            peek().type);

        er.vreport_note(span(), fmt, args);

        return false;
    }

    [[nodiscard]] constexpr auto consume_id_non_kw() -> bool {
        return !is_kw_and_report(span()) && consume(TokenType::Id);
    }

    [[nodiscard]] constexpr auto is_kw_and_report(Span const& s) -> bool {
        if (is_kw(s)) {
            er.report_error(span(), "can not use keyword '{}' as identifier",
                            span().str(source));
            return true;
        }

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
        } while (peek().is_comment() || peek().is_err());
    }
};

auto parse_into_ast(std::span<Token const> tokens, ast::Ast& ast,
                    LocalErrorReporter const& er, ParseOptions const& opt)
    -> ast::NodeFile* {
    auto p = Parser{tokens, er, opt, ast};
    return p.parse_source_file();
}

}  // namespace yal
