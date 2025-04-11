#include "parser.hpp"

#include <algorithm>
#include <charconv>
#include <cstdint>
#include <expected>
#include <string_view>
#include <utility>
#include <vector>

#include "ast-node.hpp"
#include "ast.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"
#include "nlohmann/json.hpp"
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

auto escape_string(ErrorReporterForFile& er, Span span, std::string_view s)
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
                case 't': result.push_back('\t'); break;
                case 'n': result.push_back('\n'); break;
                case 'v': result.push_back('\v'); break;
                case 'f': result.push_back('\f'); break;
                case 'r': result.push_back('\r'); break;

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

#define try(...) \
    if (!(__VA_ARGS__)) return ast->new_err(prev_loc())

struct Parser {
    [[nodiscard]] constexpr auto peek() const -> Token {
        return tokens[current];
    }

    [[nodiscard]] constexpr auto peek_prev() const -> Token {
        // in case we are at the first token, return EOF. This is safe because
        // we will always have at least one token in `tokens`, the EOF token.
        if (current == 0) return tokens[tokens.size() - 1];
        return tokens[current - 1];
    }

    [[nodiscard]] constexpr auto peek_and_advance() -> Token {
        auto t = peek();
        advance();
        return t;
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

        return tokens[current + 1].type == tt;
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

        er->report_error(span(), "expected {}, but got {}", tt, peek().type);
        advance();

        return false;
    }

    [[nodiscard]] constexpr auto consume(std::string_view tt) -> bool {
        if (match(tt)) return true;

        er->report_error(span(), "expected '{}', but got '{}'", tt,
                         span().str(source));
        advance();

        return false;
    }

    [[nodiscard]] constexpr auto consume_impl(TokenType        tt,
                                              fmt::string_view fmt,
                                              fmt::format_args args) -> bool {
        if (match(tt)) return true;

        er->report_error(span(), "expected {}, but got {}", tt, peek().type);
        er->vreport_note(span(), fmt, args);
        advance();

        return false;
    }

    [[nodiscard]] constexpr auto span() const -> Span { return peek().span; }
    [[nodiscard]] constexpr auto prev_span() const -> Span {
        return peek_prev().span;
    }

    [[nodiscard]] constexpr auto to_loc(Span s) const -> Location {
        return {.fileid = fileid, .span = s};
    }

    [[nodiscard]] constexpr auto loc() const -> Location {
        return {.fileid = fileid, .span = peek().span};
    }

    [[nodiscard]] constexpr auto prev_loc() const -> Location {
        return {.fileid = fileid, .span = peek_prev().span};
    }

    [[nodiscard]] constexpr auto is_at_end() const -> bool {
        return peek().is_eof();
    }

    constexpr void advance() {
        do {
            if (!is_at_end()) current++;
        } while (peek().is_comment());
    }

    constexpr void skip_start_comments() {
        while (peek().is_comment()) current++;
    }

    // ------------------------------------------------------------------------

    auto parse_file() -> ast::Node* {
        skip_start_comments();

        auto m = parse_module_decl();

        std::vector<ast::Node*> children;
        while (!is_at_end()) {
            children.push_back(parse_top_level_decl());
        }

        (void)consume(TokenType::Eof);

        return ast->new_source_file(m->get_loc().extend(prev_span()), m,
                                    children);
    }

    auto parse_module_decl() -> ast::Node* {
        auto start = loc();
        try(consume("module"));

        auto name = peek();
        try(consume(TokenType::Id));
        try(consume(TokenType::Semi));

        return ast->new_module_decl(start.extend(prev_span()),
                                    name.span.str(source));
    }

    auto parse_decorators() -> ast::Node* {
        auto dec_start = loc();

        std::vector<ast::Node*> decorators;
        while (check(TokenType::Decorator)) {
            decorators.push_back(parse_top_decl_attr());
        }

        return ast->new_decorators(dec_start.extend(prev_span()), decorators);
    }

    auto parse_top_level_decl() -> ast::Node* {
        if (check("import")) return parse_import();
        if (check("part")) return parse_part();

        // all other kinds of top-level may contain a top_decl_attr, se we parse
        // it here
        auto decorators = parse_decorators();

        if (check("func")) return parse_func_decl(decorators);
        if (check("var")) return parse_top_var_decl(decorators);
        if (check("def")) return parse_top_def_decl(decorators);

        er->report_error(span(),
                         "expected one of 'import', 'part', 'func', 'var' or "
                         "'def', found '{}'",
                         span().str(source));
        advance();
        return ast->new_err(prev_loc());
    }

    auto parse_import() -> ast::Node* {
        auto start = loc();
        try(consume("import"));

        auto path = span();
        try(consume(TokenType::Str));
        try(consume(TokenType::Semi));

        auto s = path.str(source);
        s = s.substr(1, s.size() - 2);
        auto result = escape_string(*er, path, s);
        return ast->new_import_stmt(start.extend(prev_span()), result);
    }

    auto parse_part() -> ast::Node* { PANIC("NOT IMPLEMENTED"); }

    auto parse_top_decl_attr() -> ast::Node* {
        auto start = loc();
        try(consume(TokenType::Decorator));

        std::vector<ast::Node*> items;
        if (match(TokenType::Lparen)) {
            while (!check(TokenType::Rparen)) {
                auto s = span();
                if (match(TokenType::Id)) {
                    if (match(TokenType::Equal)) {
                        // value and key
                        auto expr = parse_expr();
                        items.push_back(ast->new_decorator_param(
                            to_loc(s.extend(prev_span())), s.str(source),
                            expr));
                    } else {
                        // just key
                        items.push_back(ast->new_id(to_loc(s), s.str(source)));
                    }
                } else {
                    // just value, no key
                    auto expr = parse_expr();
                    items.push_back(expr);
                }

                if (check(TokenType::Rparen)) break;
                if (!consume(TokenType::Comma)) break;
            }

            try(consume(TokenType::Rparen));
        }

        return ast->new_decorator(start.extend(prev_span()),
                                  start.span.str(source).substr(1), items);
    }

    auto parse_func_decl(ast::Node* decorators) -> ast::Node* {
        auto start = loc();
        try(consume("func"));

        auto name = parse_func_id_pack();

        ast::Node* gargs = nullptr;
        if (check(TokenType::Lbracket))
            gargs = parse_func_gargs();
        else
            gargs = ast->new_func_params(prev_loc(), {});

        auto [args, is_c_varargs] = parse_func_args();

        ast::Node* ret = nullptr;
        if (!check_oneof(TokenType::Lbrace, TokenType::Semi))
            ret = parse_func_ret_pack();

        ast::Node* body = nullptr;
        if (check(TokenType::Lbrace))
            body = parse_block();
        else
            try(consume(TokenType::Semi));

        return ast->new_func_decl(start.extend(prev_span()), decorators, name,
                                  gargs, args, ret, body, is_c_varargs);
    }

    auto parse_top_var_decl(ast::Node* decorators) -> ast::Node* {
        auto start = loc();
        auto inner = parse_var_decl();

        return ast->new_top_var_decl(start.extend(prev_span()), decorators,
                                     inner);
    }

    auto parse_top_def_decl(ast::Node* decorators) -> ast::Node* {
        auto start = loc();
        auto inner = parse_def_decl();

        return ast->new_top_def_decl(start.extend(prev_span()), decorators,
                                     inner);
    }

    // ------------------------------------------------------------------------

    auto parse_id_pack() -> ast::Node* {
        auto start = loc();

        std::vector<ast::Node*> names;
        do {
            auto name_span = span();
            try(consume(TokenType::Id));

            names.push_back(
                ast->new_id(to_loc(name_span), name_span.str(source)));
        } while (match(TokenType::Comma));

        return ast->new_id_pack(start.extend(prev_span()), names);
    }

    auto parse_func_id_pack() -> ast::Node* {
        auto start = loc();

        std::vector<ast::Node*> names;
        do {
            auto name_span = span();
            try(consume(TokenType::Id));

            names.push_back(
                ast->new_id(to_loc(name_span), name_span.str(source)));
        } while (match(TokenType::Dot));

        return ast->new_id_pack(start.extend(prev_span()), names);
    }

    auto parse_func_gargs() -> ast::Node* { PANIC("NOT IMPLEMENTED"); }

    auto parse_func_args() -> std::pair<ast::Node*, bool> {
        auto start = loc();

        std::vector<ast::Node*> args;
        if (!consume(TokenType::Lparen))
            return std::make_pair(ast->new_func_params(prev_loc(), args),
                                  false);

        while (!check(TokenType::Rparen)) {
            if (check(TokenType::DotDotDot)) break;

            args.push_back(parse_func_arg());

            if (check(TokenType::Rparen)) break;
            if (!consume(TokenType::Comma)) break;
        }

        auto is_c_varargs = false;
        if (match(TokenType::DotDotDot)) {
            is_c_varargs = true;

            // we allow a trailing comma, but don't really care for it as it has
            // no semantic meaning.
            (void)match(TokenType::Comma);
        }

        if (!consume(TokenType::Rparen))
            return std::make_pair(
                ast->new_func_params(start.extend(prev_span()), args),
                is_c_varargs);
        return std::make_pair(
            ast->new_func_params(start.extend(prev_span()), args),
            is_c_varargs);
    }

    auto parse_func_arg() -> ast::Node* {
        auto start = loc();
        auto name = span();
        try(consume(TokenType::Id));

        auto end = prev_span();

        ast::Node* ty = nullptr;
        if (match(TokenType::Colon)) {
            ty = parse_expr();
            end = ty->get_loc().span;
        }

        return ast->new_func_param(start.extend(end), name.str(source), ty);
    }

    auto parse_func_ret_pack() -> ast::Node* {
        auto start = loc();

        if (match(TokenType::Lparen)) {
            std::vector<ast::Node*> ret;

            do {
                // named
                if (check(TokenType::Id) && check_next(TokenType::Colon)) {
                    auto name = span();
                    advance();

                    // skip the :
                    advance();

                    auto type = parse_expr();
                    ret.push_back(
                        ast->new_named_ret(to_loc(name.extend(prev_span())),
                                           name.str(source), type));
                }

                // not named
                else {
                    ret.push_back(parse_expr());
                }

                if (check(TokenType::Rparen)) break;
            } while (match(TokenType::Comma));

            try(consume(TokenType::Rparen));
            return ast->new_func_ret_pack(start.extend(prev_span()), ret);
        }

        auto expr = parse_expr();
        return ast->new_func_ret_pack(start.extend(prev_span()),
                                      std::array{expr});
    }

    // ========================================================================

    auto parse_block() -> ast::Node* {
        auto start = loc();
        try(consume(TokenType::Lbrace));

        std::vector<ast::Node*> children;
        while (!is_at_end() && !check(TokenType::Rbrace)) {
            children.push_back(parse_stmt());
        }

        try(consume(TokenType::Rbrace));

        return ast->new_block(start.extend(prev_span()), children);
    }

    auto parse_var_decl() -> ast::Node* {
        auto start = loc();
        try(consume("var"));

        auto       names = parse_id_pack();
        ast::Node* tys = nullptr;
        if (match(TokenType::Colon)) {
            tys = parse_expr_pack();
        }

        ast::Node* inits = nullptr;
        if (match(TokenType::Equal)) {
            inits = parse_expr_pack();
        }

        try(consume(TokenType::Semi));

        return ast->new_var_decl(start.extend(prev_span()), names, tys, inits);
    }

    auto parse_def_decl() -> ast::Node* {
        auto start = loc();
        try(consume("def"));

        auto       names = parse_id_pack();
        ast::Node* tys = nullptr;
        if (match(TokenType::Colon)) {
            tys = parse_expr_pack();
        }

        ast::Node* inits = nullptr;
        if (match(TokenType::Equal)) {
            inits = parse_expr_pack();
        } else {
            er->report_error(
                span(), "constants must have an initializer, but none found");
            inits = ast->new_err(loc());
        }

        try(consume(TokenType::Semi));

        return ast->new_def_decl(start.extend(prev_span()), names, tys, inits);
    }

    // ========================================================================

    auto parse_expr() -> ast::Node* { return parse_prec_expr(PREC_NONE); }

    auto parse_expr_pack() -> ast::Node* {
        auto start = loc();

        std::vector<ast::Node*> children;

        do {
            children.push_back(parse_expr());
        } while (match(TokenType::Comma));

        return ast->new_expr_pack(start.extend(prev_span()), children);
    }

    // ------------------------------------------------------------------------

    auto parse_prec_expr(int precedence) -> ast::Node* {
        auto left = parse_prefix_expr();
        while (get_precedence(peek()) > precedence)
            left = parse_infix_expr(left);

        return left;
    }

    auto parse_prefix_expr() -> ast::Node* {
        auto t = peek_and_advance();
        switch (t.type) {
            case TokenType::Lparen: {
                auto expr = parse_expr();
                (void)consume(TokenType::Rparen);
                return expr;
            } break;

            case TokenType::Id:
                if (t.span.str(source) == "struct") return parse_struct();
                return parse_id(t);

            case TokenType::Dot: return parse_kw_lit();

            case TokenType::Int:
            case TokenType::Hex:
            case TokenType::Float: return parse_number(t);

            case TokenType::DotLbrace: return parse_lit();
            case TokenType::Lbracket: return parse_arr();
            case TokenType::Str: return parse_str(t);
            case TokenType::Char: return parse_char();

            case TokenType::Ampersand:
            case TokenType::Bang:
            case TokenType::Tilde:
            case TokenType::Minus:
            case TokenType::Plus:
            case TokenType::Star:
            case TokenType::Question: return parse_unary(t);

            default: break;
        }

        er->report_error(t.span, "expected expression, found {}", t.type);
        advance();

        return ast->new_err(loc());
    }

    auto parse_infix_expr(ast::Node* left) -> ast::Node* {
        auto t = peek_and_advance();
        if (t.type == TokenType::Lparen) return parse_call(left);
        if (t.type == TokenType::Dot) return parse_field(left);

        auto kind = ast::NodeKind::Err;
        auto right = parse_prec_expr(get_precedence(t));

        switch (t.type) {
            case TokenType::Plus: kind = ast::NodeKind::Add; break;
            case TokenType::Minus: kind = ast::NodeKind::Sub; break;
            case TokenType::Star: kind = ast::NodeKind::Mul; break;
            case TokenType::Slash: kind = ast::NodeKind::Div; break;
            case TokenType::Percent: kind = ast::NodeKind::Mod; break;
            case TokenType::LessLess: kind = ast::NodeKind::LeftShift; break;
            case TokenType::GreaterGreater:
                kind = ast::NodeKind::RightShift;
                break;
            case TokenType::EqualEqual: kind = ast::NodeKind::Equal; break;
            case TokenType::BangEqual: kind = ast::NodeKind::NotEqual; break;
            case TokenType::Less: kind = ast::NodeKind::Less; break;
            case TokenType::LessEqual: kind = ast::NodeKind::LessEqual; break;
            case TokenType::Greater: kind = ast::NodeKind::Greater; break;
            case TokenType::GreaterEqual:
                kind = ast::NodeKind::GreaterEqual;
                break;
            case TokenType::Ampersand: kind = ast::NodeKind::Band; break;
            case TokenType::Pipe: kind = ast::NodeKind::Bor; break;
            case TokenType::Carrot: kind = ast::NodeKind::Bxor; break;

            case TokenType::Id:
                if (t.span.str(source) == "and")
                    kind = ast::NodeKind::Land;
                else if (t.span.str(source) == "or")
                    kind = ast::NodeKind::Lor;
                else if (t.span.str(source) == "as")
                    kind = ast::NodeKind::Cast;
                else
                    UNREACHABLE("invalid id in `parse_infix_expression`", t,
                                t.span.str(source));
                break;

            default: UNREACHABLE("invalid token in infix expr", t);
        }

        return ast->new_binary_expr(left->get_loc().extend(right->get_loc()),
                                    kind, left, right);
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto parse_id(Token t) const -> ast::Node* {
        return ast->new_id(to_loc(t.span), t.span.str(source));
    }

    [[nodiscard]] auto parse_number(Token t) const -> ast::Node* {
        switch (t.type) {
            case TokenType::Int: {
                // TODO: do not use replace and a dynamic string here
                auto s = std::string{t.span.str(source)};
                s.erase(begin(std::ranges::remove(s, '_')), s.end());

                uint64_t v;
                auto [ptr, ec] =
                    std::from_chars(s.data(), s.data() + s.size(), v);
                if (ec != std::errc{} || ptr != s.data() + s.size()) {
                    er->report_bug(t.span,
                                   "invalid integer found in parser: '{}'", s);
                    return ast->new_err(to_loc(t.span));
                }

                return ast->new_int(to_loc(t.span), v);
            } break;

            case TokenType::Hex: {
                // TODO: do not use replace and a dynamic string here
                auto s = std::string{t.span.str(source).substr(2)};
                s.erase(begin(std::ranges::remove(s, '_')), s.end());

                uint64_t v;
                auto [ptr, ec] =
                    std::from_chars(s.data(), s.data() + s.size(), v, 16);
                if (ec != std::errc{} || ptr != s.data() + s.size()) {
                    er->report_bug(t.span,
                                   "invalid integer found in parser: '{}'", s);
                    return ast->new_err(to_loc(t.span));
                }

                return ast->new_int(to_loc(t.span), v);
            } break;

            case TokenType::Float: {
                // TODO: do not use replace and a dynamic string here
                auto s = std::string{t.span.str(source)};
                s.erase(begin(std::ranges::remove(s, '_')), s.end());

                char* end = nullptr;
                auto  v = strtod(s.c_str(), &end);
                if (end != s.data() + s.size()) {
                    er->report_bug(t.span,
                                   "invalid f64 (double) found in parser: '{}'",
                                   s);
                    return ast->new_err(to_loc(t.span));
                }

                return ast->new_double(to_loc(t.span), v);
            } break;

            default: UNREACHABLE("unexpected token in `parse_number`", t.type);
        }
    }

    auto parse_lit() -> ast::Node* {
        auto start = prev_loc();

        std::vector<ast::Node*> items;
        while (!check(TokenType::Rbrace)) {
            auto item = parse_lit_item();
            items.push_back(item);

            if (check(TokenType::Rbrace)) break;
            if (!consume(TokenType::Comma)) break;
        }

        try(consume(TokenType::Rbrace));
        return ast->new_lit(start.extend(prev_span()), items);
    }

    auto parse_lit_item() -> ast::Node* {
        auto start = loc();

        // key-value pair
        if (match(TokenType::Dot)) {
            auto kw = parse_kw_lit_raw_str();
            if (match(TokenType::Equal)) {
                auto v = parse_expr();
                return ast->new_lit_param(start.extend(prev_span()), kw, v);
            }

            return ast->new_kw_lit(start.extend(prev_span()), kw);
        }

        return parse_expr();
    }

    auto parse_arr() -> ast::Node* {
        auto start = prev_loc();

        if (match(TokenType::Star)) {
            // multi pointer
            try(consume(TokenType::Rbracket));

            auto is_const = false;
            if (match("const")) is_const = true;

            auto inner = parse_prec_expr(PREC_UNARY);
            return ast->new_mptr(start.extend(prev_span()), is_const, inner);
        }

        if (match(TokenType::Rbracket)) {
            // slice
            auto is_const = false;
            if (match("const")) is_const = true;

            auto inner = parse_prec_expr(PREC_UNARY);
            return ast->new_slice(start.extend(prev_span()), is_const, inner);
        }

        // array type or literal

        // if we have inferred length, then this can't be a type.
        if (match("_")) {
            try(consume(TokenType::Rbracket));

            auto type = parse_prec_expr(PREC_UNARY);
            auto items = parse_array_items();
            return ast->new_array(start.extend(prev_span()), nullptr, type,
                                  items);
        }

        auto size = parse_expr();

        try(consume(TokenType::Rbracket));

        // if we have a const, this is an array type and we cant use this as a
        // literal
        if (match("const")) {
            auto type = parse_prec_expr(PREC_UNARY);
            return ast->new_array_type(start.extend(prev_span()), true, size,
                                       type);
        }

        auto type = parse_prec_expr(PREC_UNARY);

        // if we have a '{', then this is an array literal, otherwise it is a
        // type
        if (!check(TokenType::Lbrace)) {
            return ast->new_array_type(start.extend(prev_span()), false, size,
                                       type);
        }

        // got '{', then array literal
        auto items = parse_array_items();
        return ast->new_array(start.extend(prev_span()), size, type, items);
    }

    auto parse_array_items() -> std::vector<ast::Node*> {
        std::vector<ast::Node*> items;

        if (!consume(TokenType::Lbrace)) return items;

        while (!check(TokenType::Rbrace)) {
            items.push_back(parse_expr());

            if (check(TokenType::Rbrace)) break;
            if (!consume(TokenType::Comma)) break;
        }

        if (!consume(TokenType::Rbrace)) return items;

        return items;
    }

    [[nodiscard]] [[nodiscard]] auto parse_str(Token t) const -> ast::Node* {
        auto s = t.span.str(source);
        s = s.substr(1, s.size() - 2);

        auto result = escape_string(*er, t.span, s);
        return ast->new_str(to_loc(t.span), result);
    }

    auto parse_char() -> ast::Node* { PANIC("NOT IMPLEMENTED"); }

    auto parse_struct() -> ast::Node* {
        auto start = prev_loc();
        try(consume(TokenType::Lbrace));

        auto fields = parse_struct_fields();

        try(consume(TokenType::Rbrace));

        return ast->new_struct_type(start.extend(prev_span()), fields);
    }

    auto parse_kw_lit_raw_str() -> std::string_view {
        auto name = span();
        if (!consume(TokenType::Id)) return "";

        return name.str(source);
    }

    auto parse_kw_lit() -> ast::Node* {
        auto start = prev_loc();
        auto name = span();
        try(consume(TokenType::Id));

        return ast->new_kw_lit(start.extend(prev_span()), name.str(source));
    }

    auto parse_struct_fields() -> std::vector<ast::Node*> {
        std::vector<ast::Node*> fields;
        while (!check(TokenType::Rbrace)) {
            fields.push_back(parse_struct_field());

            if (check(TokenType::Rbrace)) break;
            if (!consume(TokenType::Comma)) break;
        }

        return fields;
    }

    auto parse_struct_field() -> ast::Node* {
        auto name = span();
        try(consume(TokenType::Id));
        try(consume(TokenType::Colon));

        auto       type = parse_expr();
        ast::Node* init = nullptr;
        if (match(TokenType::Equal)) {
            init = parse_expr();
        }

        return ast->new_struct_field(to_loc(name.extend(prev_span())),
                                     name.str(source), type, init);
    }

    auto parse_unary(Token t) -> ast::Node* {
        if (t.type == TokenType::Star) {
            auto is_const = false;
            if (match("const")) is_const = true;

            auto inner = parse_prec_expr(PREC_UNARY);
            return ast->new_ptr(to_loc(t.span.extend(prev_span())), is_const,
                                inner);
        }

        auto kind = ast::NodeKind::Err;
        auto child = parse_prec_expr(PREC_UNARY);

        switch (t.type) {
            case TokenType::Ampersand: kind = ast::NodeKind::AddrOf; break;
            case TokenType::Bang: kind = ast::NodeKind::Lnot; break;
            case TokenType::Tilde: kind = ast::NodeKind::Bnot; break;
            case TokenType::Minus: kind = ast::NodeKind::Neg; break;
            case TokenType::Plus:
                // this is literally a nop, just return the child
                return child;
            default: UNREACHABLE("invalid token in infix expr", t);
        }

        return ast->new_unary_expr(to_loc(t.span.extend(prev_span())), kind,
                                   child);
    }

    auto parse_call(ast::Node* left) -> ast::Node* {
        std::vector<ast::Node*> args;
        while (!check(TokenType::Rparen)) {
            args.push_back(parse_expr());

            if (check(TokenType::Rparen)) break;
            if (!consume(TokenType::Comma)) break;
        }

        try(consume(TokenType::Rparen));
        return ast->new_call(left->get_loc().extend(prev_span()), left, args);
    }

    auto parse_field(ast::Node* left) -> ast::Node* {
        auto name = span();
        try(consume(TokenType::Id));

        return ast->new_field(left->get_loc().extend(prev_span()), left,
                              name.str(source));
    }

    // ------------------------------------------------------------------------

    constexpr static auto const PREC_CALL = 10;
    constexpr static auto const PREC_UNARY = 9;
    constexpr static auto const PREC_CAST = 8;
    constexpr static auto const PREC_MUL = 7;
    constexpr static auto const PREC_ADD = 6;
    constexpr static auto const PREC_SHIFT = 5;
    constexpr static auto const PREC_COMP = 4;
    constexpr static auto const PREC_BIT = 3;
    constexpr static auto const PREC_LOGIC = 2;
    constexpr static auto const PREC_ASSIGN = 1;
    constexpr static auto const PREC_NONE = 0;

    [[nodiscard]] constexpr auto get_precedence(Token t) const -> int {
        switch (t.type) {
            case TokenType::Lparen:
            case TokenType::Dot: return PREC_CALL;
            case TokenType::Question:
            case TokenType::Lbracket: return PREC_UNARY;

            case TokenType::Star:
            case TokenType::Slash:
            case TokenType::Percent: return PREC_MUL;

            case TokenType::Plus:
            case TokenType::Minus: return PREC_ADD;

            case TokenType::LessLess:
            case TokenType::GreaterGreater: return PREC_SHIFT;

            case TokenType::EqualEqual:
            case TokenType::BangEqual:
            case TokenType::Less:
            case TokenType::LessEqual:
            case TokenType::Greater:
            case TokenType::GreaterEqual: return PREC_COMP;

            case TokenType::Ampersand:
            case TokenType::Carrot:
            case TokenType::Pipe: return PREC_BIT;

            case TokenType::Id: {
                auto kw = t.span.str(source);
                if (kw == "as") return PREC_CAST;
                if (kw == "or" || kw == "and") return PREC_LOGIC;

                return PREC_NONE;
            }

#if NOT_REALLY_EXPRESSIONS
            case TokenType::Equal:
            case TokenType::PlusEqual:
            case TokenType::MinusEqual:
            case TokenType::StarEqual:
            case TokenType::SlashEqual:
            case TokenType::PercentEqual:
            case TokenType::LessLessEqual:
            case TokenType::GreaterGreaterEqual:
            case TokenType::AmpersandEqual:
            case TokenType::CarrotEqual:
            case TokenType::PipeEqual: return PREC_ASSIGN;
#endif

            default: return PREC_NONE;
        }
    }

    // ========================================================================

    auto parse_stmt() -> ast::Node* {
        if (check("return")) return parse_return_stmt();
        if (check("var")) return parse_var_decl();
        if (check("def")) return parse_def_decl();
        if (check("if")) return parse_if_stmt();
        if (check("while")) return parse_while_stmt();
        if (check("defer")) return parse_defer_stmt();

        if (check(TokenType::Lbrace)) return parse_block();

        if (match("break")) {
            auto start = prev_loc();
            try(consume(TokenType::Semi));

            return ast->new_break(start.extend(prev_span()));
        }

        if (match("continue")) {
            auto start = prev_loc();
            try(consume(TokenType::Semi));

            return ast->new_continue(start.extend(prev_span()));
        }

        // expression statement
        auto expr = parse_expr();

        // TODO: move assignment to its own function
        // TODO: handle assignment of multiple items (we would have a comma
        // here)

        // handle assigment
        if (match_oneof(
                TokenType::Equal, TokenType::PlusEqual, TokenType::MinusEqual,
                TokenType::StarEqual, TokenType::SlashEqual,
                TokenType::PercentEqual, TokenType::LessLessEqual,
                TokenType::GreaterGreaterEqual, TokenType::AmpersandEqual,
                TokenType::CarrotEqual, TokenType::PipeEqual)) {
            auto kind = ast::NodeKind::Err;
            switch (peek_prev().type) {
                case TokenType::Equal: kind = ast::NodeKind::Assign; break;
                case TokenType::PlusEqual:
                    kind = ast::NodeKind::AssignAdd;
                    break;
                case TokenType::MinusEqual:
                    kind = ast::NodeKind::AssignSub;
                    break;
                case TokenType::StarEqual:
                    kind = ast::NodeKind::AssignMul;
                    break;
                case TokenType::SlashEqual:
                    kind = ast::NodeKind::AssignDiv;
                    break;
                case TokenType::PercentEqual:
                    kind = ast::NodeKind::AssignMod;
                    break;
                case TokenType::LessLessEqual:
                    kind = ast::NodeKind::AssignShiftLeft;
                    break;
                case TokenType::GreaterGreaterEqual:
                    kind = ast::NodeKind::AssignShiftRight;
                    break;
                case TokenType::AmpersandEqual:
                    kind = ast::NodeKind::AssignBand;
                    break;
                case TokenType::CarrotEqual:
                    kind = ast::NodeKind::AssignBxor;
                    break;
                case TokenType::PipeEqual:
                    kind = ast::NodeKind::AssignBor;
                    break;
                default:
                    UNREACHABLE("invalid token type when parsing assign",
                                peek_prev());
            }

            auto rhs = parse_expr();
            try(consume(TokenType::Semi));

            expr = ast->new_expr_pack(expr->get_loc(), std::array{expr});
            rhs = ast->new_expr_pack(rhs->get_loc(), std::array{rhs});

            return ast->new_assign_stmt(expr->get_loc().extend(prev_span()),
                                        kind, expr, rhs);
        }

        try(consume(TokenType::Semi));

        return ast->new_expr_stmt(expr->get_loc().extend(prev_span()), expr);
    }

    auto parse_return_stmt() -> ast::Node* {
        auto start = loc();
        try(consume("return"));

        ast::Node* expr = nullptr;
        if (!check(TokenType::Semi)) expr = parse_expr_pack();
        try(consume(TokenType::Semi));

        return ast->new_return_stmt(start.extend(prev_span()), expr);
    }

    auto parse_if_stmt() -> ast::Node* {
        auto start = loc();
        try(consume("if"));

        auto       cond = parse_expr();
        auto       wt = parse_block();
        ast::Node* wf = nullptr;

        if (match("else")) {
            if (check("if"))
                wf = parse_if_stmt();
            else
                wf = parse_block();
        }

        return ast->new_if_stmt(start.extend(prev_span()), cond, wt, wf);
    }

    auto parse_while_stmt() -> ast::Node* {
        auto start = loc();
        try(consume("while"));

        auto cond = parse_expr();
        auto body = parse_block();

        return ast->new_while_stmt(start.extend(prev_span()), cond, body);
    }

    auto parse_defer_stmt() -> ast::Node* {
        auto start = loc();
        try(consume("defer"));

        auto stmt = parse_stmt();
        return ast->new_defer_stmt(start.extend(prev_span()), stmt);
    }

    // ------------------------------------------------------------------------

    std::span<Token const> tokens;
    std::string_view       source;
    ErrorReporterForFile*  er;
    FileId                 fileid;

    ast::Ast* ast;
    size_t    current{};
};

auto parse_into(std::span<Token const> tokens, ast::Ast& ast,
                ErrorReporterForFile& er) -> ast::Node* {
    auto p = Parser{
        .tokens = tokens,
        .source = er.get_source(),
        .er = &er,
        .fileid = er.get_fileid(),
        .ast = &ast,
    };

    return p.parse_file();
}

auto parse(std::span<Token const> tokens, ErrorReporterForFile& er)
    -> std::pair<ast::Ast, ast::Node*> {
    ast::Ast ast;
    auto     root = parse_into(tokens, ast, er);
    return std::make_pair(std::move(ast), root);
}

}  // namespace yal
