#include "parser.hpp"

#include <algorithm>
#include <charconv>
#include <cstdint>
#include <expected>
#include <string_view>
#include <utility>
#include <vector>

#include "ast-node-id.hpp"
#include "ast-node.hpp"
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
    if (!(__VA_ARGS__)) return ast.new_err(prev_loc())

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

    auto parse_file() -> ast::NodeId {
        skip_start_comments();

        auto m = parse_module_decl();

        std::vector<ast::NodeId> children;
        while (!is_at_end()) {
            children.push_back(parse_top_level_decl());
        }

        (void)consume(TokenType::Eof);

        auto loc = ast.get_node_loc(m.as_ref()).extend(prev_span());
        return ast.new_source_file(loc, m, children);
    }

    auto parse_module_decl() -> ast::NodeId {
        auto start = loc();
        try(consume("module"));

        auto name = peek();
        try(consume(TokenType::Id));
        try(consume(TokenType::Semi));

        return ast.new_module_decl(start.extend(prev_span()),
                                   name.span.str(source));
    }

    auto parse_top_level_decl() -> ast::NodeId {
        if (check("import")) return parse_import();
        if (check("part")) return parse_part();

        // all other kinds of top-level may contain a top_decl_attr, se we parse
        // it here
        std::vector<ast::NodeId> decorators;
        while (check(TokenType::Decorator)) {
            decorators.push_back(parse_top_decl_attr());
        }

        if (check("func")) return parse_func_decl(decorators);
        if (check("var")) return parse_top_var_decl(decorators);
        if (check("def")) return parse_top_def_decl(decorators);

        er->report_error(span(),
                         "expected one of 'import', 'part', 'func', 'var' or "
                         "'def', found '{}'",
                         span().str(source));
        advance();
        return ast.new_err(prev_loc());
    }

    auto parse_import() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_part() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_top_decl_attr() -> ast::NodeId {
        auto start = loc();
        try(consume(TokenType::Decorator));

        std::vector<std::pair<std::string_view, ast::NodeId>> items;
        if (match(TokenType::Lparen)) {
            while (!check(TokenType::Rparen)) {
                auto s = span();
                if (match(TokenType::Id)) {
                    if (match(TokenType::Equal)) {
                        // value and key
                        auto expr = parse_expr();
                        items.emplace_back(s.str(source), expr);
                    } else {
                        // just key
                        items.emplace_back(s.str(source),
                                           ast::NodeId::invalid());
                    }
                } else {
                    // just value, no key
                    auto expr = parse_expr();
                    items.emplace_back("", expr);
                }

                if (check(TokenType::Rparen)) break;
                if (!consume(TokenType::Comma)) break;
            }

            try(consume(TokenType::Rparen));
        }

        return ast.new_decorator(start.extend(prev_span()),
                                 start.span.str(source).substr(1), items);
    }

    auto parse_func_decl(std::span<ast::NodeId const> decorators)
        -> ast::NodeId {
        auto start = loc();
        try(consume("func"));

        auto name = parse_func_id_pack();

        std::vector<ast::NodeId> gargs;
        if (check(TokenType::Lbracket)) gargs = parse_func_gargs();

        auto [args, is_c_varargs] = parse_func_args();

        auto ret = ast::NodeId::invalid();
        if (!check_oneof(TokenType::Lbrace, TokenType::Semi))
            ret = parse_func_ret_pack();

        auto body = ast::NodeId::invalid();
        if (check(TokenType::Lbrace))
            body = parse_block();
        else
            try(consume(TokenType::Semi));

        return ast.new_func_decl(start.extend(prev_span()), decorators, name,
                                 gargs, args, ret, body, is_c_varargs);
    }

    auto parse_top_var_decl(std::span<ast::NodeId const> decorators)
        -> ast::NodeId {
        auto start = loc();
        auto inner = parse_var_decl();

        return ast.new_top_var_decl(start.extend(prev_span()), decorators,
                                    inner);
    }

    auto parse_top_def_decl(std::span<ast::NodeId const> decorators)
        -> ast::NodeId {
        auto start = loc();
        auto inner = parse_def_decl();

        return ast.new_top_def_decl(start.extend(prev_span()), decorators,
                                    inner);
    }

    // ------------------------------------------------------------------------

    auto parse_id_pack() -> ast::NodeId {
        auto start = loc();

        std::vector<ast::NodeId> names;
        do {
            auto name_span = span();
            try(consume(TokenType::Id));

            names.push_back(ast.new_identifier(name_span.str(source)));
        } while (match(TokenType::Comma));

        return ast.new_id_pack(start.extend(prev_span()), names);
    }

    auto parse_func_id_pack() -> ast::NodeId {
        auto start = loc();

        std::vector<ast::NodeId> names;
        do {
            auto name_span = span();
            try(consume(TokenType::Id));

            names.push_back(ast.new_identifier(name_span.str(source)));
        } while (match(TokenType::Dot));

        return ast.new_id_pack(start.extend(prev_span()), names);
    }

    auto parse_func_gargs() -> std::vector<ast::NodeId> {
        PANIC("NOT IMPLEMENTED");
    }

    auto parse_func_args() -> std::pair<std::vector<ast::NodeId>, bool> {
        std::vector<ast::NodeId> args;
        if (!consume(TokenType::Lparen)) return std::make_pair(args, false);

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
            return std::make_pair(args, is_c_varargs);
        return std::make_pair(args, is_c_varargs);
    }

    auto parse_func_arg() -> ast::NodeId {
        auto start = loc();
        auto name = span();
        try(consume(TokenType::Id));

        auto end = prev_span();

        auto ty = ast::NodeId::invalid();
        if (match(TokenType::Colon)) {
            ty = parse_expr();
            end = ast.get_node_span(ty.as_ref());
        }

        return ast.new_func_param(start.extend(end), name.str(source), ty);
    }

    auto parse_func_ret_pack() -> ast::NodeId {
        auto start = loc();

        if (match(TokenType::Lparen)) {
            std::vector<std::pair<std::string_view, ast::NodeId>> ret;

            do {
                // named
                if (check(TokenType::Id) && check_next(TokenType::Colon)) {
                    auto name = span();
                    advance();

                    // skip the :
                    advance();

                    auto type = parse_expr();
                    ret.emplace_back(name.str(source), type);
                }

                // not named
                else {
                    auto type = parse_expr();
                    ret.emplace_back("", type);
                }

                if (check(TokenType::Rparen)) break;
            } while (match(TokenType::Comma));

            try(consume(TokenType::Rparen));

            // in case it is just a single return without a name, unwrap it
            if (ret.size() == 1 && ret[0].first.empty()) {
                return ret[0].second;
            }

            return ast.new_func_ret_pack(start.extend(prev_span()), ret);
        }

        return parse_expr();
    }

    // ========================================================================

    auto parse_block() -> ast::NodeId {
        auto start = loc();
        try(consume(TokenType::Lbrace));

        std::vector<ast::NodeId> children;
        while (!is_at_end() && !check(TokenType::Rbrace)) {
            children.push_back(parse_stmt());
        }

        try(consume(TokenType::Rbrace));

        return ast.new_block(start.extend(prev_span()), children);
    }

    auto parse_var_decl() -> ast::NodeId {
        auto start = loc();
        try(consume("var"));

        auto names = parse_id_pack();
        auto tys = ast::NodeId::invalid();
        if (match(TokenType::Colon)) {
            tys = parse_expr_pack();
        }

        auto inits = ast::NodeId::invalid();
        if (match(TokenType::Equal)) {
            inits = parse_expr_pack();
        }

        try(consume(TokenType::Semi));

        return ast.new_var_decl(start.extend(prev_span()), names, tys, inits);
    }

    auto parse_def_decl() -> ast::NodeId {
        auto start = loc();
        try(consume("def"));

        auto names = parse_id_pack();
        auto tys = ast::NodeId::invalid();
        if (match(TokenType::Colon)) {
            tys = parse_expr_pack();
        }

        auto inits = ast::NodeId::invalid();
        if (match(TokenType::Equal)) {
            inits = parse_expr_pack();
        } else {
            er->report_error(
                span(), "constants must have an initializer, but none found");
            inits = ast.new_err(loc());
        }

        try(consume(TokenType::Semi));

        return ast.new_def_decl(start.extend(prev_span()), names, tys, inits);
    }

    // ========================================================================

    auto parse_expr() -> ast::NodeId { return parse_prec_expr(PREC_NONE); }

    auto parse_expr_pack() -> ast::NodeId {
        auto start = loc();

        std::vector<ast::NodeId> children;

        do {
            children.push_back(parse_expr());
        } while (match(TokenType::Comma));

        return ast.new_expr_pack(start.extend(prev_span()), children);
    }

    // ------------------------------------------------------------------------

    auto parse_prec_expr(int precedence) -> ast::NodeId {
        auto left = parse_prefix_expr();
        while (get_precedence(peek()) > precedence)
            left = parse_infix_expr(left);

        return left;
    }

    auto parse_prefix_expr() -> ast::NodeId {
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

        return ast.new_err(loc());
    }

    auto parse_infix_expr(ast::NodeId left) -> ast::NodeId {
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

        return ast.new_binary_expr(
            ast.get_node_loc(left.as_ref())
                .extend(ast.get_node_span(right.as_ref())),
            kind, left, right);
    }

    // ------------------------------------------------------------------------

    auto parse_id(Token t) -> ast::NodeId {
        return ast.new_id(to_loc(t.span), t.span.str(source));
    }

    auto parse_number(Token t) -> ast::NodeId {
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
                    return ast.new_err(to_loc(t.span));
                }

                return ast.new_int(to_loc(t.span), v);
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
                    return ast.new_err(to_loc(t.span));
                }

                return ast.new_int(to_loc(t.span), v);
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
                    return ast.new_err(to_loc(t.span));
                }

                return ast.new_double(to_loc(t.span), v);
            } break;

            default: UNREACHABLE("unexpected token in `parse_number`", t.type);
        }
    }

    auto parse_lit() -> ast::NodeId {
        auto start = prev_loc();

        std::vector<std::pair<ast::NodeId, ast::NodeId>> items;
        while (!check(TokenType::Rbrace)) {
            auto item = parse_lit_item();
            items.push_back(item);

            if (check(TokenType::Rbrace)) break;
            if (!consume(TokenType::Comma)) break;
        }

        try(consume(TokenType::Rbrace));
        return ast.new_lit(start.extend(prev_span()), items);
    }

    auto parse_lit_item() -> std::pair<ast::NodeId, ast::NodeId> {
        // key-value pair
        if (match(TokenType::Dot)) {
            auto kw = parse_kw_lit();
            if (match(TokenType::Equal)) {
                auto v = parse_expr();
                return std::make_pair(kw, v);
            }

            return std::make_pair(ast::NodeId::invalid(), kw);
        }

        auto v = parse_expr();
        return std::make_pair(ast::NodeId::invalid(), v);
    }

    auto parse_arr() -> ast::NodeId {
        auto start = prev_loc();

        if (match(TokenType::Star)) {
            // multi pointer
            try(consume(TokenType::Rbracket));

            auto is_const = false;
            if (match("const")) is_const = true;

            auto inner = parse_prec_expr(PREC_UNARY);
            return ast.new_mptr(start.extend(prev_span()), is_const, inner);
        }

        if (match(TokenType::Rbracket)) {
            // slice
            auto is_const = false;
            if (match("const")) is_const = true;

            auto inner = parse_prec_expr(PREC_UNARY);
            return ast.new_slice(start.extend(prev_span()), is_const, inner);
        }

        // array type or literal

        // if we have inferred length, then this can't be a type.
        if (match("_")) {
            try(consume(TokenType::Rbracket));

            auto type = parse_prec_expr(PREC_UNARY);
            auto items = parse_array_items();
            return ast.new_array(start.extend(prev_span()),
                                 ast::NodeId::invalid(), type, items);
        }

        auto size = parse_expr();

        try(consume(TokenType::Rbracket));

        // if we have a const, this is an array type and we cant use this as a
        // literal
        if (match("const")) {
            auto type = parse_prec_expr(PREC_UNARY);
            return ast.new_array_type(start.extend(prev_span()), true, size,
                                      type);
        }

        auto type = parse_prec_expr(PREC_UNARY);

        // if we have a '{', then this is an array literal, otherwise it is a
        // type
        if (!check(TokenType::Lbrace)) {
            return ast.new_array_type(start.extend(prev_span()), false, size,
                                      type);
        }

        // got '{', then array literal
        auto items = parse_array_items();
        return ast.new_array(start.extend(prev_span()), size, type, items);
    }

    auto parse_array_items() -> std::vector<ast::NodeId> {
        std::vector<ast::NodeId> items;

        if (!consume(TokenType::Lbrace)) return items;

        while (!check(TokenType::Rbrace)) {
            items.push_back(parse_expr());

            if (check(TokenType::Rbrace)) break;
            if (!consume(TokenType::Comma)) break;
        }

        if (!consume(TokenType::Rbrace)) return items;

        return items;
    }

    auto parse_str(Token t) -> ast::NodeId {
        auto s = t.span.str(source);
        s = s.substr(1, s.size() - 2);

        auto result = escape_string(*er, t.span, s);
        return ast.new_str(to_loc(t.span), result);
    }

    auto parse_char() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }

    auto parse_struct() -> ast::NodeId {
        auto start = prev_loc();
        try(consume(TokenType::Lbrace));

        auto fields = parse_struct_fields();

        try(consume(TokenType::Rbrace));

        return ast.new_struct_type(start.extend(prev_span()), fields);
    }

    auto parse_kw_lit() -> ast::NodeId {
        auto start = prev_loc();
        auto name = span();
        try(consume(TokenType::Id));

        return ast.new_kw_lit(start.extend(prev_span()), name.str(source));
    }

    auto parse_struct_fields() -> std::vector<ast::NodeId> {
        std::vector<ast::NodeId> fields;
        while (!check(TokenType::Rbrace)) {
            fields.push_back(parse_struct_field());

            if (check(TokenType::Rbrace)) break;
            if (!consume(TokenType::Comma)) break;
        }

        return fields;
    }

    auto parse_struct_field() -> ast::NodeId {
        auto name = span();
        try(consume(TokenType::Id));
        try(consume(TokenType::Colon));

        auto type = parse_expr();
        auto init = ast::NodeId::invalid();

        if (match(TokenType::Equal)) {
            init = parse_expr();
        }

        return ast.new_struct_field(to_loc(name.extend(prev_span())),
                                    name.str(source), type, init);
    }

    auto parse_unary(Token t) -> ast::NodeId {
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

        return ast.new_unary_expr(to_loc(t.span.extend(prev_span())), kind,
                                  child);
    }

    auto parse_call(ast::NodeId left) -> ast::NodeId {
        std::vector<ast::NodeId> args;
        while (!check(TokenType::Rparen)) {
            args.push_back(parse_expr());

            if (check(TokenType::Rparen)) break;
            if (!consume(TokenType::Comma)) break;
        }

        try(consume(TokenType::Rparen));
        return ast.new_call(ast.get_node_loc(left.as_ref()).extend(prev_span()),
                            left, args);
    }

    auto parse_field(ast::NodeId left) -> ast::NodeId {
        auto name = span();
        try(consume(TokenType::Id));

        return ast.new_field(
            ast.get_node_loc(left.as_ref()).extend(prev_span()), left,
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

    auto parse_stmt() -> ast::NodeId {
        if (check("return")) return parse_return_stmt();
        if (check("var")) return parse_var_decl();
        if (check("def")) return parse_def_decl();
        if (check("if")) return parse_if_stmt();
        if (check("while")) return parse_while_stmt();

        if (match("break")) {
            auto start = prev_loc();
            try(consume(TokenType::Semi));

            return ast.new_break(start.extend(prev_span()));
        }

        if (match("continue")) {
            auto start = prev_loc();
            try(consume(TokenType::Semi));

            return ast.new_continue(start.extend(prev_span()));
        }

        // expression statement
        auto expr = parse_expr();
        try(consume(TokenType::Semi));

        return ast.new_expr_stmt(
            ast.get_node_loc(expr.as_ref()).extend(prev_span()), expr);
    }

    auto parse_return_stmt() -> ast::NodeId {
        auto start = loc();
        try(consume("return"));

        auto expr = ast::NodeId::invalid();
        if (!check(TokenType::Semi)) expr = parse_expr_pack();
        try(consume(TokenType::Semi));

        return ast.new_return_stmt(start.extend(prev_span()), expr);
    }

    auto parse_if_stmt() -> ast::NodeId {
        auto start = loc();
        try(consume("if"));

        auto cond = parse_expr();
        auto wt = parse_block();
        auto wf = ast::NodeId::invalid();

        if (match("else")) {
            if (check("if"))
                wf = parse_if_stmt();
            else
                wf = parse_block();
        }

        return ast.new_if_stmt(start.extend(prev_span()), cond, wt, wf);
    }

    auto parse_while_stmt() -> ast::NodeId {
        auto start = loc();
        try(consume("while"));

        auto cond = parse_expr();
        auto body = parse_block();

        return ast.new_while_stmt(start.extend(prev_span()), cond, body);
    }

    // ------------------------------------------------------------------------

    std::span<Token const> tokens;
    std::string_view       source;
    ErrorReporterForFile*  er;
    FileId                 fileid;

    ast::Ast ast{};
    size_t   current{};
};

auto parse(std::span<Token const> tokens, ErrorReporterForFile& er)
    -> std::pair<ast::Ast, ast::NodeId> {
    auto p = Parser{
        .tokens = tokens,
        .source = er.get_source(),
        .er = &er,
        .fileid = er.get_fileid(),
    };

    auto a = p.parse_file();
    return {p.ast, a};
}

}  // namespace yal
