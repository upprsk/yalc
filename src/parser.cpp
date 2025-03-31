#include "parser.hpp"

#include <string_view>
#include <utility>
#include <vector>

#include "ast-node-id.hpp"
#include "error_reporter.hpp"
#include "file-store.hpp"
#include "libassert/assert.hpp"
#include "tokenizer.hpp"

namespace yal {

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
    auto parse_top_decl_attr() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }

    auto parse_func_decl(std::span<ast::NodeId const> decorators)
        -> ast::NodeId {
        auto start = loc();
        try(consume("func"));

        auto name = parse_func_id();

        std::vector<ast::NodeId> gargs;
        if (check(TokenType::Lbracket)) gargs = parse_func_gargs();

        auto args = parse_func_args();

        auto ret = ast::NodeId::invalid();
        if (!check_oneof(TokenType::Lbrace, TokenType::Semi))
            ret = parse_func_ret_pack();

        auto body = ast::NodeId::invalid();
        if (check(TokenType::Lbrace)) body = parse_block();

        return ast.new_func_decl(start.extend(span()), decorators, name, gargs,
                                 args, ret, body);
    }

    auto parse_top_var_decl(std::span<ast::NodeId const> decorators)
        -> ast::NodeId {
        PANIC("NOT IMPLEMENTED");
    }

    auto parse_top_def_decl(std::span<ast::NodeId const> decorators)
        -> ast::NodeId {
        PANIC("NOT IMPLEMENTED");
    }

    // ------------------------------------------------------------------------

    auto parse_func_id() -> ast::NodeId {
        auto start = loc();

        std::vector<ast::NodeId> names;
        do {
            auto name_span = span();
            try(consume(TokenType::Id));

            names.push_back(ast.new_identifier(name_span.str(source)));
        } while (match(TokenType::Comma));

        return ast.new_func_id(start.extend(prev_span()), names);
    }

    auto parse_func_gargs() -> std::vector<ast::NodeId> {
        PANIC("NOT IMPLEMENTED");
    }

    auto parse_func_args() -> std::vector<ast::NodeId> {
        std::vector<ast::NodeId> args;
        if (!consume(TokenType::Lparen)) return args;

        while (!check(TokenType::Rparen)) {
            args.push_back(parse_func_arg());

            if (check(TokenType::Rparen)) break;
            if (!consume(TokenType::Comma)) break;
        }

        if (check(TokenType::DotDotDot))
            PANIC("c varargs have not been implemented");

        if (!consume(TokenType::Rparen)) return args;
        return args;
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

    // ========================================================================

    auto parse_expr() -> ast::NodeId { return parse_prec_expr(PREC_NONE); }

    // ------------------------------------------------------------------------

    auto parse_prec_expr(int precedence) -> ast::NodeId {
        auto left = parse_prefix_expr();
        while (get_precedence(peek().type) > precedence)
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
                if (check("struct")) return parse_struct();
                return parse_id(t);

            case TokenType::Int:
            case TokenType::Hex:
            case TokenType::Float: return parse_number();

            case TokenType::DotLbrace: return parse_lit();
            case TokenType::Lbracket: return parse_arr();
            case TokenType::Str: return parse_str();
            case TokenType::Char: return parse_char();

            case TokenType::Ampersand:
            case TokenType::Bang:
            case TokenType::Tilde:
            case TokenType::Minus:
            case TokenType::Plus:
            case TokenType::Star:
            case TokenType::Question: return parse_unary();

            default: break;
        }

        er->report_error(t.span, "expected expression, found {}", t.type);
        advance();

        return ast.new_err(loc());
    }

    auto parse_infix_expr(ast::NodeId left) -> ast::NodeId {
        PANIC("NOT IMPLEMENTED", left);
    }

    // ------------------------------------------------------------------------

    auto parse_id(Token t) -> ast::NodeId {
        return ast.new_id(to_loc(t.span), t.span.str(source));
    }

    auto parse_number() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_lit() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_arr() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_str() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_char() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_struct() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }
    auto parse_unary() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }

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

    static constexpr auto get_precedence(std::string_view kw) -> int {
        if (kw == "as") return PREC_CAST;
        if (kw == "or" || kw == "and") return PREC_LOGIC;

        return PREC_NONE;
    }

    static constexpr auto get_precedence(TokenType tt) -> int {
        switch (tt) {
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

            default: return PREC_NONE;
        }
    }

    // ========================================================================

    auto parse_stmt() -> ast::NodeId { PANIC("NOT IMPLEMENTED"); }

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
