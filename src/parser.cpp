#include "parser.hpp"

#include <algorithm>
#include <charconv>
#include <span>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "tokenizer.hpp"

namespace yal {

#define try(...) \
    if (!(__VA_ARGS__)) return ast.new_node_err(prev_span())

struct Parser {
    [[nodiscard]] constexpr auto peek() const -> Token {
        return tokens[current];
    }

    [[nodiscard]] constexpr auto peek_prev() const -> Token {
        // FIXME: this is not good, there is no bounds check
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

    [[nodiscard]] constexpr auto check_kw(std::string_view kw) const -> bool {
        return peek().is_kw(source, kw);
    }

    [[nodiscard]] constexpr auto match(TokenType tt) -> bool {
        if (!check(tt)) return false;

        advance();
        return true;
    }

    [[nodiscard]] constexpr auto match_kw(std::string_view kw) -> bool {
        if (!check_kw(kw)) return false;

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

    auto parse_file() -> NodeHandle {
        skip_start_comments();

        std::vector<NodeHandle> children;

        Span s;

        while (!is_at_end()) {
            children.push_back(parse_top_stmt());
        }

        if (!peek().is_eof()) {
            er->report_error(span(), "expected EOF, got {}", peek().type);
        }

        if (!children.empty()) {
            auto start = ast.get(children.at(0))->span;
            auto end = ast.get(children.at(children.size() - 1))->span;
            s = start.extend(end);
        }

        return ast.new_node_file(s, children);
    }

    auto parse_top_stmt() -> NodeHandle {
        if (match_kw("func")) return parse_func();
        if (match_kw("def")) return parse_def_decl();
        if (match_kw("var")) return parse_var_decl();
        if (match_kw("import")) return parse_import();

        // FIXME: this is just for testing
        return parse_expr();

        auto t = peek_and_advance();
        er->report_error(t.span,
                         "expected one of 'func', 'def', 'var' or 'import' at "
                         "top-level, found: {}",
                         t.type);

        return ast.new_node_err(t.span);
    }

    // func ::= "func" { ID "." } ID [ "[" generic_args "]" ] "(" func_args ")"
    // [ expr ] block ;
    auto parse_func() -> NodeHandle {
        auto s = prev_span();

        try(consume(TokenType::Id));
        auto name = ast.new_node_id(s.extend(prev_span()),
                                    std::string{prev_span().str(source)});
        while (match(TokenType::Id)) {
            name = ast.new_node_field(ast.get(name)->span, name,
                                      std::string{prev_span().str(source)});
        }

        try(consume(TokenType::Lparen));

        // TODO: args
        std::vector<NodeHandle> args;

        try(consume(TokenType::Rparen));

        auto ret = ast.new_node_nil(prev_span());
        if (!check(TokenType::Lbrace)) {
            ret = parse_expr();
        }

        auto body = parse_block();

        return ast.new_node_func(s.extend(ast.get(body)->span), name, args, ret,
                                 body);
    }

    auto parse_def_decl() -> NodeHandle {
        er->report_bug(span(), "def_decl not implemented");
        return ast.new_node_err(span());
    }

    auto parse_var_decl() -> NodeHandle {
        er->report_bug(span(), "var_decl not implemented");
        return ast.new_node_err(span());
    }

    auto parse_import() -> NodeHandle {
        er->report_bug(span(), "import not implemented");
        return ast.new_node_err(span());
    }

    // ------------------------------------------------------------------------

    // stmt ::= expr ";"
    //        | return_stmt
    //        | if_stmt
    //        | while_stmt
    //        | for_stmt
    //        | switch_stmt
    //        | block
    //        | def_decl
    //        | var_decl
    //        | assign
    //        | break
    //        | defer
    //        ;
    auto parse_stmt() -> NodeHandle {
        // TODO: other cases

        auto expr = parse_expr();
        try(consume(TokenType::Semi));

        return ast.new_node_unary(
            NodeKind::ExprStmt, ast.get(expr)->span.extend(prev_span()), expr);
    }

    // block ::= "{" { stmt } "}" ;
    auto parse_block() -> NodeHandle {
        auto s = span();

        std::vector<NodeHandle> children;

        try(consume(TokenType::Lbrace));

        while (!is_at_end() && !check(TokenType::Rbrace)) {
            children.push_back(parse_stmt());
        }

        try(consume(TokenType::Rbrace));

        return ast.new_node_block(s.extend(prev_span()), children);
    }

    // ------------------------------------------------------------------------

    // expr ::= logic_or ;
    auto parse_expr() -> NodeHandle { return parse_logic_or(); }

    // logic_or  ::= logic_and { "or" logic_and } ;
    auto parse_logic_or() -> NodeHandle {
        auto lhs = parse_logic_and();

        while (match_kw("or")) {
            auto rhs = parse_logic_and();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(NodeKind::LogicOr, span, lhs, rhs);
        }

        return lhs;
    }

    // logic_and ::= bin_or { "and" bin_or } ;
    auto parse_logic_and() -> NodeHandle {
        auto lhs = parse_bin_or();

        while (match_kw("and")) {
            auto rhs = parse_bin_or();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(NodeKind::LogicAnd, span, lhs, rhs);
        }

        return lhs;
    }

    // bin_or  ::= bin_xor { "|" bin_xor } ;
    auto parse_bin_or() -> NodeHandle {
        auto lhs = parse_bin_xor();

        while (match_oneof(TokenType::Pipe)) {
            auto rhs = parse_bin_xor();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(NodeKind::BinOr, span, lhs, rhs);
        }

        return lhs;
    }

    // bin_xor ::= bin_and { "^" bin_and } ;
    auto parse_bin_xor() -> NodeHandle {
        auto lhs = parse_bin_and();

        while (match_oneof(TokenType::Carrot)) {
            auto rhs = parse_bin_and();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(NodeKind::BinXor, span, lhs, rhs);
        }

        return lhs;
    }

    // bin_and ::= equality { "&" equality } ;
    auto parse_bin_and() -> NodeHandle {
        auto lhs = parse_equality();

        while (match_oneof(TokenType::Ampersand)) {
            auto rhs = parse_equality();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(NodeKind::BinAnd, span, lhs, rhs);
        }

        return lhs;
    }

    // equality ::= comparison { ( "!=" | "==" ) comparison } ;
    auto parse_equality() -> NodeHandle {
        auto lhs = parse_comparison();

        while (match_oneof(TokenType::BangEqual, TokenType::EqualEqual)) {
            NodeKind k;
            auto     t = peek_prev();
            switch (t.type) {
                case TokenType::BangEqual: k = NodeKind::NotEqual; break;
                case TokenType::EqualEqual: k = NodeKind::Equal; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_comparison();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(k, span, lhs, rhs);
        }

        return lhs;
    }

    // comparison ::= shift { ( ">" | "=>" | "<=" | "<" ) shift } ;
    auto parse_comparison() -> NodeHandle {
        auto lhs = parse_shift();

        while (match_oneof(TokenType::Greater, TokenType::GreaterEqual,
                           TokenType::LessEqual, TokenType::Less)) {
            NodeKind k;
            auto     t = peek_prev();
            switch (t.type) {
                case TokenType::Greater: k = NodeKind::Greater; break;
                case TokenType::GreaterEqual: k = NodeKind::GreaterEqual; break;
                case TokenType::Less: k = NodeKind::Smaller; break;
                case TokenType::LessEqual: k = NodeKind::SmallerEqual; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_shift();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(k, span, lhs, rhs);
        }

        return lhs;
    }

    // shift ::= term { ( "<<" | ">>" ) term } ;
    auto parse_shift() -> NodeHandle {
        auto lhs = parse_term();

        while (match_oneof(TokenType::LessLess, TokenType::GreaterGreater)) {
            NodeKind k;
            auto     t = peek_prev();
            switch (t.type) {
                case TokenType::LessLess: k = NodeKind::ShftLeft; break;
                case TokenType::GreaterGreater: k = NodeKind::ShftRight; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_term();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(k, span, lhs, rhs);
        }

        return lhs;
    }

    // term ::= factor { ( "+" | "-" ) factor } ;
    auto parse_term() -> NodeHandle {
        auto lhs = parse_factor();

        while (match_oneof(TokenType::Plus, TokenType::Minus)) {
            NodeKind k;
            auto     t = peek_prev();
            switch (t.type) {
                case TokenType::Plus: k = NodeKind::Add; break;
                case TokenType::Minus: k = NodeKind::Sub; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_factor();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(k, span, lhs, rhs);
        }

        return lhs;
    }

    // factor ::= cast { ( "*" | "/" | "%" ) cast } ;
    auto parse_factor() -> NodeHandle {
        auto lhs = parse_cast();

        while (match_oneof(TokenType::Star, TokenType::Slash,
                           TokenType::Percent)) {
            NodeKind k;
            auto     t = peek_prev();
            switch (t.type) {
                case TokenType::Star: k = NodeKind::Mul; break;
                case TokenType::Slash: k = NodeKind::Div; break;
                case TokenType::Percent: k = NodeKind::Mod; break;
                default: __builtin_unreachable();
            }

            auto rhs = parse_cast();

            auto span = ast.get(lhs)->span.extend(ast.get(rhs)->span);
            lhs = ast.new_node_binary(k, span, lhs, rhs);
        }

        return lhs;
    }

    // cast ::= unary { "as" unary }
    //        | unary "or_else" expr
    //        | unary "or_return"
    //        ;
    auto parse_cast() -> NodeHandle {
        // TODO: unary { "as" unary }
        // TODO: unary "or_else" expr
        // TODO: unary "or_return"

        auto lhs = parse_unary();

        if (match_kw("or_else")) {
            auto rhs = parse_expr();

            return ast.new_node_binary(
                NodeKind::OrElse, ast.get(lhs)->span.extend(ast.get(rhs)->span),
                lhs, rhs);
        }

        if (match_kw("or_return")) {
            return ast.new_node_unary(NodeKind::OrReturn,
                                      ast.get(lhs)->span.extend(prev_span()),
                                      lhs);
        }

        while (match_kw("as")) {
            auto rhs = parse_unary();

            lhs = ast.new_node_binary(
                NodeKind::Cast, ast.get(lhs)->span.extend(ast.get(rhs)->span),
                lhs, rhs);
        }

        return lhs;
    }

    // unary ::= ( "!" | "+" | "-" | "~" | "&" | "?" ) unary
    //         | deref
    //         ;
    auto parse_unary() -> NodeHandle {
        auto t = span();
        if (match(TokenType::Bang)) {
            auto child = parse_unary();
            return ast.new_node_unary(NodeKind::LogicNot,
                                      t.extend(ast.get(child)->span), child);
        }

        if (match(TokenType::Plus)) {
            auto child = parse_unary();
            return ast.new_node_unary(NodeKind::Plus,
                                      t.extend(ast.get(child)->span), child);
        }

        if (match(TokenType::Minus)) {
            auto child = parse_unary();
            return ast.new_node_unary(NodeKind::Neg,
                                      t.extend(ast.get(child)->span), child);
        }

        if (match(TokenType::Tilde)) {
            auto child = parse_unary();
            return ast.new_node_unary(NodeKind::BinNot,
                                      t.extend(ast.get(child)->span), child);
        }

        if (match(TokenType::Ampersand)) {
            auto child = parse_unary();
            return ast.new_node_unary(NodeKind::AddrOf,
                                      t.extend(ast.get(child)->span), child);
        }

        if (match(TokenType::Question)) {
            auto child = parse_unary();
            return ast.new_node_unary(NodeKind::Optional,
                                      t.extend(ast.get(child)->span), child);
        }

        return parse_deref();
    }

    // deref ::= call { ".*" call } ;
    auto parse_deref() -> NodeHandle {
        auto lhs = parse_call();

        while (match(TokenType::DotStar)) {
            lhs = ast.new_node_unary(
                NodeKind::Deref, ast.get(lhs)->span.extend(prev_span()), lhs);
        }

        return lhs;
    }

    // call ::= field { "(" call_args ")" }      (* func call *)
    //        | field { "[" slice_or_index "]" } (* index/slicing *)
    //        ;
    auto parse_call() -> NodeHandle {
        auto lhs = parse_field();

        if (match(TokenType::Lparen)) {
            auto args = parse_call_args();
            try(consume(TokenType::Rparen));

            return ast.new_node_call(ast.get(0)->span.extend(prev_span()), lhs,
                                     args);
        }

        // TODO: primary { "[" slice_or_index "]" } (* index/slicing *)

        return lhs;
    }

    // field ::= primary { "." ID }
    auto parse_field() -> NodeHandle {
        auto lhs = parse_primary();

        while (match(TokenType::Dot)) {
            auto t = peek();
            try(consume(TokenType::Id));

            lhs = ast.new_node_field(ast.get(lhs)->span.extend(t.span), lhs,
                                     std::string{t.span.str(source)});
        }

        return lhs;
    }

    // primary ::= "(" expr ")"
    //           | "." ID
    //           | expr_pack
    //           | array
    //           | pointer_type
    //           | struct_type
    //           | enum_type
    //           | number
    //           | STR
    //           | ID
    //           ;
    auto parse_primary() -> NodeHandle {
        // "(" expr ")" and expr_pack are together here
        if (match(TokenType::Lparen)) {
            auto child = parse_expr();

            if (check(TokenType::Comma)) {
                std::vector items{child};

                while (match(TokenType::Comma)) {
                    items.push_back(parse_expr());
                }

                auto end = ast.get(items.at(items.size() - 1))->span;
                auto span = ast.get(items.at(0))->span.extend(end);
                child = ast.new_node_expr_pack(span, items);
            }

            try(consume(TokenType::Rparen));
            return child;
        }

        if (match(TokenType::Dot)) {
            auto s = prev_span();
            auto t = peek();
            try(consume(TokenType::Id));

            return ast.new_node_enumlit(s.extend(t.span),
                                        std::string{t.span.str(source)});
        }

        if (match(TokenType::Id)) {
            return ast.new_node_id(prev_span(),
                                   std::string{prev_span().str(source)});
        }

        return parse_number();
    }

    // number ::= INT | HEX | OCT | BIN | FLOAT ;
    auto parse_number() -> NodeHandle {
        auto t = peek_and_advance();
        if (t.is_int()) {
            uint64_t value = 0xDEAD'BEEF;

            // FIXME: use something better than allocating a string for removing
            // a character
            std::string s{t.span.str(source)};
            if (s.contains('_')) {
                // NOLINTNEXTLINE(modernize-use-ranges)
                s.erase(std::remove(s.begin(), s.end(), '_'), s.end());
            }

            // NOTE: ignoring errors
            auto [ptr, ec] =
                std::from_chars(s.data(), s.data() + s.length(), value);
            if (ec != std::errc{}) {
                er->report_bug(t.span, "invalid integer (from_chars)");
            }

            return ast.new_node_int(t.span, value);
        }

        if (t.is_hex()) {
            uint64_t value = 0xDEAD'BEEF;

            // FIXME: use something better than allocating a string for removing
            // a character
            std::string s{t.span.str(source)};
            if (s.contains('_')) {
                // NOLINTNEXTLINE(modernize-use-ranges)
                s.erase(std::remove(s.begin(), s.end(), '_'), s.end());
            }

            // NOTE: ignoring errors
            auto [ptr, ec] =
                std::from_chars(s.data(), s.data() + s.length(), value, 16);
            if (ec != std::errc{}) {
                er->report_bug(t.span,
                               "invalid hexadecimal integer (from_chars)");
            }

            return ast.new_node_int(t.span, value);
        }

        er->report_error(t.span, "expected number, got {}", t.type);
        return ast.new_node_err(t.span);
    }

    // ------------------------------------------------------------------------

    auto parse_call_args() -> std::vector<NodeHandle> {
        std::vector<NodeHandle> args;

        do {
            if (check(TokenType::Rparen)) break;

            args.push_back(parse_expr());
        } while (match(TokenType::Comma));

        return args;
    }

    // ------------------------------------------------------------------------

    std::span<Token const> tokens;
    std::string_view       source;
    ErrorReporter*         er;

    Ast    ast{};
    size_t current{};
};

#undef try

auto parse_expr(std::span<Token const> tokens, std::string_view source,
                ErrorReporter& er) -> std::pair<Ast, NodeHandle> {
    auto p = Parser{.tokens = tokens, .source = source, .er = &er};
    auto a = p.parse_expr();

    return {p.ast, a};
}

auto parse(std::span<Token const> tokens, std::string_view source,
           ErrorReporter& er) -> std::pair<Ast, NodeHandle> {
    auto p = Parser{.tokens = tokens, .source = source, .er = &er};
    auto a = p.parse_file();

    return {p.ast, a};
}

}  // namespace yal
