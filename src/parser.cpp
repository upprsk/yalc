#include "parser.hpp"

#include <fmt/ranges.h>

#include <charconv>
#include <cstddef>
#include <libassert/assert.hpp>
#include <ranges>
#include <span>
#include <string_view>

#include "ast.hpp"
#include "error_reporter.hpp"
#include "location.hpp"
#include "node.hpp"
#include "tokenizer.hpp"

namespace yal {

namespace rv = std::ranges::views;

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
    ast::File*                ast_file;

    ParseOptions const& opt;

    bool silent_reporting = false;

public:
    Parser(std::span<Token const> tokens, LocalErrorReporter const& er,
           ParseOptions const& opt, ast::File* ast_file)
        : tokens{tokens},
          source{er.get_source()},
          er{er},
          ast_file{ast_file},
          opt{opt} {}

    void set_silence(bool silenced) { silent_reporting = silenced; }

    [[nodiscard]] auto get_source() const -> std::string_view { return source; }

    auto parse_source_file()
        -> std::tuple<std::string_view, Location, std::vector<ast::Decl*>> {
        // get rid of comments at the start of the file
        skip_comments();

        auto module_name = parse_module_decl();

        std::vector<ast::Decl*> children;
        while (!is_at_end()) children.push_back(parse_top_decl());

        (void)consume(TokenType::Eof);

        return {module_name.str(source), to_loc(module_name), children};
    }

    auto parse_module_decl() -> Span {
#define consume_and_recover(...)     \
    if (!consume(__VA_ARGS__)) {     \
        recover_parse_module_decl(); \
        return {};                   \
    }

        // TODO: include more info in the error message
        consume_and_recover("module");
        auto id = peek();
        consume_and_recover(TokenType::Id);
        consume_and_recover(TokenType::Semi);

        return id.span;

#undef consume_and_recover
    }

    // ------------------------------------------------------------------------

    auto parse_top_decl() -> ast::Decl* {
        if (opt.verbose) {
            er.report_debug(span(), "parse_top_decl() got '{}'",
                            span().str(source));
        }

        auto attributes = check(TokenType::Attribute)
                              ? parse_attributes()
                              : std::span<ast::DeclAttribute>{};

        if (check("var")) return parse_top_var(attributes);
        if (check("def")) return parse_top_def(attributes);
        if (check("func")) return parse_func(attributes);
        if (check("import")) return parse_import(attributes);

        er.report_error(span(), "expected top-level declaration but got '{}'",
                        span().str(source));

        auto err = ast_file->decl_err(to_loc(span()));
        recover_parse_top_decl();

        return err;
    }

    // ------------------------------------------------------------------------

    auto parse_attributes() -> std::span<ast::DeclAttribute> {
        std::vector<ast::DeclAttribute> attrs;
        while (check(TokenType::Attribute)) {
            auto attr = parse_attribute();
            if (attr) attrs.push_back(*attr);
        }

        return ast_file->alloc_decl_attributes(attrs);
    }

    // NOLINTNEXTLINE(readability-function-cognitive-complexity)
    auto parse_attribute() -> std::optional<ast::DeclAttribute> {
        auto start_span = span();

        // name of the attribute without the leading '@'
        std::string_view qualified_name;
        auto             attribute_name = start_span.str(source).substr(1);
        // NOTE: we have an unconsumed '@something' here every time
        advance();

        if (match(TokenType::Dot)) {
            qualified_name = attribute_name;
            attribute_name = span().str(source);

            if (!consume_id_non_kw()) {
                if (!check(TokenType::Lparen) && !check(TokenType::Attribute) &&
                    !check("var") && !check("def") && !check("func"))
                    return std::nullopt;
            }
        }

        std::vector<ast::Expr*>           args;
        std::vector<ast::DeclAttributeKV> kwargs;

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
                        value = ast_file->expr_err(to_loc(prev_span()));
                        recover_parse_attribute_value();
                    }

                    kwargs.push_back(ast::DeclAttributeKV{
                        .loc = to_loc(key.extend(prev_span())),
                        .name = key.str(source),
                        .value = value});
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

        return ast::DeclAttribute{
            .loc = to_loc(start_span.extend(prev_span())),
            .qualified_name = qualified_name,
            .name = attribute_name,
            .args = ast_file->dupe_exprs(args),
            .kwargs = ast_file->dupe_attribute_kvs(kwargs),
        };
    }

    // ------------------------------------------------------------------------

    auto parse_top_var(std::span<ast::DeclAttribute> attributes) -> ast::Decl* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'var' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types = match(TokenType::Colon) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};
    inits_label:
        auto inits = match(TokenType::Equal) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};

        if (!consume(TokenType::Semi)) {
            recover_parse_top_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        // in the simple case we want to use the single decl version
        if (names.size() <= 1 && types.size() <= 1 && inits.size() <= 1) {
            auto name = names.empty() ? "" : names[0].name;
            auto name_loc = names.empty() ? to_loc(start_span) : names[0].loc;
            auto type = types.empty() ? nullptr : types[0];
            auto init = inits.empty() ? nullptr : inits[0];

            return ast_file->decl_var(to_loc(start_span.extend(prev_span())),
                                      name_loc, name, attributes, type, init);
        }

        return ast_file->decl_multi_var(to_loc(start_span).extend(prev_span()),
                                        attributes, names, types, inits);
    }

    auto parse_top_def(std::span<ast::DeclAttribute> attributes) -> ast::Decl* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'def' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types = match(TokenType::Colon) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};
    inits_label:
        auto inits = match(TokenType::Equal) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};

        if (!consume(TokenType::Semi)) {
            recover_parse_top_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        // in the simple case we want to use the single decl version
        if (names.size() <= 1 && types.size() <= 1 && inits.size() <= 1) {
            auto name = names.empty() ? "" : names[0].name;
            auto name_loc = names.empty() ? to_loc(start_span) : names[0].loc;
            auto type = types.empty() ? nullptr : types[0];
            auto init = inits.empty() ? nullptr : inits[0];

            return ast_file->decl_def(to_loc(start_span.extend(prev_span())),
                                      name_loc, name, attributes, type, init);
        }

        return ast_file->decl_multi_def(to_loc(start_span).extend(prev_span()),
                                        attributes, names, types, inits);
    }

    auto parse_decl_ids() -> std::vector<ast::MultiVarName> {
        std::vector<ast::MultiVarName> names;

        do {
            auto ident = span();
            if (!check(TokenType::Id)) break;

            if (is_kw_and_report(ident)) break;

            advance();

            names.push_back({.name = ident.str(source), .loc = to_loc(ident)});
            if (!match(TokenType::Comma)) break;
        } while (true);

        return names;
    }

    auto parse_decl_types_or_inits() -> std::vector<ast::Expr*> {
        std::vector<ast::Expr*> types;

        do {
            auto expr = parse_expr_without_recover();
            if (!expr) break;

            types.push_back(expr);
            if (!match(TokenType::Comma)) break;
        } while (true);

        return types;
    }

    // ------------------------------------------------------------------------

    auto parse_func(std::span<ast::DeclAttribute> attributes) -> ast::Decl* {
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
        Span             attached_type_span;
        if (match(TokenType::Dot)) {
            attached_type = name;
            attached_type_span = name_span;

            name_span = span();
            name = "";
            if (!consume_id_non_kw()) {
                if (auto r = recover_parse_func_name_with_attached_type(
                        start_span, attributes, name, name_span, attached_type))
                    return r;
            } else {
                name = name_span.str(source);
            }
        }

        auto [params, is_c_varargs] = parse_func_params();

        auto rets_span = prev_span();
        auto rets = std::vector<ast::FuncRet>{};
        if (!check(TokenType::Lbrace) && !check(TokenType::Semi) &&
            !check(TokenType::Eof)) {
            rets_span = span();
            rets = parse_func_ret();
            rets_span = rets_span.extend(prev_span());
        }

        ast::Stmt* body = nullptr;
        if (check(TokenType::Lbrace))
            body = parse_block();
        else
            (void)consume_with_options(TokenType::Semi, TokenType::Lbrace);

        return ast_file->decl_func(to_loc(start_span.extend(prev_span())),
                                   to_loc(rets_span), to_loc(name_span), name,
                                   attached_type, attributes, params, rets,
                                   body, is_c_varargs);
    }

    auto parse_func_params() -> std::pair<std::span<ast::FuncParam>, bool> {
        if (!consume(TokenType::Lparen)) {
            recover_parse_func_paramlist();

            if (!match(TokenType::Lparen)) return {};
        }

        std::vector<ast::FuncParam> params;
        while (!check(TokenType::Rparen)) {
            if (check(TokenType::DotDotDot)) break;

            auto arg = parse_func_param();
            if (arg) params.push_back(*arg);

            if (check(TokenType::Rparen)) break;
            if (!consume_with_note(
                    TokenType::Comma,
                    "expected ',' to separate function arguments")) {
                recover_parse_func_param();
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
        (void)consume_with_note(TokenType::Rparen,
                                "expected ')' after function arguments");

        return std::make_pair(ast_file->alloc_func_params(params),
                              is_c_varargs);
    }

    auto parse_func_param() -> std::optional<ast::FuncParam> {
        auto start_span = span();

        auto is_comptime = false;
        if (match(TokenType::Dolar)) {
            is_comptime = true;
        }

        auto name_span = span();
        if (is_kw_and_report(name_span) ||
            !consume_with_note(TokenType::Id, "expected argument name"))
            return std::nullopt;

        auto       name = name_span.str(source);
        ast::Expr* type = nullptr;
        if (match(TokenType::Colon)) {
            type = parse_expr_without_recover();
        }

        return ast::FuncParam{
            .name = name,
            .loc = to_loc(start_span.extend(prev_span())),
            .type_expr = type,
            .is_comptime = is_comptime,
        };
    }

    auto parse_func_ret() -> std::vector<ast::FuncRet> {
        if (match(TokenType::Lparen)) {
            auto start_span = prev_span();
            auto had_error = false;

            std::vector<ast::FuncRet> rets;
            while (!check(TokenType::Rparen)) {
                auto ret = parse_func_multi_ret_item();
                if (ret) rets.push_back(*ret);

                if (check(TokenType::Rparen)) break;
                if (!consume_with_note(
                        TokenType::Comma,
                        "expected ',' to separate return value types")) {
                    had_error = true;
                    recover_parse_func_ret();
                    if (!match(TokenType::Comma)) break;
                }
            }

            // NOTE: we want to do something when this fails?
            (void)consume_with_note(
                TokenType::Rparen,
                "expected ')' after function return value types");

            auto s = start_span.extend(prev_span());
            if (!had_error && rets.empty()) {
                er.report_error(s, "return list is empty");
            }

            return rets;
        }

        // NOTE: may want custom error handling here?
        auto ret = parse_expr_without_recover();
        if (ret) {
            return {
                ast::FuncRet{.name = "", .loc = ret->loc, .type_expr = ret}
            };
        }

        er.report_note(prev_span(),
                       "when trying to parse function return type");

        recover_parse_func_ret_single();
        return {};
    }

    auto parse_func_multi_ret_item() -> std::optional<ast::FuncRet> {
        // this is a named return, we don't have that yet
        if (check(TokenType::Id) && check_next(TokenType::Colon)) {
            auto name = span();
            advance();  // the name
            advance();  // the :

            // NOTE: may want custom error handling here?
            auto type = parse_expr_without_recover();
            return ast::FuncRet{
                .name = name.str(source),
                .loc = to_loc(name.extend(prev_span())),
                .type_expr = type,
            };
        }

        auto start_span = span();
        auto type = parse_expr_without_recover();
        return ast::FuncRet{
            .name = "",
            .loc = type ? type->loc : to_loc(start_span.extend(prev_span())),
            .type_expr = type,
        };
    }

    // ========================================================================

    auto parse_import(std::span<ast::DeclAttribute> attributes) -> ast::Decl* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'import' here every time
        advance();

        std::vector<std::string_view> path;
        auto                          last_span = Span{};

        do {
            last_span = span();

            if (consume(TokenType::Id)) {
                path.push_back(last_span.str(source));
            }
        } while (!is_at_end() && match(TokenType::Slash));

        auto name = path.back();
        path.pop_back();

        (void)consume(TokenType::Semi);

        return ast_file->decl_import(to_loc(start_span.extend(prev_span())),
                                     to_loc(last_span), name, attributes, path);
    }

    // ========================================================================

    auto parse_block() -> ast::BlockStmt* {
        auto start_span = span();

        if (!consume(TokenType::Lbrace)) PANIC("handle missing '{' in block");

        std::vector<ast::Stmt*> children;
        while (!is_at_end() && !check(TokenType::Rbrace)) {
            auto stmt = parse_stmt();
            children.push_back(stmt);
        }

        (void)consume(TokenType::Rbrace);

        return ast_file->stmt_block(to_loc(start_span.extend(prev_span())),
                                    children);
    }

    // ========================================================================

    constexpr static auto const PREC_CALL = 10;
    constexpr static auto const PREC_UNARY = 9;
    // constexpr static auto const PREC_CAST = 8;
    constexpr static auto const PREC_MUL = 7;
    constexpr static auto const PREC_ADD = 6;
    // constexpr static auto const PREC_SHIFT = 5;
    // constexpr static auto const PREC_COMP = 4;
    // constexpr static auto const PREC_BIT = 3;
    // constexpr static auto const PREC_LOGIC = 2;
    // constexpr static auto const PREC_ASSIGN = 1;
    constexpr static auto const PREC_NONE = 0;

    auto parse_expr_without_recover() -> ast::Expr* {
        return parse_expr_with_precedence(PREC_NONE);
    }

    auto parse_expr() -> ast::Expr* {
        auto start_span = span();
        auto expr = parse_expr_without_recover();
        if (expr) return expr;

        recover_parse_expr();
        return ast_file->expr_err(to_loc(start_span.extend(prev_span())));
    }

    // ========================================================================

    auto parse_expr_with_precedence(int precedence) -> ast::Expr* {
        auto left = parse_expr_prefix();
        if (left == nullptr) return nullptr;

        while (get_precedence(peek()) > precedence) {
            left = parse_expr_infix(left);
        }

        return left;
    }

    auto parse_expr_prefix() -> ast::Expr* {
        auto start_span = span();

        // literally a nop
        if (match(TokenType::Plus))
            return parse_expr_with_precedence(PREC_UNARY);

        if (match(TokenType::Minus)) {
            auto child = parse_expr_with_precedence(PREC_UNARY);
            return ast_file->expr_neg(to_loc(start_span.extend(prev_span())),
                                      child);
        }

        if (match(TokenType::Lparen)) {
            auto expr = parse_expr_without_recover();
            // TODO: this should have some smart recovery

            (void)consume(TokenType::Rparen);
            return expr;
        }

        if (match(TokenType::Lbracket)) {
            return parse_multi_ptr_array_or_slice();
        }

        if (match(TokenType::Star)) {
            auto is_const = false;
            if (match("const")) {
                is_const = true;
            }

            auto inner = parse_expr_with_precedence(PREC_UNARY);

            return ast_file->expr_ptr(to_loc(start_span.extend(prev_span())),
                                      inner, is_const);
        }

        if (match(TokenType::Dot)) {
            std::string_view name;
            if (consume(TokenType::Id)) {
                name = prev_span().str(source);
            }

            return ast_file->expr_kw(to_loc(start_span.extend(prev_span())),
                                     name);
        }

        if (match(TokenType::Int)) return parse_int(start_span);
        if (match(TokenType::Hex)) return parse_int_with_base(start_span, 16);
        if (match(TokenType::Str)) return parse_string(start_span);

        if (match("cast")) {
            // TODO: this should have some smart recovery
            (void)consume(TokenType::Lparen);
            auto type_expr = parse_expr_without_recover();
            (void)consume(TokenType::Rparen);

            auto expr = parse_expr_with_precedence(PREC_UNARY);
            return ast_file->expr_cast(to_loc(start_span.extend(prev_span())),
                                       type_expr, expr);
        }

        // this needs to be after keywords
        if (check(TokenType::Id)) {
            // make sure that we are not trying to do something stupid
            if (is_kw_and_report(start_span)) return nullptr;

            advance();

            return ast_file->expr_id(to_loc(start_span),
                                     start_span.str(source));
        }

        er.report_error(start_span, "expected expression, but got '{}'",
                        start_span.str(source));
        return nullptr;
    }

    auto parse_expr_infix(ast::Expr* left) -> ast::Expr* {
        auto tok = peek();
        advance();

        if (tok.type == TokenType::Dot) {
            if (match(TokenType::Id)) {
                auto name = prev_span();

                return ast_file->expr_field(left->loc.extend(name), left,
                                            name.str(source));
            }

            er.report_error(span(), "expected field name, found {}",
                            span().str(source));
            return ast_file->expr_err(left->loc.extend(span()));
        }

        if (tok.type == TokenType::Lparen) {
            return parse_call(left);
        }

        if (tok.type == TokenType::Ampersand) {
            return ast_file->expr_ref(left->loc.extend(tok.span), left);
        }

        if (tok.type == TokenType::DotStar) {
            return ast_file->expr_deref(left->loc.extend(tok.span), left);
        }

        auto right = parse_expr_with_precedence(get_precedence(tok));

        ast::ExprKind kind;
        switch (tok.type) {
            case TokenType::Plus: kind = ast::ExprKind::Add; break;
            case TokenType::Minus: kind = ast::ExprKind::Sub; break;
            case TokenType::Star: kind = ast::ExprKind::Mul; break;
            case TokenType::Slash: kind = ast::ExprKind::Div; break;
            case TokenType::Percent: kind = ast::ExprKind::Mod; break;
            default:
                UNREACHABLE("unexpected token kind in parse infix", tok, *left);
        }

        return ast_file->expr_arith(left->loc.extend(prev_span()), kind, left,
                                    right);
    }

    // ------------------------------------------------------------------------

    auto get_precedence(Token const& t) -> int {
        switch (t.type) {
            case TokenType::Plus:
            case TokenType::Minus: return PREC_ADD;

            case TokenType::Star:
            case TokenType::Slash:
            case TokenType::Percent: return PREC_MUL;

            case TokenType::Lparen:
            case TokenType::Dot: return PREC_CALL;

            case TokenType::Ampersand:
            case TokenType::DotStar: return PREC_CALL;

            default: return PREC_NONE;
        }
    }

    // ------------------------------------------------------------------------

    auto parse_multi_ptr_array_or_slice() -> ast::Expr* {
        auto start_span = prev_span();

        if (match(TokenType::Star)) {
            // this is a multi pointer

            // TODO: this should have some smart recovery
            (void)consume(TokenType::Rbracket);

            auto is_const = false;
            if (match("const")) {
                is_const = true;
            }

            auto inner = parse_expr_with_precedence(PREC_UNARY);

            return ast_file->expr_multi_ptr(
                to_loc(start_span.extend(prev_span())), inner, is_const);
        }

        if (match(TokenType::Rbracket)) {
            // this is a multi slice

            auto is_const = false;
            if (match("const")) {
                is_const = true;
            }

            auto inner = parse_expr_with_precedence(PREC_UNARY);

            return ast_file->expr_slice(to_loc(start_span.extend(prev_span())),
                                        inner, is_const);
        }

        // this is an array
        auto count = parse_expr_without_recover();
        if (!count) recover_parse_array_count();

        (void)consume(TokenType::Rbracket);

        auto is_const = false;
        if (match("const")) {
            is_const = true;
        }

        auto inner = parse_expr_with_precedence(PREC_UNARY);

        return ast_file->expr_array(to_loc(start_span.extend(prev_span())),
                                    count, inner, is_const);
    }

    auto parse_call(ast::Expr* callee) -> ast::Expr* {
        auto args_span = prev_span();  // span of opening (

        std::vector<ast::Expr*> args;
        while (!check(TokenType::Rparen)) {
            auto arg = parse_expr_without_recover();
            if (arg) args.push_back(arg);

            if (check(TokenType::Rparen)) break;
            if (!consume_with_note(TokenType::Comma,
                                   "expected ',' to separate call arguments")) {
                recover_parse_call_arg();
                if (!match(TokenType::Comma)) break;
            }
        }

        (void)consume(TokenType::Rparen);
        args_span = args_span.extend(prev_span());

        return ast_file->expr_call(callee->loc.extend(prev_span()),
                                   to_loc(args_span), callee, args);
    }

    // ------------------------------------------------------------------------

    auto parse_int(Span const& span) -> ast::Expr* {
        // TODO: do not use replace and a dynamic string here
        auto s = std::string{span.str(source)};
        s.erase(begin(std::ranges::remove(s, '_')), s.end());

        uint64_t v;
        auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), v);
        if (ec != std::errc{} || ptr != s.data() + s.size()) {
            er.report_bug(span, "invalid integer found in parser: '{}'", s);
            return ast_file->expr_err(to_loc(span));
        }

        return ast_file->expr_int(to_loc(span), v);
    }

    auto parse_int_with_base(Span const& span, int base) -> ast::Expr* {
        // TODO: do not use replace and a dynamic string here
        auto s = std::string{span.str(source).substr(2)};
        s.erase(begin(std::ranges::remove(s, '_')), s.end());

        uint64_t v;
        auto [ptr, ec] =
            std::from_chars(s.data(), s.data() + s.size(), v, base);
        if (ec != std::errc{} || ptr != s.data() + s.size()) {
            er.report_bug(span, "invalid integer found in parser: '{}'", s);
            return ast_file->expr_err(to_loc(span));
        }

        return ast_file->expr_int(to_loc(span), v);
    }

    auto parse_string(Span const& span) -> ast::Expr* {
        auto s = span.str(source);
        s = s.substr(1, s.size() - 2);

        auto result = escape_string(er, span, s);
        return ast_file->expr_string(to_loc(span), result);
    }

    // ========================================================================

    auto parse_stmt() -> ast::Stmt* {
        if (opt.verbose) {
            er.report_debug(span(), "parse_stmt() got '{}'",
                            span().str(source));
        }

        auto start_span = span();
        if (check("var")) return parse_var();
        if (check("def")) return parse_def();
        if (check("return")) return parse_return_stmt();

        auto expr = parse_expr_without_recover();
        if (!expr) {
            recover_parse_expr_stmt();
            expr = ast_file->expr_err(to_loc(start_span.extend(prev_span())));
        }

        if (match(TokenType::Equal)) {
            auto rhs = parse_expr();

            (void)consume_with_note(TokenType::Semi,
                                    "expected end of assigment statement");

            return ast_file->stmt_assign(to_loc(start_span.extend(prev_span())),
                                         expr, rhs);
        }

        if (match(TokenType::Comma)) {
            return parse_multi_assignment(expr);
        }

        (void)consume_with_note(TokenType::Semi,
                                "expected end of expression statement");

        auto s = start_span.extend(prev_span());
        return ast_file->stmt_expr(to_loc(s), expr);
    }

    auto parse_var() -> ast::Stmt* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'var' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types = match(TokenType::Colon) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};
    inits_label:
        auto inits = match(TokenType::Equal) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};

        if (!consume(TokenType::Semi)) {
            recover_parse_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        // in the simple case we want to use the single decl version
        if (names.size() <= 1 && types.size() <= 1 && inits.size() <= 1) {
            auto name = names.empty() ? "" : names[0].name;
            auto name_loc = names.empty() ? to_loc(start_span) : names[0].loc;
            auto type = types.empty() ? nullptr : types[0];
            auto init = inits.empty() ? nullptr : inits[0];

            return ast_file->stmt_var(to_loc(start_span.extend(prev_span())),
                                      name_loc, name, type, init);
        }

        return ast_file->stmt_multi_var(to_loc(start_span).extend(prev_span()),
                                        names, types, inits);
    }

    auto parse_def() -> ast::Stmt* {
        auto start_span = span();

        // NOTE: we have an unconsumed 'def' here every time
        advance();

        auto names = parse_decl_ids();

    types_label:
        auto types = match(TokenType::Colon) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};
    inits_label:
        auto inits = match(TokenType::Equal) ? parse_decl_types_or_inits()
                                             : std::vector<ast::Expr*>{};

        if (!consume(TokenType::Semi)) {
            recover_parse_top_def_or_var();

            // in case we recovered with a colon, then try types again
            if (check(TokenType::Colon)) goto types_label;

            // in case we recovered with an equals, then try to use it as the
            // inits
            if (check(TokenType::Equal)) goto inits_label;

            if (check(TokenType::Semi)) advance();
        }

        // in the simple case we want to use the single decl version
        if (names.size() <= 1 && types.size() <= 1 && inits.size() <= 1) {
            auto name = names.empty() ? "" : names[0].name;
            auto name_loc = names.empty() ? to_loc(start_span) : names[0].loc;
            auto type = types.empty() ? nullptr : types[0];
            auto init = inits.empty() ? nullptr : inits[0];

            return ast_file->stmt_def(to_loc(start_span.extend(prev_span())),
                                      name_loc, name, type, init);
        }

        return ast_file->stmt_multi_def(to_loc(start_span).extend(prev_span()),
                                        names, types, inits);
    }

    auto parse_return_stmt() -> ast::Stmt* {
        auto start_span = span();
        auto had_error = false;

        // skip over the 'return'
        advance();

        auto children_span = span();

        std::vector<ast::Expr*> rets;
        while (!check(TokenType::Semi)) {
            auto ret = parse_expr_without_recover();
            if (ret) rets.push_back(ret);

            if (check(TokenType::Semi)) break;
            if (!consume_with_options(TokenType::Comma, TokenType::Semi)) {
                had_error = true;
                break;
            }
        }

        children_span = children_span.extend(prev_span());

        // NOTE: we want to do something when this fails?
        if (!had_error) {
            (void)consume_with_note(
                TokenType::Semi, "expected ';' after function return values");
        }

        return ast_file->stmt_return(to_loc(start_span.extend(prev_span())),
                                     to_loc(children_span), rets);
    }

    auto parse_multi_assignment(ast::Expr* first_lhs) -> ast::Stmt* {
        std::vector<ast::Expr*> lhs{first_lhs};
        std::vector<ast::Expr*> rhs;

        do {
            auto expr = parse_expr();
            lhs.push_back(expr);
        } while (match(TokenType::Comma));

        if (!consume_with_note(TokenType::Equal,
                               "expected '=' for multiple assignment")) {
            recover_parse_multi_assign();
            return ast_file->stmt_multi_assign(
                first_lhs->loc.extend(lhs.back()->loc), lhs, {});
        }

        do {
            auto expr = parse_expr();
            rhs.push_back(expr);
        } while (match(TokenType::Comma));

        (void)consume_with_note(TokenType::Semi,
                                "expected end of assignment statement");

        return ast_file->stmt_multi_assign(first_lhs->loc.extend(prev_span()),
                                           lhs, rhs);
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
                       TokenType::Semi, TokenType::Rbrace, TokenType::Rparen,
                       TokenType::Attribute, "var", "def", "func", "return");
    }

    void recover_parse_func_param() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Rparen,
                       TokenType::Lbrace, TokenType::Semi, TokenType::Attribute,
                       "var", "def", "func");
    }

    void recover_parse_func_paramlist() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Lparen,
                       TokenType::Rparen, TokenType::Lbrace, TokenType::Semi,
                       TokenType::Attribute, "var", "def", "func");
    }

    void recover_parse_func_ret() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Rparen,
                       TokenType::Semi, TokenType::Lbrace, TokenType::Attribute,
                       "var", "def", "func");
    }

    void recover_parse_func_ret_single() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Semi,
                       TokenType::Lbrace, TokenType::Attribute, "var", "def",
                       "func");
    }

    void recover_parse_expr() {
        skip_while_not(TokenType::Eof, TokenType::Semi, "var", "def", "func",
                       "return");
        if (check(TokenType::Semi)) advance();
    }

    void recover_parse_call_arg() {
        skip_while_not(TokenType::Eof, TokenType::Comma, TokenType::Rparen,
                       TokenType::Lbrace, TokenType::Semi, TokenType::Attribute,
                       "var", "def", "func");
    }

    void recover_parse_array_count() {
        skip_while_not(TokenType::Eof, TokenType::Rbracket, TokenType::Semi,
                       "const", "var", "def", "func", "return");
        if (check(TokenType::Semi)) advance();
    }

    void recover_parse_multi_assign() {
        skip_while_not(TokenType::Eof, TokenType::Semi, "var", "def", "func",
                       "return");
        if (check(TokenType::Semi)) advance();
    }

    void recover_parse_expr_stmt() {
        skip_while_not(TokenType::Eof, TokenType::Semi, "var", "def", "func",
                       "return");
        if (check(TokenType::Semi)) advance();
    }

    // ------------------------------------------------------------------------

    [[nodiscard]] auto recover_parse_func_name(Span const& start_span)
        -> ast::ErrDecl* {
        skip_while_not(TokenType::Eof, TokenType::Dot, TokenType::Lparen,
                       TokenType::Semi, TokenType::Attribute, "var", "def",
                       "func");

        // in case we are at the end, just abort
        if (is_at_end())
            return ast_file->decl_err(to_loc(start_span.extend(prev_span())));

        // too far, we can not recover this
        if (check(TokenType::Semi) || is_kw(span())) {
            (void)match(TokenType::Semi);
            return ast_file->decl_err(to_loc(start_span.extend(prev_span())));
        }

        return nullptr;
    }

    [[nodiscard]] auto recover_parse_func_name_with_attached_type(
        Span const& start_span, std::span<ast::DeclAttribute> attributes,
        std::string_view name, Span name_span, std::string_view attached_type)
        -> ast::FuncDecl* {
        skip_while_not(TokenType::Eof, TokenType::Lparen, TokenType::Semi,
                       TokenType::Attribute, "var", "def", "func");

        auto s = start_span.extend(prev_span());

        // in case we are at the end, just abort
        if (is_at_end())
            return ast_file->decl_func(to_loc(s), to_loc(name_span),
                                       to_loc(name_span), name, attached_type,
                                       attributes, {}, {}, nullptr, false);

        // too far, we can not recover this
        if (check(TokenType::Semi) || is_kw(span())) {
            (void)match(TokenType::Semi);
            return ast_file->decl_func(to_loc(s), to_loc(name_span),
                                       to_loc(name_span), name, attached_type,
                                       attributes, {}, {}, nullptr, false);
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
        return s == "var" || s == "def" || s == "func" || s == "return" ||
               s == "import" || s == "cast";
    }

    // ========================================================================

    [[nodiscard]] constexpr auto peek() const -> Token {
        return tokens[current_token];
    }

    [[nodiscard]] constexpr auto peek_prev() const -> Token {
        // in case we are at the first token, return EOF. This is safe because
        // we will always have at least one token in `tokens`, the EOF token.
        if (current_token == 0) return tokens[tokens.size() - 1];

        for (size_t idx = current_token - 1; idx > 0; --idx) {
            if (!tokens[idx].is_comment()) return tokens[idx];
        }

        // all tokens were comments, return the latest one?
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

    // NOTE: depends on peek
    constexpr void report_consume(auto&& tt) {
        if (silent_reporting) return;

        if (peek().has_chars())
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            span().str(source));
        else
            er.report_error(span(), "expected '{}', but got '{}'", tt,
                            peek().type);
    }

    constexpr void build_options_string(auto&& /*unused*/) {}
    constexpr void build_options_string(auto&& out, auto&& tt, auto&&... rest) {
        fmt::format_to(out, " or '{}'", tt);
        return build_options_string(out, rest...);
    }

    // NOTE: depends on peek
    constexpr void report_consume_with_options(auto&& ftt, auto&&... tts) {
        if (silent_reporting) return;

        std::string out;
        build_options_string(std::back_inserter(out), tts...);

        if (peek().has_chars())
            er.report_error(span(), "expected '{}'{}, but got '{}'", ftt, out,
                            span().str(source));
        else
            er.report_error(span(), "expected '{}'{}, but got '{}'", ftt, out,
                            peek().type);
    }

    template <typename... T>
    [[nodiscard]] constexpr auto consume_with_note(TokenType                tt,
                                                   fmt::format_string<T...> fmt,
                                                   T&&... args) -> bool {
        return consume_with_note_impl(tt, fmt, fmt::make_format_args(args...));
    }

    [[nodiscard]] constexpr auto consume_with_options(TokenType tt,
                                                      auto&&... other_opts)
        -> bool {
        if (match(tt)) return true;

        constexpr auto other_opts_size = sizeof...(other_opts);
        if (other_opts_size == 0) {
            report_consume(tt);
            return false;
        }

        report_consume_with_options(tt, other_opts...);
        return false;
    }

    [[nodiscard]] constexpr auto consume(TokenType tt) -> bool {
        if (match(tt)) return true;

        report_consume(tt);
        return false;
    }

    [[nodiscard]] constexpr auto consume(std::string_view tt) -> bool {
        if (match(tt)) return true;

        report_consume(tt);
        return false;
    }

    [[nodiscard]] constexpr auto consume_with_note_impl(TokenType        tt,
                                                        fmt::string_view fmt,
                                                        fmt::format_args args)
        -> bool {
        if (match(tt)) return true;

        report_consume(tt);
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

void parse_into_ast_file(std::span<Token const> tokens, ast::File& ast_file,
                         LocalErrorReporter const& er,
                         ParseOptions const&       opt) {
    auto p = Parser{tokens, er, opt, &ast_file};
    auto [module_name, module_name_loc, decls] = p.parse_source_file();

    DEBUG_ASSERT(ast_file.get_module_name() == "");
    DEBUG_ASSERT(ast_file.get_declarations().size() == 0);

    ast_file.set_module_name(module_name, module_name_loc);
    ast_file.append_declarations(std::move(decls));
}

auto parse_module_declaration(std::span<Token const>    tokens,
                              LocalErrorReporter const& er,
                              ParseOptions const& opt) -> ast::ModuleDecl {
    auto p = Parser{tokens, er, opt, nullptr};
    p.set_silence(true);

    auto module_name_span = p.parse_module_decl();

    return {
        .name = std::string{module_name_span.str(p.get_source())},
        .name_loc = p.to_loc(module_name_span),
        .file = er.get_fileid(),
    };
}

}  // namespace yal
