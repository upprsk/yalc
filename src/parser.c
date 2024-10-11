#include "parser.h"

#include <stdbool.h>
#include <stdint.h>

#include "ast.h"
#include "da/da.h"
#include "errors.h"
#include "slice/slice.h"
#include "span.h"
#include "tokenizer.h"
#include "utils/strconv.h"

typedef struct parser {
    allocator_t alloc;
    allocator_t temp_alloc;

    str_t         source;
    slice_token_t tokens;

    ast_t* ast;

    error_reporter_t* er;
} parser_t;

typedef enum parse_result {
    PRES_OK,
    PRES_UNEXP,
    PRES_EEXPR,
} pr_t;

/// Get the current token to be parsed.
static inline token_t peek(parser_t const* p) { return slice_at(p->tokens, 0); }

/// Get the type of the current token to be parsed.
static inline token_type_t peek_tt(parser_t const* p) { return peek(p).type; }

/// If we have reached the end of the token stream.
static inline bool is_at_end(parser_t const* p) { return peek_tt(p) == TT_EOF; }

/// Get the next token to be parsed.
static inline token_t peek_next(parser_t const* p) {
    size_t idx = is_at_end(p) ? 0 : 1;
    return slice_at(p->tokens, idx);
}

/// Get the type of the next token to be parsed.
static inline token_type_t peek_next_tt(parser_t const* p) {
    return peek_next(p).type;
}

/// Advance to the next token.
static inline void advance(parser_t* p) {
    if (!is_at_end(p)) slice_shift(&p->tokens);
}

/// If the current token has the expected type, advance and return true.
/// Otherwise return false.
static bool match(parser_t* p, token_type_t expected) {
    if (peek_tt(p) == expected) {
        advance(p);
        return true;
    }

    return false;
}

/// If the current token has the expected type advance and returns ok. Otherwise
/// it is an error and returns PRES_UNEXP.
static pr_t consume(parser_t* p, token_type_t expected) {
    if (match(p, expected)) return PRES_OK;

    token_t t = peek(p);
    str_t   s = span_to_slice(t.span, p->source);
    report_error(p->er, t.span, "unexpectd token \"%.*s\" (%s), expected %s",
                 (int)s.len, s.ptr, token_to_str(t.type),
                 token_to_str(expected));

    return PRES_UNEXP;
}

/// Recover from a parsing failure.
static void recover(parser_t* p) {
    report_warn(p->er, peek(p).span, "recover not implemente");
    advance(p);
}

/// Helper to call recover and return with the given parse status.
#define do_recover(_p)          \
    {                           \
        recover(_p);            \
        return (node_ref_t){0}; \
    }

/// Helper to call consume, recover if an error happens and return.
#define try_consume(_p, _tt)                         \
    do {                                             \
        pr_t pr;                                     \
        if ((pr = consume(_p, _tt))) do_recover(_p); \
    } while (0)

static int const  unary_precedence = 50;
static inline int get_precedence(token_type_t tt) {
    switch (tt) {
        case TT_PLUS:
        case TT_MINUS: return 10;
        case TT_STAR:
        case TT_SLASH: return 20;
        case TT_LPAREN: return 70;
        default: return 0;
    }
}

static str_t validate_string_lit(parser_t* p, str_t s) {
    assert_char(slice_at(s, 0), ==, '"');
    assert_char(slice_at(s, s.len - 1), ==, '"');

    slice_shift(&s);
    slice_pop(&s);

    return str_dupe(p->alloc, s);
}

// ----------------------------------------------------------------------------
// Parsing functions
// ----------------------------------------------------------------------------

static node_ref_t parse_mod(parser_t* p);

static node_ref_t parse_decl(parser_t* p);
static node_ref_t parse_blk_decl(parser_t* p, span_t start,
                                 node_decl_flags_t flags, str_t extern_name);

static node_ref_t parse_expr(parser_t* p, int precedence);
static node_ref_t parse_prefix(parser_t* p);
static node_ref_t parse_infix(parser_t* p, node_ref_t left);

static node_ref_t parse_unary(parser_t* p);
static node_ref_t parse_proc(parser_t* p);
static node_ref_t parse_ptr(parser_t* p);
static node_ref_t parse_int(parser_t* p);
static node_ref_t parse_ident(parser_t* p);

static node_ref_t parse_additive(parser_t* p, node_ref_t left);
static node_ref_t parse_multiplicative(parser_t* p, node_ref_t left);
static node_ref_t parse_call(parser_t* p, node_ref_t left);

static da_node_ref_t parse_call_args(parser_t* p);

static da_node_ref_t parse_proc_args(parser_t* p, bool* is_varargs);
static node_ref_t    parse_proc_arg(parser_t* p);

static node_ref_t parse_blk(parser_t* p);
static node_ref_t parse_blk_item(parser_t* p);

static node_ref_t parse_stmt(parser_t* p);
static node_ref_t parse_stmt_ret(parser_t* p);
static node_ref_t parse_stmt_expr(parser_t* p);
static node_ref_t parse_stmt_if(parser_t* p);
static node_ref_t parse_stmt_while(parser_t* p);
static node_ref_t parse_stmt_assign(parser_t* p, node_ref_t node_ref,
                                    node_ref_t left);

// ----------------------------------------------------------------------------

// <mod> ::= { <decl> } ;
static node_ref_t parse_mod(parser_t* p) {
    node_ref_t    node_ref = ast_alloc_node(p->ast, NULL);
    da_node_ref_t children = da_init(p->temp_alloc);

    span_t span = peek(p).span;

    while (!is_at_end(p)) {
        node_ref_t child = parse_decl(p);
        da_push_back(&children, child);

        span.end = ast_get_span(p->ast, child).end;
    }

    node_arr_t children_refs = ast_add_refs(p->ast, children);
    node_init_mod(ast_get(p->ast, node_ref), span, children_refs);

    return node_ref;
}

// <decl> ::= [ "extern" [ <string> ] ] <blk_decl> ;
static node_ref_t parse_decl(parser_t* p) {
    span_t            start = peek(p).span;
    str_t             extern_name = {};
    node_decl_flags_t flags = 0;

    if (match(p, TT_EXTERN)) {
        flags |= DECL_EXTERN;

        if (peek_tt(p) == TT_STR) {
            token_t t = peek(p);
            advance(p);

            extern_name =
                validate_string_lit(p, span_to_slice(t.span, p->source));
        }
    }

    return parse_blk_decl(p, start, flags, extern_name);
}

// <blk_decl> ::= <ident> ":" [ <expr> ] [ ( ":" | "=" ) <expr> ] ";" ;
static node_ref_t parse_blk_decl(parser_t* p, span_t start,
                                 node_decl_flags_t flags, str_t extern_name) {
    token_t name_tok = peek(p);
    try_consume(p, TT_IDENT);
    try_consume(p, TT_COLON);

    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);
    node_ref_t type = {};
    node_ref_t init = {};

    if (peek_tt(p) != TT_COLON && peek_tt(p) != TT_EQUAL &&
        peek_tt(p) != TT_SEMICOLON) {
        type = parse_expr(p, 0);
    }

    if (match(p, TT_COLON)) {
        init = parse_expr(p, 0);
    } else if (match(p, TT_EQUAL)) {
        flags |= DECL_VAR;
        init = parse_expr(p, 0);
    }

    span_t end = peek(p).span;
    try_consume(p, TT_SEMICOLON);

    str_t name = str_dupe(p->alloc, span_to_slice(name_tok.span, p->source));
    node_init_decl(ast_get(p->ast, node_ref), span_between(start, end), flags,
                   extern_name, name, type, init);

    return node_ref;
}

// <expr> ::= <prefix> | <infix>;
static node_ref_t parse_expr(parser_t* p, int precedence) {
    node_ref_t left = parse_prefix(p);

    while (get_precedence(peek_tt(p)) > precedence) left = parse_infix(p, left);

    return left;
}

// <prefix> ::= <unary>
//            | <proc>
//            | <ptr>
//            | <int>
//            | <ident>
//            ;
static node_ref_t parse_prefix(parser_t* p) {
    token_t tok = peek(p);

    switch (tok.type) {
        case TT_MINUS:
        case TT_BANG: return parse_unary(p);
        case TT_DOT_LPAREN: return parse_proc(p);
        case TT_STAR: return parse_ptr(p);
        case TT_LBRACKET: return parse_ptr(p);
        case TT_INT: return parse_int(p);
        case TT_IDENT: return parse_ident(p);
        case TT_LPAREN: {
            try_consume(p, TT_LPAREN);
            node_ref_t n = parse_expr(p, 0);
            try_consume(p, TT_RPAREN);

            return n;
        }
        default: break;
    }

    str_t s = span_to_slice(tok.span, p->source);
    report_error(p->er, tok.span, "expected expression, found \"%.*s\" (%s)",
                 (int)s.len, s.ptr, token_to_str(tok.type));

    do_recover(p);
}

// <infix>  ::= <additive>
//            | <multiplicative>
//            | <call>
//            ;
static node_ref_t parse_infix(parser_t* p, node_ref_t left) {
    token_t tok = peek(p);

    switch (tok.type) {
        case TT_PLUS:
        case TT_MINUS: return parse_additive(p, left);
        case TT_STAR:
        case TT_SLASH: return parse_multiplicative(p, left);
        case TT_LPAREN: return parse_call(p, left);
        default: break;
    }

    node_t* left_node = ast_get(p->ast, left);

    str_t s = span_to_slice(tok.span, p->source);
    report_error(p->er, tok.span,
                 "expected rest of %s expression, found \"%.*s\" (%s)",
                 node_kind_str(left_node->kind), (int)s.len, s.ptr,
                 token_to_str(tok.type));

    do_recover(p);
}

// <unary>    ::= ( "-" | "!" ) <expr> ;
static node_ref_t parse_unary(parser_t* p) {
    token_t tok = peek(p);
    advance(p);

    node_kind_t kind;
    switch (tok.type) {
        case TT_MINUS: kind = NODE_NEG; break;
        case TT_BANG: kind = NODE_NOT; break;
        default: assert(false);
    }

    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);
    node_ref_t child = parse_expr(p, unary_precedence);

    node_init_unary(ast_get(p->ast, node_ref),
                    span_between(tok.span, ast_get_span(p->ast, child)), kind,
                    child);

    return node_ref;
}

// <proc>     ::= ".(" [ <proc_args> ] ")" [ <expr> ] ( <blk> | "..." ) ;
static node_ref_t parse_proc(parser_t* p) {
    token_t           start = peek(p);
    node_ref_t        node_ref = ast_alloc_node(p->ast, NULL);
    node_ref_t        ret = {};
    node_proc_flags_t flags = 0;

    bool has_vararg = false;

    try_consume(p, TT_DOT_LPAREN);
    da_node_ref_t args_arr = parse_proc_args(p, &has_vararg);
    try_consume(p, TT_RPAREN);

    if (has_vararg) flags |= PROC_HAS_VARARG;

    if (peek_tt(p) != TT_LBRACE && peek_tt(p) != TT_3MINUS) {
        ret = parse_expr(p, 0);
    }

    node_ref_t body = {};
    span_t     end = peek(p).span;
    if (!match(p, TT_3MINUS)) {
        body = parse_blk(p);
        end = ast_get_span(p->ast, body);
    }

    node_arr_t args =
        args_arr.size ? ast_add_refs(p->ast, args_arr) : (node_arr_t){};

    node_init_proc(ast_get(p->ast, node_ref), span_between(start.span, end),
                   flags, args, ret, body);

    return node_ref;
}

// <ptr>      ::= "*" [ "const" ] <expr>
//              | "[" [ <ptr_arr> ] "]" [ "const" ] <expr>
//              ;
// <ptr_arr> ::= "*" [ ":" <expr> "]"
//             | <expr>
//             ;
static node_ref_t parse_ptr(parser_t* p) {
    node_ptr_flags_t flags = 0;
    node_ref_t       term = {0};
    span_t           start = peek(p).span;

    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);

    if (match(p, TT_LBRACKET)) {
        // <ptr_arr>
        if (match(p, TT_RBRACKET)) {
            // a slice

            // TODO: handle terminates slices

            flags |= PTR_SLICE;
            if (match(p, TT_CONST)) flags |= PTR_CONST;

            node_ref_t child = parse_expr(p, 0);
            span_t     end = ast_get_span(p->ast, child);

            node_init_ptr(ast_get(p->ast, node_ref), span_between(start, end),
                          flags, child, term);

            return node_ref;
        }

        if (match(p, TT_STAR)) {
            flags |= PTR_MULTI;

            if (match(p, TT_COLON)) term = parse_expr(p, 0);

            try_consume(p, TT_RBRACKET);
            if (match(p, TT_CONST)) flags |= PTR_CONST;

            node_ref_t child = parse_expr(p, 0);
            span_t     end = ast_get_span(p->ast, child);

            node_init_ptr(ast_get(p->ast, node_ref), span_between(start, end),
                          flags, child, term);

            return node_ref;
        }

        // this would be an array, which is not implemented
        assert(false);
    }

    try_consume(p, TT_STAR);
    if (match(p, TT_CONST)) flags |= PTR_CONST;

    node_ref_t child = parse_expr(p, 0);
    span_t     end = ast_get_span(p->ast, child);

    node_init_ptr(ast_get(p->ast, node_ref), span_between(start, end), flags,
                  child, term);

    return node_ref;
}

// <int>      ::= INTEGER ;
static node_ref_t parse_int(parser_t* p) {
    token_t t = peek(p);
    advance(p);

    str_t    s = span_to_slice(t.span, p->source);
    uint64_t v = parse_uint64(s);

    node_t*    n = NULL;
    node_ref_t ref = ast_alloc_node(p->ast, &n);
    node_init_int(n, t.span, v);

    return ref;
}

// <ident>    ::= IDENT ;
static node_ref_t parse_ident(parser_t* p) {
    token_t t = peek(p);
    advance(p);

    str_t s = span_to_slice(t.span, p->source);

    node_t*    n = NULL;
    node_ref_t ref = ast_alloc_node(p->ast, &n);
    node_init_ident(n, t.span, str_dupe(p->alloc, s));

    return ref;
}

// <additive>       ::= <expr> ( "+" | "-" ) <expr> ;
static node_ref_t parse_additive(parser_t* p, node_ref_t left) {
    token_type_t tt = peek_tt(p);
    advance(p);

    node_kind_t kind = NODE_INVAL;
    switch (tt) {
        case TT_PLUS: kind = NODE_ADD; break;
        case TT_MINUS: kind = NODE_SUB; break;
        default: assert(false);
    }

    node_ref_t right = parse_expr(p, get_precedence(tt));

    node_t*    n = NULL;
    node_ref_t ref = ast_alloc_node(p->ast, &n);
    node_init_binary(
        n,
        span_between(ast_get_span(p->ast, left), ast_get_span(p->ast, right)),
        kind, left, right);

    return ref;
}

// <multiplicative> ::= <expr> ( "*" | "/" ) <expr> ;
static node_ref_t parse_multiplicative(parser_t* p, node_ref_t left) {
    token_type_t tt = peek_tt(p);
    advance(p);

    node_kind_t kind = NODE_INVAL;
    switch (tt) {
        case TT_STAR: kind = NODE_MUL; break;
        case TT_SLASH: kind = NODE_DIV; break;
        default: assert(false);
    }

    node_ref_t right = parse_expr(p, get_precedence(tt));

    node_t*    n = NULL;
    node_ref_t ref = ast_alloc_node(p->ast, &n);
    node_init_binary(
        n,
        span_between(ast_get_span(p->ast, left), ast_get_span(p->ast, right)),
        kind, left, right);

    return ref;
}

// <call>           ::= <expr> "(" [ <call_args> ] ")" ;
static node_ref_t parse_call(parser_t* p, node_ref_t left) {
    try_consume(p, TT_LPAREN);

    node_ref_t    node_ref = ast_alloc_node(p->ast, NULL);
    da_node_ref_t args = parse_call_args(p);

    span_t end = peek(p).span;
    try_consume(p, TT_RPAREN);

    node_init_call(ast_get(p->ast, node_ref),
                   span_between(ast_get_span(p->ast, left), end), left,
                   ast_add_refs(p->ast, args));

    return node_ref;
}

// <call_args> ::= <expr> { "," <expr> } [ "," ] ;
static da_node_ref_t parse_call_args(parser_t* p) {
    da_node_ref_t args = da_init(p->temp_alloc);

    while (!is_at_end(p) && peek_tt(p) != TT_RPAREN) {
        node_ref_t arg_ref = parse_expr(p, 0);
        da_push_back(&args, arg_ref);

        if (!match(p, TT_COMMA)) break;
    }

    return args;
}

// <proc_args> ::= <proc_arg> { "," <proc_arg> } [ "," ] ;
static da_node_ref_t parse_proc_args(parser_t* p, bool* is_varargs) {
    da_node_ref_t args = da_init(p->temp_alloc);

    while (!is_at_end(p) && peek_tt(p) != TT_RPAREN) {
        if (match(p, TT_DOT_DOT_DOT)) {
            *is_varargs = true;
            match(p, TT_COMMA);

            break;
        }

        node_ref_t arg_ref = parse_proc_arg(p);
        da_push_back(&args, arg_ref);

        if (!match(p, TT_COMMA)) break;
    }

    return args;
}

// <proc_arg>  ::= <ident> [ ":" <expr> ] ;
static node_ref_t parse_proc_arg(parser_t* p) {
    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);
    token_t    name_tok = peek(p);
    span_t     span = name_tok.span;

    try_consume(p, TT_IDENT);

    node_ref_t type = {};
    if (match(p, TT_COLON)) {
        type = parse_expr(p, 0);
        span = span_between(span, ast_get_span(p->ast, type));
    }

    node_init_arg(ast_get(p->ast, node_ref), span,
                  str_dupe(p->alloc, span_to_slice(name_tok.span, p->source)),
                  type);

    return node_ref;
}

// <blk>      ::= "{" { <blk_item> } "}" ;
static node_ref_t parse_blk(parser_t* p) {
    try_consume(p, TT_LBRACE);

    node_ref_t    node_ref = ast_alloc_node(p->ast, NULL);
    da_node_ref_t children = da_init(p->temp_alloc);
    span_t        start = peek(p).span;

    while (!is_at_end(p) && peek_tt(p) != TT_RBRACE) {
        node_ref_t child = parse_blk_item(p);
        da_push_back(&children, child);
    }

    try_consume(p, TT_RBRACE);

    span_t     end = peek(p).span;
    node_arr_t refs = ast_add_refs(p->ast, children);
    node_init_blk(ast_get(p->ast, node_ref), span_between(start, end), refs);

    return node_ref;
}

// <blk_item> ::= <blk_decl> | <stmt> ;
static node_ref_t parse_blk_item(parser_t* p) {
    token_t t = peek(p);

    if (peek_next_tt(p) == TT_COLON)
        return parse_blk_decl(p, t.span, 0, (str_t){});

    return parse_stmt(p);
}

// <stmt> ::= <stmt_ret>
//          | <stmt_expr>
//          | <stmt_if>
//          | <stmt_while>
//          | <assign>
//          ;
static node_ref_t parse_stmt(parser_t* p) {
    token_t t = peek(p);

    if (t.type == TT_RETURN) return parse_stmt_ret(p);
    if (t.type == TT_IF) return parse_stmt_if(p);
    if (t.type == TT_WHILE) return parse_stmt_while(p);

    return parse_stmt_expr(p);
}

// <stmt_ret> ::= "return" [ <expr> ] ";" ;
static node_ref_t parse_stmt_ret(parser_t* p) {
    span_t start = peek(p).span;
    try_consume(p, TT_RETURN);

    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);

    node_ref_t child = {};
    if (peek_tt(p) != TT_SEMICOLON) child = parse_expr(p, 0);

    span_t end = peek(p).span;
    try_consume(p, TT_SEMICOLON);

    node_init_ret(ast_get(p->ast, node_ref), span_between(start, end), child);

    return node_ref;
}

// <stmt_expr> ::= <expr> ";" ;
static node_ref_t parse_stmt_expr(parser_t* p) {
    span_t     start = peek(p).span;
    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);
    node_ref_t child = parse_expr(p, 0);

    span_t end = peek(p).span;

    // handle assignment
    if (match(p, TT_EQUAL)) return parse_stmt_assign(p, node_ref, child);

    try_consume(p, TT_SEMICOLON);

    node_init_stmt_expr(ast_get(p->ast, node_ref), span_between(start, end),
                        child);

    return node_ref;
}

// <stmt_if>   ::= "if" <expr> <blk> [ "else" <blk> ] ;
static node_ref_t parse_stmt_if(parser_t* p) {
    span_t start = peek(p).span;
    try_consume(p, TT_IF);

    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);
    node_ref_t cond = parse_expr(p, 0);
    node_ref_t wtrue = parse_blk(p);
    node_ref_t wfalse = {};

    if (match(p, TT_ELSE)) wfalse = parse_blk(p);

    node_init_if(ast_get(p->ast, node_ref),
                 span_between(start, ast_get_span(p->ast, node_ref_valid(wfalse)
                                                              ? wfalse
                                                              : wtrue)),
                 cond, wtrue, wfalse);

    return node_ref;
}

// <stmt_while>   ::= "while" <expr> <blk> ;
static node_ref_t parse_stmt_while(parser_t* p) {
    span_t start = peek(p).span;
    try_consume(p, TT_WHILE);

    node_ref_t node_ref = ast_alloc_node(p->ast, NULL);
    node_ref_t cond = parse_expr(p, 0);
    node_ref_t body = parse_blk(p);

    node_init_while(ast_get(p->ast, node_ref),
                    span_between(start, ast_get_span(p->ast, body)), cond,
                    body);

    return node_ref;
}

// <assign> ::= <expr> "=" <expr> ";" ;
static node_ref_t parse_stmt_assign(parser_t* p, node_ref_t node_ref,
                                    node_ref_t left) {
    node_ref_t right = parse_expr(p, 0);

    span_t end = peek(p).span;
    try_consume(p, TT_SEMICOLON);

    node_init_assign(ast_get(p->ast, node_ref),
                     span_between(ast_get_span(p->ast, left), end), left,
                     right);

    return node_ref;
}

// ----------------------------------------------------------------------------

static node_ref_t parse_file(parser_t* p) {
    node_ref_t r = parse_mod(p);
    consume(p, TT_EOF);

    return r;
}

node_ref_t parse(parser_desc_t* desc, ast_t* ast) {
    assert_not_null(desc);
    assert_not_null(ast);

    parser_t p = {
        .alloc = desc->alloc,
        .temp_alloc = desc->temp_alloc,
        .source = desc->source,
        .tokens = desc->tokens,
        .ast = ast,
        .er = desc->er,
    };

    ast_init(p.ast, p.alloc);

    return parse_file(&p);
}
