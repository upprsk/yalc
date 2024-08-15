#include "parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "allocator.h"
#include "ast.h"
#include "da.h"
#include "errors.h"
#include "span.h"
#include "tokenizer.h"

typedef struct parser {
    uint32_t current;
    uint32_t source_len;

    char const* filename;

    token_t const* tokens;
    char const*    source;

    error_reporter_t* er;

    allocator_t node_alloc;
} parser_t;

static inline token_t peek(parser_t const* p) { return p->tokens[p->current]; }
static inline token_t peek_next(parser_t const* p) {
    return p->tokens[p->current + 1];
}

static inline void advance(parser_t* p) {
    if (peek(p).type != TT_EOF) {
        do {
            p->current++;
        } while (peek(p).type == TT_ERR);
    }
}

static inline token_t next(parser_t* p) {
    token_t t = peek(p);
    advance(p);

    return t;
}

static inline bool match(parser_t* p, token_type_t expected) {
    if (peek(p).type != expected) return false;

    advance(p);
    return true;
}

static void consume(parser_t* p, token_type_t expected) {
    token_t tok = peek(p);

    if (tok.type != expected && tok.type != TT_ERR) {
        uint32_t    litlen;
        char const* lit =
            span_str_parts(tok.span, p->source, p->source_len, &litlen);

        report_error(p->er, p->filename, p->source, tok.span,
                     "unexpected token '%.*s' (%s), expected %s", litlen, lit,
                     token_to_str(tok.type), token_to_str(expected));
    }

    advance(p);
}

static inline node_t* node_err(parser_t* p, span_t span) {
    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){.type = NODE_ERR, .span = span};

    return n;
}

static char const* span_conv_strlit(span_t s, char const* source,
                                    uint32_t source_len, allocator_t alloc,
                                    uint32_t* out_len) {
    uint32_t    len;
    char const* lit = span_str_parts(s, source, source_len, &len);
    munit_assert_char(lit[0], ==, '"');
    munit_assert_char(lit[len - 1], ==, '"');

    ++lit;
    len -= 2;

    char* str = da_init_char(alloc);

#define append(c)                              \
    do {                                       \
        char cc = c;                           \
        str = da_append_char(str, alloc, &cc); \
    } while (0)

    size_t i = 0;
    for (; i < len; ++i) {
        if (lit[i] == '\\') {
            switch (lit[++i]) {
                case 'n': append('\n'); break;
                case 'r': append('\r'); break;
                case 't': append('\t'); break;
                // unrecognized are just copyed over
                default: append(lit[i]); break;
            }
        } else {
            append(lit[i]);
        }
    }

    append(0);
#undef append

    if (out_len) *out_len = i;

    size_t size = da_get_size(str);
    char*  buf = allocator_alloc(alloc, size);
    memcpy(buf, str, size);

    da_free(str, alloc);

    return str;
}

static int const unary_precedence = 30;

static inline int get_precedence(token_type_t tt) {
    switch (tt) {
        case TT_PLUS:
        case TT_MINUS: return 10;
        case TT_STAR:
        case TT_SLASH: return 20;
        case TT_LPAREN:
        case TT_DOT_STAR: return 40;
        default: return 0;
    }
}

typedef enum stmt_opt {
    STMT_OPT_IS_MODULE = 1 << 0,
} stmt_opt_t;

static node_t* parse_prefix(parser_t* p);
static node_t* parse_expr(parser_t* p, int precedence);
static node_t* parse_stmt(parser_t* p, stmt_opt_t opt);

static node_t* parse_int(parser_t* p, token_t tok) {
    uint32_t    len;
    char const* str = span_str_parts(tok.span, p->source, p->source_len, &len);

    uint64_t value = 0;
    for (size_t i = 0; i < len; ++i) {
        value = value * 10 + str[i] - '0';
    }

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_INT, .as.int_ = {.value = value}, .span = tok.span};

    return n;
}

static node_t* parse_str(parser_t* p, token_t tok) {
    uint32_t    len;
    char const* contents = span_conv_strlit(tok.span, p->source, p->source_len,
                                            p->node_alloc, &len);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_STR,
        .as.str = {.str = contents, .len = len},
        .span = tok.span
    };

    return n;
}

static node_t* parse_unary(parser_t* p, token_t tok) {
    unop_type_t op;

    switch (tok.type) {
        case TT_MINUS: op = UNOP_NEG; break;
        default: munit_assert(false);
    }

    node_t* child = parse_expr(p, unary_precedence);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_UNOP,
        .as.unop = {             .type = op,         .child = child},
        .span = {.start = tok.span.start, .end = child->span.end}
    };

    return n;
}

static node_t* parse_binary(parser_t* p, node_t* lhs, token_t tok) {
    binop_type_t op;

    switch (tok.type) {
        case TT_PLUS: op = BINOP_ADD; break;
        case TT_MINUS: op = BINOP_SUB; break;
        case TT_STAR: op = BINOP_MUL; break;
        case TT_SLASH: op = BINOP_DIV; break;
        default: munit_assert(false);
    }

    node_t* rhs = parse_expr(p, get_precedence(tok.type));

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_BINOP,
        .as.binop = {.type = op, .left = lhs, .right = rhs},
        .span = {.start = lhs->span.start, .end = rhs->span.end}
    };

    return n;
}

static node_t* parse_grouping(parser_t* p) {
    node_t* expr = parse_expr(p, 0);
    consume(p, TT_RPAREN);

    return expr;
}

static char const* span_dupe_str(span_t s, char const* source,
                                 uint32_t source_len, allocator_t alloc) {
    uint32_t    len;
    char const* lit = span_str_parts(s, source, source_len, &len);

    char* str = allocator_alloc(alloc, len + 1);
    memcpy(str, lit, len);
    str[len] = 0;

    return str;
}

static node_t* parse_ident(parser_t* p, token_t tok) {
    // FIXME: allocating the string in the same pool as the nodes
    char const* ident =
        span_dupe_str(tok.span, p->source, p->source_len, p->node_alloc);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_IDENT, .as.ident = {.ident = ident}, .span = tok.span};

    return n;
}

static node_t* parse_block(parser_t* p) {
    token_t open_tok = peek(p);
    consume(p, TT_LBRACE);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){.type = NODE_STMT_BLK,
                  .as.stmt_blk.stmts = da_init_node(p->node_alloc),
                  .span.start = open_tok.span.start};

    while (peek(p).type != TT_EOF && peek(p).type != TT_RBRACE) {
        node_t* stmt = parse_stmt(p, 0);
        n->as.stmt_blk.stmts =
            da_append_node(n->as.stmt_blk.stmts, p->node_alloc, &stmt);
    }

    token_t end_tok = peek(p);
    consume(p, TT_RBRACE);

    n->span.end = end_tok.span.end;

    return n;
}

static node_t* parse_proc(parser_t* p, token_t tok) {
    node_t** args = da_init_node(p->node_alloc);

    while (peek(p).type != TT_RPAREN) {
        token_t argname = peek(p);
        consume(p, TT_IDENT);

        node_t* argtype = NULL;
        if (match(p, TT_COLON)) {
            argtype = parse_expr(p, 0);
        }

        // FIXME: allocating the string in the same pool as the nodes
        char const* name = span_dupe_str(argname.span, p->source, p->source_len,
                                         p->node_alloc);

        node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
        *n = (node_t){
            .type = NODE_ARG,
            .as.arg.name = name,
            .as.arg.type = argtype,
            .span = {.start = argname.span.start,
                     .end = argtype ? argtype->span.end : argname.span.end}
        };

        args = da_append_node(args, p->node_alloc, &n);

        if (!match(p, TT_COMMA)) break;
    }

    token_t rparen = peek(p);
    consume(p, TT_RPAREN);

    node_t* return_type = NULL;
    if (match(p, TT_ARROW)) {
        return_type = parse_expr(p, 0);
    }
    node_t* body = NULL;
    if (peek(p).type == TT_LBRACE) body = parse_block(p);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_PROC,
        .as.proc = {.return_type = return_type, .args = args, .body = body},
        .span.start = tok.span.start,
        .span.end = body          ? body->span.end
                    : return_type ? return_type->span.end
                                  : rparen.span.end,
    };

    return n;
}

static node_t* parse_pointer(parser_t* p, token_t tok) {
    node_t* expr = parse_prefix(p);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_PTR,
        .as.ptr = {.child = expr},
        .span.start = tok.span.start,
        .span.end = expr->span.end,
    };

    return n;
}

static node_t* parse_mptr_or_slice(parser_t* p, token_t tok) {
    if (match(p, TT_STAR)) {
        node_t* term = NULL;

        if (match(p, TT_COLON)) {
            // terminated multi pointer
            term = parse_expr(p, 0);
        }

        consume(p, TT_RBRACKET);

        // multi pointer
        node_t* expr = parse_prefix(p);

        node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
        *n = (node_t){
            .type = NODE_MPTR,
            .as.mptr = {.child = expr, .term = term},
            .span.start = tok.span.start,
            .span.end = expr->span.end,
        };

        return n;
    }

    if (match(p, TT_RBRACKET)) {
        // slice
        report_error(p->er, p->filename, p->source, tok.span,
                     "slices not implemented");
        munit_assert(false);
    }

    node_t* array_len = parse_expr(p, 0);
    consume(p, TT_RBRACKET);

    node_t* array_type = parse_expr(p, 0);
    consume(p, TT_LBRACE);

    node_t** initlist = da_init_node(p->node_alloc);
    while (peek(p).type != TT_RBRACE) {
        node_t* arg = parse_expr(p, 0);
        initlist = da_append_node(initlist, p->node_alloc, &arg);

        if (!match(p, TT_COMMA)) break;
    }

    token_t rbrace = peek(p);
    consume(p, TT_RBRACE);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_ARRAY,
        .as.array = {.len = array_len,
                     .type = array_type,
                     .initializer_list = initlist},
        .span.start = tok.span.start,
        .span.end = rbrace.span.end,
    };

    return n;

    // array
    report_error(p->er, p->filename, p->source, tok.span,
                 "arrays not implemented");
    munit_assert(false);
}

static node_t* parse_ref(parser_t* p, token_t tok) {
    node_t* child = parse_expr(p, unary_precedence);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_REF,
        .as.ref = {.child = child},
        .span.start = tok.span.start,
        .span.end = child->span.end,
    };

    return n;
}

static node_t* parse_call(parser_t* p, node_t* lhs) {
    node_t** args = da_init_node(p->node_alloc);

    while (peek(p).type != TT_RPAREN) {
        node_t* arg = parse_expr(p, 0);
        args = da_append_node(args, p->node_alloc, &arg);

        if (!match(p, TT_COMMA)) break;
    }

    token_t rparen = peek(p);
    consume(p, TT_RPAREN);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_CALL,
        .as.call = {.args = args, .callee = lhs},
        .span.start = lhs->span.start,
        .span.end = rparen.span.end,
    };

    return n;
}

static node_t* parse_deref(parser_t* p, node_t* lhs, token_t tok) {
    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_DEREF,
        .as.deref = {.child = lhs},
        .span.start = lhs->span.start,
        .span.end = tok.span.end,
    };

    return n;
}

static node_t* parse_prefix(parser_t* p) {
    token_t tok = next(p);
    switch (tok.type) {
        case TT_INT: return parse_int(p, tok);
        case TT_STR: return parse_str(p, tok);
        case TT_MINUS: return parse_unary(p, tok);
        case TT_LPAREN: return parse_grouping(p);
        case TT_IDENT: return parse_ident(p, tok);
        case TT_DOT_LPAREN: return parse_proc(p, tok);
        case TT_STAR: return parse_pointer(p, tok);
        case TT_LBRACKET: return parse_mptr_or_slice(p, tok);
        case TT_AMPERSAND: return parse_ref(p, tok);
        default: break;
    }

    uint32_t    litlen;
    char const* lit =
        span_str_parts(tok.span, p->source, p->source_len, &litlen);

    report_error(p->er, p->filename, p->source, tok.span,
                 "expected prefix expression, found '%.*s' (%s)", litlen, lit,
                 token_to_str(tok.type));

    return node_err(p, tok.span);
}

static node_t* parse_infix(parser_t* p, node_t* lhs) {
    token_t tok = next(p);
    switch (tok.type) {
        case TT_PLUS:
        case TT_MINUS:
        case TT_STAR:
        case TT_SLASH: return parse_binary(p, lhs, tok);
        case TT_LPAREN: return parse_call(p, lhs);
        case TT_DOT_STAR: return parse_deref(p, lhs, tok);
        default: break;
    }

    uint32_t    litlen;
    char const* lit =
        span_str_parts(tok.span, p->source, p->source_len, &litlen);

    report_error(p->er, p->filename, p->source, tok.span,
                 "expected infix expression, found '%.*s' (%s)", litlen, lit,
                 token_to_str(tok.type));

    return node_err(p, tok.span);
}

static node_t* parse_expr(parser_t* p, int precedence) {
    node_t* left = parse_prefix(p);
    while (get_precedence(peek(p).type) > precedence)
        left = parse_infix(p, left);

    return left;
}

typedef enum decl_opt {
    DECL_OPT_IS_EXTERN = 1 << 0,
} decl_opt_t;

static node_t* parse_decl(parser_t* p, token_t tok, decl_opt_t opt,
                          char const* extern_name) {
    node_t* type = NULL;
    node_t* init = NULL;

    token_t name = next(p);
    if (match(p, TT_COLON)) {
        if (peek(p).type != TT_EQUAL) {
            type = parse_expr(p, 0);
        }
    }

    if (match(p, TT_EQUAL)) {
        init = parse_expr(p, 0);
    }

    token_t end_tok = peek(p);
    consume(p, TT_SEMICOLON);

    char const* name_str =
        span_dupe_str(name.span, p->source, p->source_len, p->node_alloc);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_DECL,
        .as.decl = {.name = name_str,
                    .extern_name = extern_name,
                    .name_span = name.span,
                    .type = type,
                    .init = init,
                    .is_extern = (opt & DECL_OPT_IS_EXTERN) != 0},
        .span = {.start = tok.span.start, .end = end_tok.span.end}
    };

    return n;
}

static node_t* parse_stmt(parser_t* p, stmt_opt_t opt) {
    token_t stmt_start_tok = peek(p);

    if (match(p, TT_RETURN)) {
        node_t* expr = NULL;

        if (peek(p).type != TT_SEMICOLON) {
            expr = parse_expr(p, 0);
        }

        token_t end_tok = peek(p);
        consume(p, TT_SEMICOLON);

        node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
        *n = (node_t){
            .type = NODE_STMT_RET,
            .as.stmt_ret = {.child = expr},
            .span = {.start = stmt_start_tok.span.start,
                            .end = end_tok.span.end}
        };

        return n;
    }

    if (peek(p).type == TT_EXTERN) {
        token_t extern_tok = next(p);

        token_t extern_name_tok = peek(p);
        consume(p, TT_STR);

        char const* extern_name =
            span_conv_strlit(extern_name_tok.span, p->source, p->source_len,
                             p->node_alloc, NULL);
        return parse_decl(p, extern_tok, DECL_OPT_IS_EXTERN, extern_name);
    }

    if (peek_next(p).type == TT_COLON) {
        return parse_decl(p, peek(p), 0, NULL);
    }

    node_t* expr = parse_expr(p, 0);

    if (match(p, TT_EQUAL)) {
        node_t* rhs = parse_expr(p, 0);

        node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
        *n = (node_t){
            .type = NODE_ASSIGN,
            .as.assign = {              .lhs = expr,           .rhs = rhs},
            .span = {.start = expr->span.start, .end = rhs->span.end}
        };

        consume(p, TT_SEMICOLON);
        return n;
    }

    token_t semi = peek(p);
    consume(p, TT_SEMICOLON);

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_STMT_EXPR,
        .as.stmt_expr = {.expr = expr},
        .span = {.start = expr->span.start, .end = semi.span.end}
    };

    return n;
}

node_t* parse(parse_params_t* params) {
    allocator_t node_alloc = {};
    allocator_init_arena(&node_alloc, params->arena);

    parser_t p = {.filename = params->filename,
                  .source_len = params->source_len,
                  .source = params->source,
                  .tokens = params->tokens,
                  .er = params->er,
                  .node_alloc = node_alloc};

    node_t* n = allocator_alloc(p.node_alloc, sizeof(*n));
    *n = (node_t){.type = NODE_MOD,
                  .as.mod.decls = da_init_node(p.node_alloc),
                  .span.start = peek(&p).span.start};

    while (peek(&p).type != TT_EOF) {
        node_t* stmt = parse_stmt(&p, STMT_OPT_IS_MODULE);
        n->span.end = stmt->span.end;
        n->as.mod.decls = da_append_node(n->as.mod.decls, p.node_alloc, &stmt);
    }

    consume(&p, TT_EOF);
    return n;
}
