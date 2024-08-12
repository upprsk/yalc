#include "parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "allocator.h"
#include "ast.h"
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

static token_t peek(parser_t const* p) { return p->tokens[p->current]; }
static void    advance(parser_t* p) { p->current++; }

static token_t next(parser_t* p) {
    token_t t = peek(p);
    advance(p);

    return t;
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

static inline int get_precedence(token_type_t tt) {
    switch (tt) {
        case TT_PLUS:
        case TT_MINUS: return 10;
        case TT_STAR:
        case TT_SLASH: return 20;
        default: return 0;
    }
}

static node_t* parse_prefix(parser_t* p);
static node_t* parse_expr(parser_t* p, int precedence);

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

static node_t* parse_unary(parser_t* p, token_t tok) {
    unop_type_t op;

    switch (tok.type) {
        case TT_MINUS: op = UNOP_NEG; break;
        default: munit_assert(false);
    }

    node_t* child = parse_prefix(p);

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

static node_t* parse_ident(parser_t* p, token_t tok) {
    uint32_t    len;
    char const* lit = span_str_parts(tok.span, p->source, p->source_len, &len);

    // FIXME: allocating the string in the same pool as the nodes
    char* ident = allocator_alloc(p->node_alloc, len + 1);
    memcpy(ident, lit, len);
    ident[len] = 0;

    node_t* n = allocator_alloc(p->node_alloc, sizeof(*n));
    *n = (node_t){
        .type = NODE_IDENT, .as.ident = {.ident = ident}, .span = tok.span};

    return n;
}

static node_t* parse_prefix(parser_t* p) {
    token_t tok = next(p);
    switch (tok.type) {
        case TT_INT: return parse_int(p, tok);
        case TT_MINUS: return parse_unary(p, tok);
        case TT_LPAREN: return parse_grouping(p);
        case TT_IDENT: return parse_ident(p, tok);
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
        default: break;
    }

    munit_assert(false);
}

static node_t* parse_expr(parser_t* p, int precedence) {
    node_t* left = parse_prefix(p);
    while (get_precedence(peek(p).type) > precedence)
        left = parse_infix(p, left);

    return left;
}

void dump_node(node_t* node, int indent) {
    munit_assert_not_null(node);

    for (int i = 0; i < indent; ++i) printf("  ");

    switch (node->type) {
        case NODE_ERR: printf("ERR\n"); break;
        case NODE_INT: printf("INT (%lu)\n", node->as.int_.value); break;
        case NODE_FLOAT: printf("FLOAT (%f)\n", node->as.float_.value); break;
        case NODE_BINOP:
            printf("BINOP ");
            switch (node->as.binop.type) {
                case BINOP_ADD: printf("ADD"); break;
                case BINOP_SUB: printf("SUB"); break;
                case BINOP_MUL: printf("MUl"); break;
                case BINOP_DIV: printf("DIV"); break;
            }

            printf("\n");

            dump_node(node->as.binop.left, indent + 1);
            dump_node(node->as.binop.right, indent + 1);
            break;
        case NODE_IDENT: printf("IDENT '%s'\n", node->as.ident.ident); break;
        case NODE_UNOP:
            printf("UNOP ");
            switch (node->as.unop.type) {
                case UNOP_NEG: printf("NEG"); break;
            }

            printf("\n");
            dump_node(node->as.unop.child, indent + 1);
            break;
    }
}

void parse(error_reporter_t* er, char const* filename, char const* source,
           uint32_t source_len, token_t const* tokens) {
    Arena       arena = {0};
    allocator_t node_alloc = {};
    allocator_init_arena(&node_alloc, &arena);

    parser_t p = {.filename = filename,
                  .source_len = source_len,
                  .source = source,
                  .tokens = tokens,
                  .er = er,
                  .node_alloc = node_alloc};

    node_t* n = parse_expr(&p, 0);

    consume(&p, TT_EOF);

    dump_node(n, 0);

    arena_free(&arena);
}
