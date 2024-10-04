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

    ast_t ast;

    error_reporter_t* er;
} parser_t;

typedef parse_result_t pr_t;

/// Get the current token to be parsed.
static inline token_t peek(parser_t const* p) { return slice_at(p->tokens, 0); }

/// Get the type of the current token to be parsed.
static inline token_type_t peek_tt(parser_t const* p) { return peek(p).type; }

/// If we have reached the end of the token stream.
static inline bool is_at_end(parser_t const* p) { return peek_tt(p) == TT_EOF; }

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

/// Bitmask of places from where we can fail and need different ways of
/// recovering strategies.
typedef enum recover_mode {
    RECM_MOD = 1 << 0,
    RECM_DECL_TYPE = 1 << 1,
    RECM_DECL_INIT = 1 << 2,
} recover_mode_t;

/// Recover from a parsing failure.
static void recover(parser_t* p, recover_mode_t mode) {
    // report_warn(p->er, peek(p).span, "trying to recover (mode=0x%x)", mode);

    do {
        advance(p);
        token_type_t tt = peek_tt(p);

        report_warn(p->er, peek(p).span, "trying to recover (mode=0x%x): %s",
                    mode, token_to_str(tt));

        if (mode & RECM_MOD) {
            if (tt == TT_IDENT) break;
        }

        if (mode & RECM_DECL_TYPE) {
            if (tt == TT_IDENT || tt == TT_COLON || tt == TT_SEMICOLON) break;
        }

        if (mode & RECM_DECL_INIT) {
            if (tt == TT_IDENT || tt == TT_SEMICOLON) break;
        }
    } while (!is_at_end(p));
}

/// Helper to call recover and return with the given parse status.
#define do_recover(_p, _rec)    \
    {                           \
        recover(_p, _rec);      \
        return (node_ref_t){0}; \
    }

/// Helper to call consume, recover if an error happens and return.
#define try_consume(_p, _tt, _rec, _pr) \
    if ((_pr = consume(_p, _tt))) do_recover(_p, _rec)

// ----------------------------------------------------------------------------

static node_ref_t parse_mod(parser_t* p);
static node_ref_t parse_decl(parser_t* p, recover_mode_t rec);
static node_ref_t parse_stmt(parser_t* p, recover_mode_t rec);
static node_ref_t parse_expr(parser_t* p, int precedence, recover_mode_t rec);

static node_ref_t parse_mod(parser_t* p) {
    node_ref_t modn_ref;
    ast_alloc_node(&p->ast, &modn_ref);

    da_node_ref_t children = da_init(p->temp_alloc);

    while (!is_at_end(p)) {
        node_ref_t node = parse_decl(p, RECM_MOD);
        da_push_back(&children, node);
    }

    node_arr_t children_refs = ast_add_refs(&p->ast, children);
    node_t*    mod_node = ast_get(&p->ast, modn_ref);
    *mod_node = (node_t){
        .kind = NODE_MOD,
        .as.w_children = {.children = children_refs},
    };

    return modn_ref;
}

/// Parse a declaration.
static node_ref_t parse_decl(parser_t* p, recover_mode_t rec) {
    pr_t       pr = PRES_OK;
    node_ref_t decln_ref;
    ast_alloc_node(&p->ast, &decln_ref);

    token_t    name = peek(p);
    node_ref_t type = {0};
    node_ref_t init = {0};

    try_consume(p, TT_IDENT, rec, pr);
    try_consume(p, TT_COLON, rec, pr);

    if (peek_tt(p) != TT_COLON) {
        type = parse_expr(p, 0, rec | RECM_DECL_TYPE);
    }

    try_consume(p, TT_COLON, rec, pr);

    if (peek_tt(p) != TT_SEMICOLON) {
        init = parse_expr(p, 0, rec | RECM_DECL_INIT);
    }

    try_consume(p, TT_SEMICOLON, rec, pr);

    str_t name_str = span_to_slice(name.span, p->source);

    node_t* decl_node = ast_get(&p->ast, decln_ref);
    *decl_node = (node_t){
        .kind = NODE_DECL,
        .as.decl = {.name = slice_dupe(p->alloc, name_str),
                    .type = type,
                    .init = init},
    };

    return decln_ref;
}

// ----------------------------------------------------------------------------

static int const  unary_precedence = 50;
static inline int get_precedence(token_type_t tt) {
    switch (tt) {
        default: return 0;
    }
}

// ----------------------------------------------------------------------------

static node_ref_t parse_int(parser_t* p) {
    token_t t = peek(p);
    str_t   s = span_to_slice(t.span, p->source);

    uint64_t v = parse_uint64(s);

    node_ref_t ref = {0};
    node_t*    n = ast_alloc_node(&p->ast, &ref);
    *n = (node_t){
        .kind = NODE_INT,
        .as.int_ = {.value = v},
    };

    return ref;
}

// ----------------------------------------------------------------------------

static node_ref_t parse_prefix(parser_t* p, recover_mode_t rec) {
    token_t tok = peek(p);
    advance(p);

    switch (tok.type) {
        case TT_INT: return parse_int(p);
        default: break;
    }

    str_t s = span_to_slice(tok.span, p->source);
    report_error(p->er, tok.span, "expected expression, found \"%.*s\" (%s)",
                 (int)s.len, s.ptr, token_to_str(tok.type));

    recover(p, rec);
    return (node_ref_t){0};
}

static node_ref_t parse_infix(parser_t* p, node_ref_t left,
                              recover_mode_t rec) {
    token_t tok = peek(p);

    switch (tok.type) {
        default: break;
    }

    node_t* left_node = ast_get(&p->ast, left);

    str_t s = span_to_slice(tok.span, p->source);
    report_error(p->er, tok.span,
                 "expected rest of %s expression, found \"%.*s\" (%s)",
                 node_kind_str(left_node->kind), (int)s.len, s.ptr,
                 token_to_str(tok.type));

    recover(p, rec);
    return (node_ref_t){0};
}

static node_ref_t parse_expr(parser_t* p, int precedence, recover_mode_t rec) {
    node_ref_t left = parse_prefix(p, rec);

    while (get_precedence(peek_tt(p)) > precedence)
        left = parse_infix(p, left, rec);

    return left;
}

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
        .er = desc->er,
    };

    ast_init(&p.ast, p.alloc);

    node_ref_t root = parse_file(&p);

    *ast = p.ast;
    return root;
}
