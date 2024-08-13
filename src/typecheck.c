#include "typecheck.h"

#include <stdint.h>

#include "errors.h"
#include "span.h"
#include "typestore.h"

typedef struct typechecker {
    error_reporter_t* er;
    char const*       filename;
    char const*       source;
    uint32_t          source_len;
} typechecker_t;

static void typechecker_init(typechecker_t* tc) { *tc = (typechecker_t){}; }

static type_id_t typecheck_node(typechecker_t* tc, node_t* node) {
    munit_assert_not_null(node);

    switch (node->type) {
        case NODE_ERR:
            report_error(tc->er, tc->filename, tc->source, node->span,
                         "found error node in typecheck");
            return INVALID_TYPEID;
        case NODE_INT:
        case NODE_FLOAT:
        case NODE_IDENT:
        case NODE_BINOP:
        case NODE_UNOP:
        case NODE_STMT_EXPR:
        case NODE_STMT_RET:
        case NODE_STMT_BLK:
        case NODE_DECL:
        case NODE_ASSIGN:
        case NODE_ARG:
        case NODE_PROC:
        case NODE_PTR:
        case NODE_MPTR: break;
    }

    munit_assert(false);
}

void pass_typecheck(typecheck_params_t const* params) {
    typechecker_t tc = {
        .filename = params->filename,
        .source = params->source,
        .source_len = params->source_len,
        .er = params->er,
    };
    typechecker_init(&tc);

    typecheck_node(&tc, params->ast);
}
