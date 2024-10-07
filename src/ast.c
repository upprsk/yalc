#include "ast.h"

#include <stdbool.h>

#include "alloc/allocator.h"
#include "da/da.h"
#include "slice/slice.h"

string_t ast_dump(ast_t const* a, node_ref_t node, allocator_t alloc) {
    assert_not_null(a);

    if (!node_ref_valid(node)) return da_sprintf(alloc, "<invalid>");

    node_t* n = ast_get(a, node);
    switch (n->kind) {
        case NODE_INVAL: return da_sprintf(alloc, "<inval>");
        case NODE_MOD: {
            slice_node_ref_t children =
                ast_get_arr(a, n->as.w_children.children);

            string_t s = da_sprintf(alloc, "(mod");
            slice_foreach(children, i) {
                string_t c = ast_dump(a, slice_at(children, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);
            da_free(&s);
            return ss;
        }
        case NODE_DECL: {
            string_t type = node_ref_valid(n->as.decl.type)
                                ? ast_dump(a, n->as.decl.type, alloc)
                                : (string_t){
                                      "nil", 3, 3, {NULL, NULL}
            };
            string_t init = node_ref_valid(n->as.decl.init)
                                ? ast_dump(a, n->as.decl.init, alloc)
                                : (string_t){
                                      "nil", 3, 3, {NULL, NULL}
            };

            string_t s =
                da_sprintf(alloc, "(decl \"%s\" %s %s)", n->as.decl.name.ptr,
                           type.items, init.items);

            da_free(&type);
            da_free(&init);
            return s;
        }
        case NODE_PROC: {
            string_t s = da_sprintf(alloc, "(proc");

            slice_node_ref_t args = ast_get_arr(a, n->as.proc.args);
            slice_foreach(args, i) {
                string_t c = ast_dump(a, slice_at(args, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            if (node_ref_valid(n->as.proc.ret)) {
                string_t r = ast_dump(a, n->as.proc.ret, alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, r.items);

                da_free(&r);
                da_free(&s);

                s = ss;
            } else {
                string_t ss = da_sprintf(alloc, "%s nil", s.items);
                da_free(&s);
                s = ss;
            }

            string_t b = ast_dump(a, n->as.proc.body, alloc);
            string_t ss = da_sprintf(alloc, "%s %s)", s.items, b.items);

            da_free(&b);
            da_free(&s);

            return ss;
        }
        case NODE_BLK: {
            slice_node_ref_t children =
                ast_get_arr(a, n->as.w_children.children);

            string_t s = da_sprintf(alloc, "(blk");
            slice_foreach(children, i) {
                string_t c = ast_dump(a, slice_at(children, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);
            da_free(&s);
            return ss;
        }
        case NODE_ADD:  // fallthrough
        case NODE_SUB: {
            string_t lhs = ast_dump(a, n->as.binary.left, alloc);
            string_t rhs = ast_dump(a, n->as.binary.right, alloc);

            char const* kind;
            switch (n->kind) {
                case NODE_ADD: kind = "add"; break;
                case NODE_SUB: kind = "sub"; break;
                default: assert(false);
            }

            string_t s =
                da_sprintf(alloc, "(%s %s %s)", kind, lhs.items, rhs.items);

            da_free(&lhs);
            da_free(&rhs);
            return s;
        }
        case NODE_RET: {
            string_t c = ast_dump(a, n->as.w_child.child, alloc);
            string_t s = da_sprintf(alloc, "(return %s)", c.items);

            da_free(&c);
            return s;
        }
        case NODE_IDENT: return da_sprintf(alloc, "%s", n->as.ident.ident.ptr);
        case NODE_INT: return da_sprintf(alloc, "%lu", n->as.int_.value);
    }

    assert(false);
}
