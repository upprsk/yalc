#include "ast.h"

#include "alloc/allocator.h"
#include "da/da.h"
#include "slice/slice.h"

string_t ast_dump(ast_t const* a, node_ref_t node, allocator_t alloc) {
    assert_not_null(a);

    node_t* n = ast_get(a, node);
    switch (n->kind) {
        case NODE_INVAL: return da_sprintf(alloc, "<invalid>");
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
                                : (string_t){};
            string_t init = node_ref_valid(n->as.decl.init)
                                ? ast_dump(a, n->as.decl.init, alloc)
                                : (string_t){};

            string_t s =
                da_sprintf(alloc, "(decl \"%s\" %s %s)", n->as.decl.name.ptr,
                           type.items, init.items);

            da_free(&type);
            da_free(&init);
            return s;
        }
        case NODE_PROC: return da_sprintf(alloc, "<invalid>");
        case NODE_BLK: return da_sprintf(alloc, "<invalid>");
        case NODE_RET: {
            string_t c = ast_dump(a, n->as.w_child.child, alloc);
            string_t s = da_sprintf(alloc, "(return %s)", c.items);

            da_free(&c);
            return s;
        }
        case NODE_IDENT: return da_sprintf(alloc, "%s", n->as.ident.ident.ptr);
        case NODE_INT: return da_sprintf(alloc, "%lu", n->as.int_.value);
    }
}
