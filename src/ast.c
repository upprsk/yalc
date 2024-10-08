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
            node_w_children_t* node = node_as_mod(n);
            slice_node_ref_t   children = ast_get_arr(a, node->children);

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
            node_decl_t* node = node_as_decl(n);
            string_t     type = node_ref_valid(node->type)
                                    ? ast_dump(a, node->type, alloc)
                                    : (string_t){
                                      "nil", 3, 3, {NULL, NULL}
            };
            string_t     init = node_ref_valid(node->init)
                                    ? ast_dump(a, node->init, alloc)
                                    : (string_t){
                                      "nil", 3, 3, {NULL, NULL}
            };

            char const* var_s = decl_is_var(node->flags) ? " var" : "";

            string_t s;
            if (decl_is_extern(node->flags)) {
                if (node->extern_name.ptr) {
                    s = da_sprintf(alloc,
                                   "(decl%s \"%s\" (extern \"%s\") %s %s)",
                                   var_s, node->name.ptr, node->extern_name.ptr,
                                   type.items, init.items);
                } else {
                    s = da_sprintf(alloc, "(decl%s \"%s\" (extern) %s %s)",
                                   var_s, node->name.ptr, type.items,
                                   init.items);
                }
            } else {
                s = da_sprintf(alloc, "(decl%s \"%s\" %s %s)", var_s,
                               node->name.ptr, type.items, init.items);
            }

            da_free(&type);
            da_free(&init);
            return s;
        }
        case NODE_PROC: {
            node_proc_t* node = node_as_proc(n);
            string_t     s = da_sprintf(alloc, "(proc");

            slice_node_ref_t args = ast_get_arr(a, node->args);
            slice_foreach(args, i) {
                string_t c = ast_dump(a, slice_at(args, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            if (node_ref_valid(node->ret)) {
                string_t r = ast_dump(a, node->ret, alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, r.items);

                da_free(&r);
                da_free(&s);

                s = ss;
            } else {
                string_t ss = da_sprintf(alloc, "%s nil", s.items);
                da_free(&s);
                s = ss;
            }

            string_t b = ast_dump(a, node->body, alloc);
            string_t ss = da_sprintf(alloc, "%s %s)", s.items, b.items);

            da_free(&b);
            da_free(&s);

            return ss;
        }
        case NODE_CALL: {
            node_call_t* node = node_as_call(n);

            string_t c = ast_dump(a, node->callee, alloc);
            string_t s = da_sprintf(alloc, "(call %s", c.items);

            slice_node_ref_t args = ast_get_arr(a, node->args);
            slice_foreach(args, i) {
                string_t c = ast_dump(a, slice_at(args, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);
            da_free(&s);
            return ss;
        }
        case NODE_BLK: {
            node_w_children_t* node = node_as_blk(n);
            slice_node_ref_t   children = ast_get_arr(a, node->children);

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
        case NODE_ARG: {
            node_arg_t* node = node_as_arg(n);

            string_t s = da_sprintf(alloc, "(arg %s", node->name.ptr);
            if (node_ref_valid(node->type)) {
                string_t c = ast_dump(a, node->type, alloc);
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
        case NODE_SUB:  // fallthrough
        case NODE_MUL:  // fallthrough
        case NODE_DIV: {
            node_binary_t* node = node_as_binary(n);
            string_t       lhs = ast_dump(a, node->left, alloc);
            string_t       rhs = ast_dump(a, node->right, alloc);

            char const* kind;
            switch (n->kind) {
                case NODE_ADD: kind = "add"; break;
                case NODE_SUB: kind = "sub"; break;
                case NODE_MUL: kind = "mul"; break;
                case NODE_DIV: kind = "div"; break;
                default: assert(false);
            }

            string_t s =
                da_sprintf(alloc, "(%s %s %s)", kind, lhs.items, rhs.items);

            da_free(&lhs);
            da_free(&rhs);
            return s;
        }
        case NODE_NEG:
        case NODE_NOT: {
            node_w_child_t* node = node_as_unary(n);
            string_t        c = ast_dump(a, node->child, alloc);

            char const* kind;
            switch (n->kind) {
                case NODE_NEG: kind = "neg"; break;
                case NODE_NOT: kind = "not"; break;
                default: assert(false);
            }

            string_t s = da_sprintf(alloc, "(%s %s)", kind, c.items);

            da_free(&c);
            return s;
        }
        case NODE_RET: {
            node_w_child_t* node = node_as_ret(n);
            if (node_ref_valid(node->child)) {
                string_t c = ast_dump(a, node->child, alloc);
                string_t s = da_sprintf(alloc, "(return %s)", c.items);

                da_free(&c);
                return s;
            }

            return da_sprintf(alloc, "(return)");
        }
        case NODE_IDENT: {
            node_ident_t* node = node_as_ident(n);
            return da_sprintf(alloc, "%s", node->ident.ptr);
        }
        case NODE_INT: {
            node_int_t* node = node_as_int(n);
            return da_sprintf(alloc, "%lu", node->value);
        }
    }

    assert(false);
}
