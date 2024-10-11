#include "ast.h"

#include <stdbool.h>

#include "alloc/allocator.h"
#include "da/da.h"
#include "slice/slice.h"
#include "tstore.h"

string_t ast_dump_with_types(ast_t const* a, tstore_t* ts, node_ref_t node,
                             allocator_t alloc) {
    assert_not_null(a);

    if (!node_ref_valid(node)) return da_sprintf(alloc, "<invalid>");

    type_ref_t type_ref = ast_get_type(a, node);
    string_t   ty = tstore_type_ref_str(ts, type_ref, alloc);

    node_t const* n = ast_get(a, node);
    switch (n->kind) {
        case NODE_INVAL: return da_sprintf(alloc, "<inval>");
        case NODE_MOD: {
            node_w_children_t const* node = node_as_mod(n);
            slice_node_ref_t         children = ast_get_arr(a, node->children);

            string_t s = da_sprintf(alloc, "(<%s> mod ", ty.items);
            slice_foreach(children, i) {
                string_t c =
                    ast_dump_with_types(a, ts, slice_at(children, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);
            da_free(&ty);
            da_free(&s);
            return ss;
        }
        case NODE_DECL: {
            node_decl_t const* node = node_as_decl(n);
            string_t           type = node_ref_valid(node->type)
                                          ? ast_dump_with_types(a, ts, node->type, alloc)
                                          : (string_t){};
            string_t           init = node_ref_valid(node->init)
                                          ? ast_dump_with_types(a, ts, node->init, alloc)
                                          : (string_t){};

            char const* var_s = decl_is_var(node->flags) ? " var" : "";
            str_t       name = ast_get_str(a, node->extern_name);

            string_t s;
            if (decl_is_extern(node->flags)) {
                if (node->extern_name.len) {
                    str_t ename = ast_get_str(a, node->extern_name);
                    s = da_sprintf(alloc,
                                   "(<%s> decl%s \"%s\" (extern \"%s\") %s %s)",
                                   ty.items, var_s, name.ptr, ename.ptr,
                                   type.items, init.items);
                } else {
                    s = da_sprintf(alloc, "(<%s> decl%s \"%s\" (extern) %s %s)",
                                   ty.items, var_s, name.ptr, type.items,
                                   init.items);
                }
            } else {
                s = da_sprintf(alloc, "(<%s> decl%s \"%s\" %s %s)", ty.items,
                               var_s, name.ptr, type.items, init.items);
            }

            da_free(&ty);
            da_free(&type);
            da_free(&init);
            return s;
        }
        case NODE_PROC: {
            node_proc_t const* node = node_as_proc(n);
            string_t           s = da_sprintf(alloc, "(<%s> proc", ty.items);

            slice_node_ref_t args = ast_get_arr(a, node->args);
            slice_foreach(args, i) {
                string_t c =
                    ast_dump_with_types(a, ts, slice_at(args, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            if (proc_has_vararg(node->flags)) {
                string_t ss = da_sprintf(alloc, "%s ...", s.items);
                da_free(&s);
                s = ss;
            }

            if (node_ref_valid(node->ret)) {
                string_t r = ast_dump_with_types(a, ts, node->ret, alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, r.items);

                da_free(&r);
                da_free(&s);

                s = ss;
            } else {
                string_t ss = da_sprintf(alloc, "%s nil", s.items);
                da_free(&s);
                s = ss;
            }

            string_t b = node_ref_valid(node->body)
                             ? ast_dump_with_types(a, ts, node->body, alloc)
                             : (string_t){
                                   "nil", 3, 3, {NULL, NULL}
            };
            string_t ss = da_sprintf(alloc, "%s %s)", s.items, b.items);

            da_free(&ty);
            da_free(&b);
            da_free(&s);
            return ss;
        }
        case NODE_CALL: {
            node_call_t const* node = node_as_call(n);

            string_t c = ast_dump_with_types(a, ts, node->callee, alloc);
            string_t s = da_sprintf(alloc, "(<%s> call %s", ty.items, c.items);

            slice_node_ref_t args = ast_get_arr(a, node->args);
            slice_foreach(args, i) {
                string_t c =
                    ast_dump_with_types(a, ts, slice_at(args, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);

            da_free(&ty);
            da_free(&s);
            return ss;
        }
        case NODE_BLK: {
            node_w_children_t const* node = node_as_blk(n);
            slice_node_ref_t         children = ast_get_arr(a, node->children);

            string_t s = da_sprintf(alloc, "(<%s> blk", ty.items);
            slice_foreach(children, i) {
                string_t c =
                    ast_dump_with_types(a, ts, slice_at(children, i), alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);

            da_free(&ty);
            da_free(&s);
            return ss;
        }
        case NODE_ARG: {
            node_arg_t const* node = node_as_arg(n);
            str_t             name = ast_get_str(a, node->name);

            string_t s = da_sprintf(alloc, "(<%s> arg %s", ty.items, name.ptr);
            if (node_ref_valid(node->type)) {
                string_t c = ast_dump_with_types(a, ts, node->type, alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);

                s = ss;
            }

            string_t ss = da_sprintf(alloc, "%s)", s.items);

            da_free(&ty);
            da_free(&s);
            return ss;
        }
        case NODE_ADD:  // fallthrough
        case NODE_SUB:  // fallthrough
        case NODE_MUL:  // fallthrough
        case NODE_DIV: {
            node_binary_t const* node = node_as_binary(n);
            string_t lhs = ast_dump_with_types(a, ts, node->left, alloc);
            string_t rhs = ast_dump_with_types(a, ts, node->right, alloc);

            char const* kind;
            switch (n->kind) {
                case NODE_ADD: kind = "add"; break;
                case NODE_SUB: kind = "sub"; break;
                case NODE_MUL: kind = "mul"; break;
                case NODE_DIV: kind = "div"; break;
                default: assert(false);
            }

            string_t s = da_sprintf(alloc, "(<%s> %s %s %s)", ty.items, kind,
                                    lhs.items, rhs.items);

            da_free(&ty);
            da_free(&lhs);
            da_free(&rhs);
            return s;
        }
        case NODE_IF: {
            node_ternary_t const* node = node_as_if(n);

            string_t s = da_sprintf(alloc, "(if <%s>", ty.items);

            string_t c = ast_dump_with_types(a, ts, node->cond, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            c = ast_dump_with_types(a, ts, node->wtrue, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            if (node_ref_valid(node->wfalse)) {
                c = ast_dump_with_types(a, ts, node->wtrue, alloc);
                da_strjoin_and_free(&s, &c, &string_from_lit(" "));
            }

            da_catfmt(&s, ")");

            da_free(&ty);
            return s;
        }
        case NODE_WHILE: {
            node_ternary_t const* node = node_as_while(n);

            string_t s = da_sprintf(alloc, "(while <%s>", ty.items);

            string_t c = ast_dump_with_types(a, ts, node->cond, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            c = ast_dump_with_types(a, ts, node->wtrue, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            da_catfmt(&s, ")");

            da_free(&ty);
            return s;
        }
        case NODE_STMT_EXPR: {
            node_w_child_t const* node = node_as_stmt_expr(n);

            string_t s = da_sprintf(alloc, "(stmt-expr <%s>", ty.items);

            string_t c = ast_dump_with_types(a, ts, node->child, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            da_catfmt(&s, ")");

            da_free(&ty);
            return s;
        }
        case NODE_ASSIGN: {
            node_binary_t const* node = node_as_assign(n);

            string_t s = da_sprintf(alloc, "(assign <%s>", ty.items);

            string_t c = ast_dump_with_types(a, ts, node->left, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            c = ast_dump_with_types(a, ts, node->right, alloc);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));

            da_catfmt(&s, ")");

            da_free(&ty);
            return s;
        }
        case NODE_NEG:
        case NODE_NOT: {
            node_w_child_t const* node = node_as_unary(n);
            string_t c = ast_dump_with_types(a, ts, node->child, alloc);

            char const* kind;
            switch (n->kind) {
                case NODE_NEG: kind = "neg"; break;
                case NODE_NOT: kind = "not"; break;
                default: assert(false);
            }

            string_t s =
                da_sprintf(alloc, "(<%s> %s %s)", ty.items, kind, c.items);

            da_free(&ty);
            da_free(&c);
            return s;
        }
        case NODE_RET: {
            node_w_child_t const* node = node_as_ret(n);
            if (node_ref_valid(node->child)) {
                string_t c = ast_dump_with_types(a, ts, node->child, alloc);
                string_t s =
                    da_sprintf(alloc, "(<%s> return %s)", ty.items, c.items);

                da_free(&ty);
                da_free(&c);
                return s;
            }

            string_t s = da_sprintf(alloc, "(<%s> return)", ty.items);

            da_free(&ty);
            return s;
        }
        case NODE_REF: {
            node_w_child_t const* node = node_as_ref(n);
            string_t c = ast_dump_with_types(a, ts, node->child, alloc);
            string_t s = da_sprintf(alloc, "(<%s> ref %s)", ty.items, c.items);

            da_free(&c);
            da_free(&ty);
            return s;
        }
        case NODE_DEREF: {
            node_w_child_t const* node = node_as_deref(n);
            string_t              c = ast_dump_with_types(a, ts, node->child, alloc);
            string_t              s =
                da_sprintf(alloc, "(<%s> deref %s)", ty.items, c.items);

            da_free(&c);
            da_free(&ty);
            return s;
        }
        case NODE_PTR: {
            node_ptr_t const* node = node_as_ptr(n);

            char const* multi = ptr_is_multi(node->flags) ? " multi" : "";
            char const* const_ = ptr_is_const(node->flags) ? " const" : "";
            char const* slice = ptr_is_slice(node->flags) ? " slice" : "";

            string_t s = da_sprintf(alloc, "(<%s> ptr%s%s%s", ty.items, multi,
                                    slice, const_);
            if (node_ref_valid(node->term)) {
                string_t c = ast_dump_with_types(a, ts, node->term, alloc);
                string_t ss = da_sprintf(alloc, "%s %s", s.items, c.items);

                da_free(&c);
                da_free(&s);
                s = ss;
            }

            string_t c = ast_dump_with_types(a, ts, node->child, alloc);
            string_t ss = da_sprintf(alloc, "%s %s)", s.items, c.items);

            da_free(&ty);
            da_free(&c);
            da_free(&s);
            return ss;
        }
        case NODE_IDENT: {
            node_ident_t const* node = node_as_ident(n);
            str_t               ident = ast_get_str(a, node->ident);
            string_t s = da_sprintf(alloc, "(<%s> %s)", ty.items, ident.ptr);

            da_free(&ty);
            return s;
        }
        case NODE_INT: {
            node_int_t const* node = node_as_int(n);
            string_t s = da_sprintf(alloc, "(<%s> %lu)", ty.items, node->value);

            da_free(&ty);
            return s;
        }
    }

    assert(false);
}
