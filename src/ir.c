#include "ir.h"

#include <munit.h>
#include <stdbool.h>

#include "da/da.h"
#include "slice/slice.h"

string_t inst_str(inst_t inst, allocator_t alloc) {
    switch (inst.op) {
        case IR_INVAL: return da_sprintf(alloc, "inval");
        case IR_COPY:
            return da_sprintf(alloc, "$%d = %s<%s> %lu", inst.rd,
                              irop_str(inst.op), irtype_str(inst.type),
                              inst.imm);
        case IR_MOVE:
            return da_sprintf(alloc, "$%d = %s<%s> $%d", inst.rd,
                              irop_str(inst.op), irtype_str(inst.type),
                              inst.rs);
        case IR_ADD:
        case IR_SUB:
        case IR_MUL:
        case IR_DIV:
        case IR_SLT:
        case IR_SLE:
        case IR_SGT:
        case IR_SGE:
        case IR_SEQ:
        case IR_SNE:
            return da_sprintf(alloc, "$%d = %s<%s> $%d, $%d", inst.rd,
                              irop_str(inst.op), irtype_str(inst.type), inst.rs,
                              inst.rt);
        case IR_BZ:
        case IR_BNZ:
            return da_sprintf(alloc, "%s $%d, @%d", irop_str(inst.op), inst.rd,
                              inst.label);
        case IR_B:
            return da_sprintf(alloc, "%s @%d", irop_str(inst.op), inst.label);
        case IR_CALL:
            return da_sprintf(alloc, "$%d = %s<%s> %d[%d], $%d", inst.rd,
                              irop_str(inst.op), irtype_str(inst.type),
                              inst.pid, inst.count, inst.cs);
        case IR_RET: return da_sprintf(alloc, "ret $%d", inst.rd);
    }

    assert(false);
}

string_t proc_dump(proc_t const* p, allocator_t alloc) {
    assert_not_null(p);

    string_t s =
        da_sprintf(alloc, "proc %d \"%s\":", p->ref.id, p->link_name.ptr);

    if (p->cprocs.len) {
        da_strcat(&s, &string_from_lit("\ncprocs: ["));

        slice_foreach(p->cprocs, i) {
            string_t c =
                da_sprintf(alloc, "%zu: %d", i, slice_at(p->cprocs, i).id);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));
        }

        da_strcat(&s, &string_from_lit(" ]"));
    }

    if (p->labels.len) {
        da_strcat(&s, &string_from_lit("\nlabels: ["));

        slice_foreach(p->labels, i) {
            string_t c =
                da_sprintf(alloc, "%zu: %d", i, slice_at(p->labels, i).off);
            da_strjoin_and_free(&s, &c, &string_from_lit(" "));
        }

        da_strcat(&s, &string_from_lit(" ]"));
    }

    slice_foreach(p->insts, i) {
        string_t c = inst_str(slice_at(p->insts, i), alloc);

#if 1  // start expensive label
        slice_foreach(p->labels, j) {
            if (i == slice_at(p->labels, j).off) da_catfmt(&s, "\n@%zu", j);
        }
#endif  // end expensive label

        da_catfmt(&s, "\n[%04zu] %s", i, c.items);
        da_free(&c);
    }

    return s;
}
