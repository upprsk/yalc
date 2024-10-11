#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "alloc/allocator.h"
#include "da/da.h"
#include "slice/slice.h"

// Represent a type in the IR.
typedef enum irtype : uint8_t {
    IRT_INVAL,

    IRT_VOID,  // represents the absence of a value. This is used just for
               // functions that return void/nothing.

    IRT_I8,   // signed 8bit integer.
    IRT_U8,   // unsigned 8bit integer.
    IRT_I16,  // signed 16bit integer.
    IRT_U16,  // unsigned 16bit integer.
    IRT_I32,  // signed 32bit integer.
    IRT_U32,  // unsigned 32bit integer.
    IRT_I64,  // signed 64bit integer.
    IRT_U64,  // unsigned 64bit integer.
} irtype_t;

static inline char const* irtype_str(irtype_t ty) {
    switch (ty) {
        case IRT_INVAL: return "IRT_INVAL";
        case IRT_VOID: return "void";
        case IRT_I8: return "i8";
        case IRT_U8: return "u8";
        case IRT_I16: return "i16";
        case IRT_U16: return "u16";
        case IRT_I32: return "i32";
        case IRT_U32: return "u32";
        case IRT_I64: return "i64";
        case IRT_U64: return "u64";
    }

    return "?";
}

typedef enum irop : uint8_t {
    IR_INVAL,

    // used for loading constants, copies the given data as is. The receiving
    // register will have type T.
    //
    //  rd = copy<T> imm
    IR_COPY,

    // used to copy from one register to another. The destination type is the
    // same as the input type. The stored type is used for sanity checks.
    //
    //  rd = move<T> rs
    IR_MOVE,

    // add the values of two registers. Both the argments must have the same
    // type T and the return will also have type T.
    //
    //  rd = add<T> rs, rt
    IR_ADD,

    // subtract the values of two registers. Both the argments must have the
    // same type T and the return will also have type T.
    //
    //  rd = sub<T> rs, rt
    IR_SUB,

    // multiply the values of two registers. Both the argments must have the
    // same type T and the return will also have type T.
    //
    //  rd = mul<T> rs, rt
    IR_MUL,

    // divide the values of two registers. Both the argments must have the
    // same type T and the return will also have type T.
    //
    //  rd = mul<T> rs, rt
    IR_DIV,

    // compare if `rs < rt`, `rs <= rt`, `rs > rt`, `rs >= rt`, `rs == rt` or
    // `rs != rt` and store the result (0 or 1) in `rd`. The registers need to
    // have the same type.
    //
    //  rd = slt<T> rs, rt
    //  rd = sle<T> rs, rt
    //  rd = sgt<T> rs, rt
    //  rd = sgte<T> rs, rt
    //  rd = seq<T> rs, rt
    //  rd = sne<T> rs, rt
    //
    // TODO: What to do about the type?
    IR_SLT,
    IR_SLE,
    IR_SGT,
    IR_SGE,
    IR_SEQ,
    IR_SNE,

    // break/jump to the specified label if the given register is (or not) equal
    // to zero. Labels are just indexes into an offset array.
    //
    //  bz rd, @label
    //  bnz rd, @label
    IR_BZ,
    IR_BNZ,

    // break/jump to the specified label unconditionally.
    //
    //  b @label
    IR_B,

    // load or store bytes from the location pointed by the given register. The
    // type determines if the value is zero- or sign-extended. A constant offset
    // (in bytes) from the pointer can be given in `off`.
    //
    //  rd = load<T> rs, off
    //  stor<T> rd, rs, off
    //
    // rd is the address int the stack, rs is the value to load/store.
    IR_LOAD,
    IR_STOR,

    // allocate space in the stack for `imm` items with the given size. this
    // instruction returns a pointer to the allocated memory.
    //
    //  rd = alloca<T> imm
    IR_ALLOCA,

    /// call another procedure `pid`. The return value will be interpreted
    /// as T (should match the return type of the called procedure). To keep the
    /// IR simple, there is no way to represent a variable number of arguments,
    /// as needed for a call. Because of this, the instruction has the number of
    /// arguments and a base register. The arguments will be in the `count`
    /// registers starting at `cs`. The return value is placed in the `rd`
    /// register.
    ///
    /// `pid` is an index into a procedure call-list. Each procedure stores
    /// all other procedures it calls (which is 2^16-1 max).
    //
    //  rd = call<T> pid[count] $cs
    IR_CALL,

    /// return from a procedure. Procedures that don't return anything do not
    /// need to call ret, as falling of the end of the procedure automatically
    /// returns.
    ///
    ///  ret rd
    IR_RET,
} irop_t;

static inline char const* irop_str(irop_t op) {
    switch (op) {
        case IR_INVAL: return "IR_INVAL";
        case IR_COPY: return "copy";
        case IR_MOVE: return "move";
        case IR_ADD: return "add";
        case IR_SUB: return "sub";
        case IR_MUL: return "mul";
        case IR_DIV: return "div";
        case IR_SLT: return "slt";
        case IR_SLE: return "sle";
        case IR_SGT: return "sgt";
        case IR_SGE: return "sge";
        case IR_SEQ: return "seq";
        case IR_SNE: return "sne";
        case IR_BZ: return "bz";
        case IR_BNZ: return "bnz";
        case IR_B: return "b";
        case IR_LOAD: return "load";
        case IR_STOR: return "stor";
        case IR_ALLOCA: return "alloca";
        case IR_CALL: return "call";
        case IR_RET: return "ret";
    }

    return "?";
}

/// An instuction is 16 bytes long and uses different fields of the union
/// depending on the instuction op. All instuctions store a type, it is used to
/// know the size of the arguments and the resulting type of the operation. It
/// is also used to perform some rudimentary checks on the used types (like
/// mixing signed and unsigned).
///
/// imm and imm_bytes may be used by the instructions that take immediates
/// depending on the type.
typedef struct inst {
    irop_t   op;
    irtype_t type;
    uint32_t rd;
    union {
        uint64_t imm;
        uint8_t  imm_bytes[8];
        struct {
            uint32_t rs;
            union {
                uint32_t rt;
                uint32_t off;
            };
        };
        struct {
            uint32_t label;
        };
        struct {
            uint32_t cs;
            uint16_t pid;
            uint16_t count;
        };
    };
} inst_t;

/// convert an instruction to a nice readable format.
string_t inst_str(inst_t inst, allocator_t alloc);

typedef da_t(inst_t) da_inst_t;
typedef slice_t(inst_t) slice_inst_t;
typedef da_t(irtype_t) da_irtype_t;
typedef slice_t(irtype_t) slice_irtype_t;

/// A reference to a procedure in the compilation unit.
typedef struct proc_ref {
    uint32_t id;
} proc_ref_t;

typedef da_t(proc_ref_t) da_proc_ref_t;
typedef slice_t(proc_ref_t) slice_proc_ref_t;

#define proc_ref_valid(_ref)    (!!(_ref).id)
#define proc_ref_eq(_lhs, _rhs) ((_lhs).id == (_rhs).id)

/// A reference to a procedure inside of a procedure.
typedef struct lproc_ref {
    uint16_t id;
} lproc_ref_t;

typedef da_t(lproc_ref_t) da_lproc_ref_t;
typedef slice_t(lproc_ref_t) slice_lproc_ref_t;

typedef struct label_ref {
    uint16_t id;
} label_ref_t;

typedef da_t(label_ref_t) da_label_ref_t;
typedef slice_t(label_ref_t) slice_label_ref_t;

/// A label stores the offset from the start of the procedure to the target
/// location.
typedef struct label {
    uint32_t off;
} label_t;

typedef da_t(label_t) da_label_t;
typedef slice_t(label_t) slice_label_t;

/// Flags for a procedure
typedef enum proc_flags {
    PFLAG_EXTERN = 1 << 0,
} proc_flags_t;

static inline bool proc_is_extern(proc_flags_t flags) {
    return flags & PFLAG_EXTERN;
}

/// Represent a procedure, fully converted to the ir. It contains a list of
/// instructions, the types of it's arguments (IR types, not language types),
/// all the other procedures it calls, stack allocation information and some
/// debugging information.
///
/// TODO: add stack usage info
typedef struct proc {
    slice_irtype_t args;
    irtype_t       ret;

    proc_flags_t flags;

    slice_proc_ref_t cprocs;     // called procs
    slice_label_t    labels;     // used labels
    str_t            link_name;  // name to use for the procedure
    proc_ref_t       ref;        // the global id of this proc

    slice_inst_t insts;
} proc_t;

typedef da_t(proc_t) da_proc_t;
typedef slice_t(proc_t) slice_proc_t;

/// Represent the collection of all procedures and globals in a compilation
/// unit.
///
/// TODO: add globals, the static memory allocator and comptime memory
/// allocator.
typedef struct obj {
    slice_proc_t procs;
} obj_t;

string_t proc_dump(proc_t const* p, allocator_t alloc);

// ----------------------------------------------------------------------------

static inline inst_t inst_copy(irtype_t t, uint32_t rd, uint64_t imm) {
    return (inst_t){.op = IR_COPY, .type = t, .rd = rd, .imm = imm};
}

static inline inst_t inst_move(irtype_t t, uint32_t rd, uint32_t rs) {
    return (inst_t){.op = IR_MOVE, .type = t, .rd = rd, .rs = rs};
}

static inline inst_t inst_add(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_ADD, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_sub(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SUB, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_mul(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_MUL, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_div(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_DIV, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_slt(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SLT, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_sle(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SLE, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_sgt(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SGT, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_sge(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SGE, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_seq(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SEQ, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_sne(irtype_t t, uint32_t rd, uint32_t rs,
                              uint32_t rt) {
    return (inst_t){.op = IR_SNE, .type = t, .rd = rd, .rs = rs, .rt = rt};
}

static inline inst_t inst_bz(irtype_t t, uint32_t rd, uint32_t label) {
    return (inst_t){.op = IR_BZ, .type = t, .rd = rd, .label = label};
}

static inline inst_t inst_bnz(irtype_t t, uint32_t rd, uint32_t label) {
    return (inst_t){.op = IR_BNZ, .type = t, .rd = rd, .label = label};
}

static inline inst_t inst_b(irtype_t t, uint32_t label) {
    return (inst_t){.op = IR_B, .type = t, .label = label};
}

static inline inst_t inst_load(irtype_t t, uint32_t rd, uint32_t rs,
                               uint32_t off) {
    return (inst_t){.op = IR_LOAD, .type = t, .rd = rd, .rs = rs, .off = off};
}

static inline inst_t inst_stor(irtype_t t, uint32_t rd, uint32_t rs,
                               uint32_t off) {
    return (inst_t){.op = IR_STOR, .type = t, .rd = rd, .rs = rs, .off = off};
}

static inline inst_t inst_alloca(irtype_t t, uint32_t rd, uint64_t imm) {
    return (inst_t){.op = IR_ALLOCA, .type = t, .rd = rd, .imm = imm};
}

static inline inst_t inst_call(irtype_t t, uint32_t rd, uint16_t pid,
                               uint16_t count, uint32_t cs) {
    return (inst_t){.op = IR_CALL,
                    .type = t,
                    .rd = rd,
                    .pid = pid,
                    .count = count,
                    .cs = cs};
}

static inline inst_t inst_ret(irtype_t t, uint32_t rd) {
    return (inst_t){.op = IR_RET, .type = t, .rd = rd};
}
