/*! \file build-literals-v3.c
 *
 * This file implements a simple bytecode interpreter that implements a language
 * for initializing a record of compile-time constant values.
 *
 * This code needs to agree with the code generator in base/CPS/main/literals.sml
 *
 * See https://github.com/smlnj/.github/wiki/Literals-v3 for a description of
 * the bytecode.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "heap.h"
#include <string.h>
#include <inttypes.h>

/* printf formats for Int_t/Word_t types */
#  define PRINT         PRId64
#  define PRWORD        PRIu64

#define V1_MAGIC        0x19981022
#define V2_MAGIC        0x20190921
#define V3_MAGIC        0x20260912

/* the amount of free space that we want in the allocation arena; this value must be
 * less than MIN_ALLOC_SZB (defined in include/ml-limits.h)
 */
#define FREE_REQ_SZB    64*ONE_K

/* for backward compatibility */
ml_val_t BuildLiteralsV1 (ml_state_t *msp, Byte_t *lits, int pc, int len);
ml_val_t BuildLiteralsV2 (ml_state_t *msp, Byte_t *code, int len, int maxDepth, int pc);

#define VK_TAGINT       0
#define VK_RAWINT       1
#define VK_REAL32       2
#define VK_REAL64       3
#define VK_OBJ          4

typedef struct {                /* items on the stack */
    union {
        ml_val_t ml;
        Unsigned64_t i;
        float f;
        double d;
    } val;
    Byte_t kind;
} StkItem_t;

/****** The Interpreter State *****/
typedef struct {
    ml_state_t *msp;    /*<! the ML state pointer */
    Byte_t *code;       /*!< the bytecode being interpreted */
    StkItem_t *stk;     /*!< the stack */
    ml_val_t *saved;    /*!< the scratch memory for saving values */
    int codeSz;         /*!< the size of the code */
    int pc;             /*!< the current program counter */
    int tos;            /*!< the top of the stack; -1 means an empty stack */
    int maxDepth;       /*!< the maximum depth of the stack (i.e., tos < maxDepth) */
    int maxSaved;       /*!< the size of the scratch memory */
} State_t;

/* invoke the garbage collector while preserving
 * the heap-allocated parts of the interpreter state.
 */
PVT void GC (State_t *stp, int gcLevel)
{
    ml_val_t rootObj;

    /* construct an object containing the roots that are in the state */
    {
        /* the roots cover the size of the save area plus the current stack depth */
        int nRoots = stp->maxSaved + stp->tos + 1;

        ML_AllocWrite(stp->msp, 0, MAKE_DESC(nRoots, DTAG_record));
        /* first we copy the values from the saved array */
        for (int i = 0;  i < stp->maxSaved;  ++i) {
            ML_AllocWrite(stp->msp, i+1, stp->saved[i]);
        }
        /* copy the stack roots starting from the bottom */
        for (int i = 0;  i <= stp->tos;  ++i) {
            if (stp->stk[i].kind == VK_OBJ) {
                ML_AllocWrite(stp->msp, stp->maxSaved+1+i, stp->stk[i].val.ml);
            } else {
                /* we store unit for non-object stack slots */
                ML_AllocWrite(stp->msp, stp->maxSaved+1+i, ML_unit);
            }
        }
        rootObj = ML_Alloc(stp->msp, nRoots);
    }

    /* invoke the collector */
    InvokeGCWithRoots (
        stp->msp, gcLevel, (ml_val_t *)&stp->code, &rootObj, NIL(ml_val_t *));

    /* restore the roots */
    {
        ml_val_t *p = (ml_val_t *)rootObj;

        /* restore the saved array */
        for (int i = 0;  i < stp->maxSaved;  ++i, p++) {
            stp->saved[i] = *p;
        }

        /* restore the objects in the stack. */
        for (int i = 0;  i <= stp->tos;  ++i, p++) {
            if (stp->stk[i].kind == VK_OBJ) {
                stp->stk[i].val.ml = *p;
            }
        }
    }

} /* GC */

/* conditionally invoke the garbage collector while preserving
 * the heap-allocated parts of the interpreter state.
 */
STATIC_INLINE void CheckGC (State_t *stp, Addr_t reqSpace)
{
    ml_val_t rootObj;

    Addr_t availSpace = ((Addr_t)stp->msp->ml_limitPtr - (Addr_t)stp->msp->ml_allocPtr);
    if (reqSpace > availSpace) {
#ifdef DEBUG_LITERALS
        SayDebug("BuildLiterals: invoke GC; avail = %" PRIu64 ", req = %" PRIu64 "\n",
            availSpace, reqSpace);
#endif
        GC(stp, 0);
    }
}

/* copy bytes from the instruction stream in correct byte order (the
 * instruction stream is in bigendian order)
 */
STATIC_INLINE void GetBytes (Byte_t *dst, State_t *stp, int n)
{
    int pc = stp->pc;
    for (int i = 0;  i < n;  i++) {
#ifdef BYTE_ORDER_LITTLE
        dst[n-1-i] = stp->code[pc++];
#else
        dst[i] = stp->code[pc++];
#endif
    }
    stp->pc = pc;
}

/* inline functions for fetching arguments */
STATIC_INLINE signed char GetI8Arg (State_t *stp)
{
    int pc = stp->pc;
    signed char i = (signed char)stp->code[pc++];
    stp->pc = pc;
    return i;
}
STATIC_INLINE unsigned char GetU8Arg (State_t *stp)
{
    int pc = stp->pc;
    unsigned char i = stp->code[pc++];
    stp->pc = pc;
    return i;
}
STATIC_INLINE Int16_t GetI16Arg (State_t *stp)
{
    union { Byte_t b[sizeof(Int16_t)]; Int16_t i; } arg;
    GetBytes(arg.b, stp, sizeof(Int16_t));
    return arg.i;
}
STATIC_INLINE Unsigned16_t GetU16Arg (State_t *stp)
{
    union { Byte_t b[sizeof(Unsigned16_t)]; Unsigned16_t u; } arg;
    GetBytes(arg.b, stp, sizeof(Unsigned16_t));
    return arg.u;
}
STATIC_INLINE Int32_t GetI32Arg (State_t *stp)
{
    union { Byte_t b[sizeof(Int32_t)]; Int32_t i; } arg;
    GetBytes(arg.b, stp, sizeof(Int32_t));
    return arg.i;
}
STATIC_INLINE Unsigned32_t GetU32Arg (State_t *stp)
{
    union { Byte_t b[sizeof(Unsigned32_t)]; Unsigned32_t u; } arg;
    GetBytes(arg.b, stp, sizeof(Unsigned32_t));
    return arg.u;
}
STATIC_INLINE Int64_t GetI64Arg (State_t *stp)
{
    union { Byte_t b[sizeof(Int64_t)]; Int64_t i; } arg;
    GetBytes(arg.b, stp, sizeof(Int64_t));
    return arg.i;
}
STATIC_INLINE Unsigned64_t GetU64Arg (State_t *stp)
{
    union { Byte_t b[sizeof(Unsigned64_t)]; Unsigned64_t u; } arg;
    GetBytes(arg.b, stp, sizeof(Unsigned64_t));
    return arg.u;
}
STATIC_INLINE float GetR32Arg (State_t *stp)
{
    union { Byte_t b[sizeof(float)]; float r; } arg;
    GetBytes(arg.b, stp, sizeof(float));
    return arg.r;
}
STATIC_INLINE double GetR64Arg (State_t *stp)
{
    union { Byte_t b[sizeof(double)]; double r; } arg;
    GetBytes(arg.b, stp, sizeof(double));
    return arg.r;
}

STATIC_INLINE void PushTaggedInt (State_t *stp, Int_t n)
{
    int tos = ++stp->tos;
    ASSERT((0 <= tos) && (tos < stp->maxDepth));
    stp->stk[tos].kind = VK_TAGINT;
    stp->stk[tos].val.ml = INT_CtoML(n);
}
STATIC_INLINE void PushRawInt64 (State_t *stp, Int64_t n)
{
    int tos = ++stp->tos;
    ASSERT((0 <= tos) && (tos < stp->maxDepth));
    stp->stk[tos].kind = VK_RAWINT;
    stp->stk[tos].val.i = (Unsigned64_t)n;
}
STATIC_INLINE void PushReal32 (State_t *stp, float f)
{
    int tos = ++stp->tos;
    ASSERT((0 <= tos) && (tos < stp->maxDepth));
    stp->stk[tos].kind = VK_REAL32;
    stp->stk[tos].val.d = f;
}
STATIC_INLINE void PushReal64 (State_t *stp, double d)
{
    int tos = ++stp->tos;
    ASSERT((0 <= tos) && (tos < stp->maxDepth));
    stp->stk[tos].kind = VK_REAL64;
    stp->stk[tos].val.d = d;
}
STATIC_INLINE void PushMLValue (State_t *stp, ml_val_t v)
{
    int tos = ++stp->tos;
    ASSERT((0 <= tos) && (tos < stp->maxDepth));
    stp->stk[tos].kind = VK_OBJ;
    stp->stk[tos].val.ml = v;
}

/* create a record and push it on the stack */
STATIC_INLINE void PushRecord (State_t *stp, int sz)
{
    ASSERT (sz <= stp->tos + 1);

    CheckGC (stp, WORD_SZB * (sz + 1));

    ML_AllocWrite(stp->msp, 0, MAKE_DESC(sz, DTAG_record));
    int top = stp->tos - sz;
    for (int i = 1;  i <= sz;  ++i) {
        int j = top + i;
        ASSERT ((0 <= j) && (j <= stp->tos));
        ASSERT ((stp->stk[j].kind == VK_TAGINT) || (stp->stk[j].kind == VK_OBJ));
        ML_AllocWrite(stp->msp, i, stp->stk[j].val.ml);
    }
    stp->tos -= sz;
    PushMLValue(stp, ML_Alloc(stp->msp, sz));

} /* PushRecord */

/* create a raw record and push it on the stack */
STATIC_INLINE void PushRawRecord (State_t *stp, int sz)
{
    ASSERT (sz <= stp->tos + 1);

    CheckGC (stp, WORD_SZB * (sz + 1));

    ML_AllocWrite(stp->msp, 0, MAKE_DESC(sz, DTAG_raw));
    int top = stp->tos - sz;
    for (int i = 1;  i <= sz;  ++i) {
        int j = top + i;
        ASSERT ((0 <= j) && (j <= stp->tos));
        ASSERT (stp->stk[j].kind != VK_OBJ);
        ML_AllocWrite(stp->msp, i, stp->stk[j].val.ml);
    }
    stp->tos -= sz;
    PushMLValue(stp, ML_Alloc(stp->msp, sz));

} /* PushRawRecord */

/* create a mixed record and push it on the stack */
STATIC_INLINE void PushMixedRecord (State_t *stp, int ptrLen, int rawLen)
{
    int sz = ptrLen + rawLen;

    ASSERT (sz <= stp->tos + 1);

    CheckGC (stp, WORD_SZB * (sz + 1));

    ML_AllocWrite(stp->msp, 0, MAKE_MIXED_DESC(ptrLen, rawLen));
    int top = stp->tos - sz;
    int i = 1;
    /* initialize the pointer fields */
    for (;  i <= ptrLen;  ++i) {
        int j = top + i;
        ASSERT ((0 <= j) && (j <= stp->tos));
        ASSERT ((stp->stk[j].kind == VK_TAGINT) || (stp->stk[j].kind == VK_OBJ));
        ML_AllocWrite(stp->msp, i, stp->stk[j].val.ml);
    }
    /* initialize the raw fields */
    for (;  i <= sz;  ++i) {
        int j = top + i;
        ASSERT ((0 <= j) && (j <= stp->tos));
        ASSERT (stp->stk[j].kind != VK_OBJ);
        ML_AllocWrite(stp->msp, i, stp->stk[j].val.ml);
    }
    stp->tos = top;
    PushMLValue(stp, ML_Alloc(stp->msp, sz));

} /* PushMixedRecord */

/* create a string literal and push it on the stack */
STATIC_INLINE void PushString (State_t *stp, int len)
{
    ml_val_t res;

    ASSERT (stp->pc + len < stp->codeSz);

    int szw = BYTES_TO_WORDS(len+1);  /* include space for '\0' */

    if (szw > SMALL_OBJ_SZW) {
/* TODO */
        Die ("STRING(%d) unimplemented", len);
    } else {
        CheckGC (stp, WORD_SZB * (szw + 1 + 3));
        /* allocate and initialize the data object in the nursery */
        ML_AllocWrite(stp->msp, 0, MAKE_DESC(szw, DTAG_raw));
        ML_AllocWrite (stp->msp, szw, 0);  /* so word-by-word string equality works */
        ml_val_t data = ML_Alloc (stp->msp, szw);
        memcpy (PTR_MLtoC(void, data), stp->code + stp->pc, len);
        stp->pc += len;
        /* allocate the header object */
        SEQHDR_ALLOC(stp->msp, res, DESC_string, data, len);
    }
    PushMLValue(stp, res);

} /* PushString */

STATIC_INLINE void PushVector (State_t *stp, int len)
{
    ml_val_t res;

    ASSERT (len <= stp->tos + 1);
    ASSERT (0 < len);

    if (len > SMALL_OBJ_SZW) {
      /* Since we want to avoid pointers from the 1st generation record space
       * into the allocation space, we need to do a GC before creating the vector.
       */
        arena_t *ap = stp->msp->ml_heap->gen[0]->arena[RECORD_INDX];
        Die ("VEC(%d) unimplemented", len);
/* TODO */
    } else {
        CheckGC (stp, WORD_SZB * (len + 1 + 3));
        /* allocate and initialize the data object */
        ML_AllocWrite(stp->msp, 0, MAKE_DESC(len, DTAG_vec_data));
        int top = stp->tos - len;
        for (int i = 1;  i <= len;  ++i) {
            int j = top + i;
            ASSERT ((0 <= j) && (j <= stp->tos));
            ASSERT ((stp->stk[j].kind == VK_TAGINT) || (stp->stk[j].kind == VK_OBJ));
            ML_AllocWrite(stp->msp, i, stp->stk[j].val.ml);
        }
        stp->tos -= len;
        ml_val_t data = ML_Alloc(stp->msp, len);
        /* allocate the header */
        SEQHDR_ALLOC(stp->msp, res, DESC_polyvec, data, len);
    }
    PushMLValue(stp, res);

} /* PushVector */


/* BuildLiterals:
 *
 * NOTE: we allocate all of the objects in the first generation, and allocate
 * the vector of literals in the allocation space.
 */
ml_val_t BuildLiterals (ml_state_t *msp, Byte_t *code, int len)
{
#ifdef DEBUG_LITERALS
    int depth = 0;
#endif
    State_t state;

    /* the V2 generator produces a one-byte code object when there are no literals */
    if (len < 4*sizeof(Unsigned32_t) + 1) {
        return ML_unit;
    }

    state.msp = msp;
    state.code = code;
    state.codeSz = len;
    state.pc = 0;

#ifdef DEBUG_LITERALS
    SayDebug("# BuildLiterals: code = %p, len = %d\n", (void *)code, len);
#endif

    /* the V2 and V3 headers consist of four 32-bit words:
     *
     *    struct literal_header {
     *        uint32_t    magic;
     *        uint32_t    maxDepth;
     *        uint32_t    wordSz;
     *        uint32_t    maxSaved;
     *    };
     */
    Unsigned32_t magic = GetU32Arg(&state);
    int maxDepth = (int)GetU32Arg(&state);

    if (magic == V1_MAGIC) {
#ifdef DEBUG_LITERALS
        SayDebug("# BuildLiterals: VERSION 1\n");
#endif
        return BuildLiteralsV1 (msp, code, state.pc, len);
    }
    else if (magic == V2_MAGIC) {
#ifdef DEBUG_LITERALS
        SayDebug("# BuildLiterals: VERSION 2\n");
#endif
        return BuildLiteralsV2 (msp, code, len, maxDepth, state.pc);
    }
    else if (magic != V3_MAGIC) {
        Die("bogus literal magic number %#x", magic);
    }
#ifdef DEBUG_LITERALS
    SayDebug("# BuildLiterals: VERSION 3\n");
#endif

  /* get the rest of the header */
    Unsigned32_t wordSz = GetU32Arg(&state);
    if (wordSz != 64) {
        Die("expected word size = 64, but found %d\n", wordSz);
    }

  /* We represent the saved array as a C array of ML values.  When we do a GC, we
   * copy these into a heap-allocated root record.
   */
    int maxSaved = (int)GetU32Arg(&state);
    if (maxSaved > 0) {
        state.saved = NEW_VEC(ml_val_t, maxSaved);
        for (int i = 0;  i < maxSaved;  ++i) {
            state.saved[i] = ML_unit;
        }
    }
    else {
        state.saved = NIL(ml_val_t *);
    }

    /* allocate space for the stack */
    ASSERT(maxDepth > 0);
    state.stk = NEW_VEC(StkItem_t, maxDepth);

    /* initialize the rest of the state */
    state.tos = -1;
    state.maxDepth = maxDepth;
    state.maxSaved = maxSaved;
    ASSERT (state.pc == 4*sizeof(Unsigned32_t));

#ifdef DEBUG_LITERALS
    SayDebug("# BuildLiterals: avail = %d bytes; maxDepth = %d, maxSaved = %d\n",
        (int)((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr),
        maxDepth, maxSaved);
#endif
    while (TRUE) {
        ASSERT(state.pc < len);

/* top of stack pointer */
#define TOP     (state.stk+state.tos)

        /* get the next instruction */
        Byte_t opcode = state.code[state.pc++];

#ifdef DEBUG_LITERALS
        SayDebug("## pc = %d, opcode = %02x, tos = %d\n",
            state.pc, (int)opcode, state.tos);
#endif
        ASSERT(state.tos < maxDepth);
        /* handle the operation */
        switch (opcode) {
          case 0x00:
          case 0x01:
          case 0x02:
          case 0x03:
          case 0x04:
          case 0x05:
          case 0x06:
          case 0x07:
          case 0x08:
          case 0x09:
          case 0x0A:
          case 0x0B:
          case 0x0C:
          case 0x0D:
          case 0x0E:
          case 0x0F:
          case 0x10:
          case 0x11:
          case 0x12:
          case 0x13:
          case 0x14:
          case 0x15:
          case 0x16:
          case 0x17:
          case 0x18:
          case 0x19:
          case 0x1A:
          case 0x1B:
          case 0x1C:
          case 0x1D:
          case 0x1E:
          case 0x1F:
            /* push opcode as a tagged int */
            PushTaggedInt (&state, (int)opcode);
            break;
          case 0x20:
          case 0x21:
          case 0x22:
          case 0x23:
          case 0x24:
          case 0x25:
          case 0x26:
          case 0x27:
          case 0x28:
          case 0x29:
          case 0x2A:
          case 0x2B:
          case 0x2C:
          case 0x2D:
          case 0x2E:
          case 0x2F:
          case 0x30:
          case 0x31:
          case 0x32:
          case 0x33:
          case 0x34:
          case 0x35:
          case 0x36:
          case 0x37:
          case 0x38:
          case 0x39:
          case 0x3A:
          case 0x3B:
          case 0x3C:
          case 0x3D:
          case 0x3E:
          case 0x3F:
            /* push (opcode - 64) as a tagged int; it will be < 0 */
            PushTaggedInt (&state, (int)opcode - 64);
            break;
          case 0x40:
          case 0x41:
          case 0x42:
          case 0x43:
          case 0x44:
          case 0x45:
          case 0x46:
          case 0x47:
          case 0x48:
          case 0x49:
          case 0x4A:
          case 0x4B:
          case 0x4C:
          case 0x4D:
          case 0x4E:
          case 0x4F:
          case 0x50:
          case 0x51:
          case 0x52:
          case 0x53:
          case 0x54:
          case 0x55:
          case 0x56:
          case 0x57:
          case 0x58:
          case 0x59:
          case 0x5A:
          case 0x5B:
          case 0x5C:
          case 0x5D:
          case 0x5E:
          case 0x5F:
            /* push (opcode - 64) as a raw int64 */
            PushRawInt64 (&state, (Int64_t)opcode - 64);
            break;
          case 0x60:
          case 0x61:
          case 0x62:
          case 0x63:
          case 0x64:
          case 0x65:
          case 0x66:
          case 0x67:
          case 0x68:
          case 0x69:
          case 0x6A:
          case 0x6B:
          case 0x6C:
          case 0x6D:
          case 0x6E:
          case 0x6F:
          case 0x70:
          case 0x71:
          case 0x72:
          case 0x73:
          case 0x74:
          case 0x75:
          case 0x76:
          case 0x77:
          case 0x78:
          case 0x79:
          case 0x7A:
          case 0x7B:
          case 0x7C:
          case 0x7D:
          case 0x7E:
          case 0x7F:
            /* push (opcode - 128) as a raw int64; it will be < 0 */
            PushRawInt64 (&state, (Int64_t)opcode - 128);
            break;
          case 0x80: /* INT63(b) */
            PushTaggedInt (&state, GetI8Arg(&state));
SayDebug("### TOP = %p\n", TOP->val.ml);
            break;
          case 0x81: /* INT63(h) */
            PushTaggedInt (&state, GetI16Arg(&state));
            break;
          case 0x82: /* INT63(w) */
            PushTaggedInt (&state, (Int64_t)GetI32Arg(&state));
            break;
          case 0x83: /* INT63(l) */
            PushTaggedInt (&state, (Int64_t)GetI64Arg(&state));
            break;
          case 0x84: /* REAL32 */
            PushReal32(&state, GetR32Arg(&state));
            break;
          case 0x85: /* REAL64 */
            PushReal64(&state, GetR64Arg(&state));
            break;
          case 0x86:
          case 0x87: {
                /* BIGINT(sign, uh) */
                bool_t sign = ((opcode & 1) == 1);
                int nDigits = GetU16Arg(&state);
/* TODO: build a list from the digits */
	        Die("BIGINT -- reserved for future use");
            } break;
          case 0x88: /* STR8(0) */
            /* push the empty string */
            PushMLValue (&state, ML_string0);
            break;
          case 0x89: /* STR8(ub) */
            PushString (&state, GetU8Arg(&state));
            break;
          case 0x8A: /* STR8(uh) */
            PushString (&state, GetU16Arg(&state));
            break;
          case 0x8B: /* STR8(uw) */
            PushString (&state, GetU32Arg(&state));
            break;
          case 0x8C: /* UTF8(0) */
          case 0x8D: /* UTF8(ub) */
          case 0x8E: /* UTF8(uh) */
          case 0x8F: /* UTF8(uw) */
            /* reserved for future use */
	    Die("UTF8 -- reserved for future use");
            break;
          case 0x90: /* RAWINT(8,8) */
            /* reserved for future use */
	    Die("RAWINT(8,8) -- reserved for future use");
            break;
          /* 0x91 -- 0x93 UNUSED */
          case 0x94: /* RAWINT(16,8) */
            /* reserved for future use */
	    Die("RAWINT(16,8) -- reserved for future use");
            break;
          case 0x95: /* RAWINT(16,16) */
            /* reserved for future use */
	    Die("RAWINT(16,16) -- reserved for future use");
            break;
          /* 0x96 -- 0x97 UNUSED */
          case 0x98: /* RAWINT(32,8) */
            /* reserved for future use */
	    Die("RAWINT(32,8) -- reserved for future use");
            break;
          case 0x99: /* RAWINT(32,16) */
            /* reserved for future use */
	    Die("RAWINT(32,16) -- reserved for future use");
            break;
          case 0x9A: /* RAWINT(32,32) */
            /* reserved for future use */
	    Die("RAWINT(32,32) -- reserved for future use");
            break;
          /* 0x9B UNUSED */
          case 0x9C: /* RAWINT(64,8) */
            PushRawInt64 (&state, (Int64_t)GetI8Arg(&state));
            break;
          case 0x9D: /* RAWINT(64,16) */
            PushRawInt64 (&state, (Int64_t)GetI16Arg(&state));
            break;
          case 0x9E: /* RAWINT(64,32) */
            PushRawInt64 (&state, (Int64_t)GetI32Arg(&state));
            break;
          case 0x9F: /* RAWINT(64,64) */
            PushRawInt64 (&state, (Int64_t)GetI64Arg(&state));
            break;
          case 0xA0:
          case 0xA1:
          case 0xA2:
          case 0xA3:
          case 0xA4:
          case 0xA5:
          case 0xA6:
          case 0xA7:
          case 0xA8:
          case 0xA9:
          case 0xAA:
          case 0xAB:
          case 0xAC:
          case 0xAD: /* RECORD */
            PushRecord (&state, (int)(opcode & 0xF) + 1);
            break;
          case 0xAE: /* RECORD(ub) */
            PushRecord (&state, GetU8Arg(&state));
            break;
          case 0xAF: /* RECORD(uh) */
            PushRecord (&state, GetU16Arg(&state));
            break;
          case 0xB0:
          case 0xB1:
          case 0xB2:
          case 0xB3:
          case 0xB4:
          case 0xB5:
          case 0xB6:
          case 0xB7:
          case 0xB8:
          case 0xB9:
          case 0xBA:
          case 0xBB:
          case 0xBC:
          case 0xBD: /* RAW */
            PushRawRecord (&state, (int)(opcode & 0xF) + 1);
            break;
          case 0xBE: /* RAW(ub) */
            PushRawRecord (&state, GetU8Arg(&state));
            break;
          case 0xBF: /* RAW(uh) */
            PushRawRecord (&state, GetU16Arg(&state));
            break;
          case 0xC0: { /* MIXED(ub, ub) */
                int ptrLen = GetU8Arg(&state);
                int rawLen = GetU8Arg(&state);
                PushMixedRecord (&state, ptrLen, rawLen);
            } break;
          case 0xC1: { /* MIXED(uh, uh) */
                int ptrLen = GetU16Arg(&state);
                int rawLen = GetU16Arg(&state);
                PushMixedRecord (&state, ptrLen, rawLen);
            } break;
          /* 0xC2 -- 0xC7 UNUSED */
          case 0xC8: /* VEC(0) */
            PushMLValue(&state, ML_vector0);
            break;
          case 0xC9: { /* VEC(ub) */
                int len = GetU8Arg(&state);
                PushVector (&state, len);
            } break;
          case 0xCA: { /* VEC(uh) */
                int len = GetU16Arg(&state);
                PushVector (&state, len);
             } break;
          /* 0xCB -- 0xD7 UNUSED */
          case 0xD8:
          case 0xD9:
          case 0xDA:
          case 0xDB: {
                /* RAWVEC(sz, ub) */
                int elemSz = 1 << (opcode & 0x7);
                int len = GetU8Arg(&state);
	        Die("RAWVEC(%d,%d) -- reserved for future use", elemSz, len);
            } break;
          case 0xDC:
          case 0xDD:
          case 0xDE:
          case 0xDF: {
                /* RAWVEC(sz, uh) */
                int elemSz = 1 << (opcode & 0x7);
                int len = GetU16Arg(&state);
	        Die("RAWVEC(%d,%d) -- reserved for future use", elemSz, len);
            } break;
          case 0xE0:
          case 0xE1:
          case 0xE2:
          case 0xE3: {
                /* RAWVEC(sz, uw) */
                int elemSz = 1 << (opcode & 0x7);
                Unsigned32_t len = GetU32Arg(&state);
	        Die("RAWVEC(%d,%d) -- reserved for future use", elemSz, len);
            } break;
          /* 0xE4 -- 0xE7 UNUSED */
          case 0xE8:
          case 0xE9:
          case 0xEA:
          case 0xEB:
          case 0xEC:
          case 0xED:
          case 0xEE: {
                /* STORE */
                int slot = (int)(opcode & 0x7);
                ASSERT (TOP->kind == VK_OBJ);
                ASSERT(slot < state.maxSaved);
                state.saved[slot] = TOP->val.ml;
            } break;
          case 0xEF: { /* STORE(uh) */
                int slot = (int)GetU16Arg(&state);
                ASSERT (TOP->kind == VK_OBJ);
                ASSERT(slot < state.maxSaved);
                state.saved[slot] = TOP->val.ml;
            } break;
          case 0xF0:
          case 0xF1:
          case 0xF2:
          case 0xF3:
          case 0xF4:
          case 0xF5:
          case 0xF6: {
                /* LOAD */
                int slot = (int)(opcode & 0x7);
                ASSERT(slot < state.maxSaved);
                PushMLValue(&state, state.saved[slot]);
            } break;
          case 0xF7: { /* LOAD(uh) */
                int slot = (int)GetU16Arg(&state);
                ASSERT(slot < state.maxSaved);
                PushMLValue(&state, state.saved[slot]);
            } break;
          case 0xF8:
          case 0xF9:
          case 0xFA:
          case 0xFB:
          case 0xFC:
          case 0xFD:
          case 0xFE: {
                /* CONCAT */
                int nArgs = (int)(opcode & 0x7) + 2;
/* TODO */
	        Die("CONCAT(%d) -- reserved for future use", nArgs);
            }
          case 0xFF: { /* RETURN */
                ASSERT(state.tos == 0);
                ASSERT((TOP->kind == VK_OBJ) || (TOP->kind == VK_TAGINT));
                ml_val_t res = TOP->val.ml;
                /* free memory */
                if (state.saved != NIL(ml_val_t *)) { FREE(state.saved); }
                FREE(state.stk);
#ifdef DEBUG_LITERALS
                SayDebug("BuildLiterals: return %p\n", res);
#endif
                return res;
            }
          default:
            Die ("BuildLiterals: bogus literal opcode #%04x @ %d",
                (int)opcode, state.pc-1);
        } /* switch */
    } /* while */

} /* end of BuildLiterals */
