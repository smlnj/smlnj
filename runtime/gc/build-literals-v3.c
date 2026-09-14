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

STATIC_INLINE void StoreTaggedInt (StkItem_t *dst, Int64_t n)
{
    dst->kind = VK_TAGINT;
    dst->val.i = (Unsigned64_t)n;
}
STATIC_INLINE void StoreRawInt64 (StkItem_t *dst, Int64_t n)
{
    dst->kind = VK_RAWINT;
    dst->val.i = (Unsigned64_t)n;
}
STATIC_INLINE void StoreReal64 (StkItem_t *dst, double d)
{
    dst->kind = VK_TAGINT;
    dst->val.d = d;
}
STATIC_INLINE void StoreMLValue (StkItem_t *dst, ml_val_t v)
{
    dst->kind = VK_OBJ;
    dst->val.ml = v;
}

/* copy bytes from the instruction stream in correct byte order (the
 * instruction stream is in bigendian order)
 */
STATIC_INLINE void GetBytes (Byte_t *dst, Byte_t *code, int n)
{
    int i;
    for (i = 0;  i < n;  i++) {
#ifdef BYTE_ORDER_LITTLE
        dst[n-1-i] = code[i];
#else
        dst[i] = code[i];
#endif
    }
}

/* inline functions for fetching arguments */
STATIC_INLINE signed char GetI8Arg (Byte_t *code)
{
    signed char i = code[0];
    return i;
}
STATIC_INLINE unsigned char GetU8Arg (Byte_t *code)
{
    unsigned char i = code[0];
    return i;
}
STATIC_INLINE Int16_t GetI16Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(Int16_t)]; Int16_t i; } arg;
    GetBytes(arg.b, code, sizeof(Int16_t));
    return arg.i;
}
STATIC_INLINE Unsigned16_t GetU16Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(Unsigned16_t)]; Unsigned16_t u; } arg;
    GetBytes(arg.b, code, sizeof(Unsigned16_t));
    return arg.u;
}
STATIC_INLINE Int32_t GetI32Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(Int32_t)]; Int32_t i; } arg;
    GetBytes(arg.b, code, sizeof(Int32_t));
    return arg.i;
}
STATIC_INLINE Unsigned32_t GetU32Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(Unsigned32_t)]; Unsigned32_t u; } arg;
    GetBytes(arg.b, code, sizeof(Unsigned32_t));
    return arg.u;
}
STATIC_INLINE Int64_t GetI64Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(Int64_t)]; Int64_t i; } arg;
    GetBytes(arg.b, code, sizeof(Int64_t));
    return arg.i;
}
STATIC_INLINE Unsigned64_t GetU64Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(Unsigned64_t)]; Unsigned64_t u; } arg;
    GetBytes(arg.b, code, sizeof(Unsigned64_t));
    return arg.u;
}
STATIC_INLINE float GetR32Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(float)]; float r; } arg;
    GetBytes(arg.b, code, sizeof(float));
    return arg.r;
}
STATIC_INLINE double GetR64Arg (Byte_t *code)
{
    union { Byte_t b[sizeof(double)]; double r; } arg;
    GetBytes(arg.b, code, sizeof(double));
    return arg.r;
}
#ifdef SIZE_64
#define GetRawArg       GetU64Arg
#else /* SIZE_32 */
#define GetRawArg       GetU32Arg
#endif

/* the size of a list cons cell in bytes */
#define CONS_SZB        (WORD_SZB*3)

/* the amount of free space that we want in the allocation arena; this value must be
 * less than MIN_ALLOC_SZB (defined in include/ml-limits.h)
 */
#define FREE_REQ_SZB    64*ONE_K

/* for backward compatibility */
ml_val_t BuildLiteralsV1 (ml_state_t *msp, Byte_t *lits, int pc, int len);
ml_val_t BuildLiteralsV2 (ml_state_t *msp, Byte_t *lits, int pc, int len);

#ifdef DEBUG_LITERALS
#  define GC_MESSAGE    SayDebug("BuildLiterals: invoke GC\n");
#else
#  define GC_MESSAGE
#endif

/* save roots in the heap prior to GC */
PVT ml_val_t SaveRoots (ml_state_t *msp, int maxSaved, ml_val_t *saved, int tos, StkItem_t *stk)
{
    /* the roots cover the size of the save area plus the current stack depth */
    int nRoots = maxSaved + tos + 1;

    ML_AllocWrite(msp, 0, MAKE_DESC(nRoots, DTAG_record));
    /* first we copy the values from the saved array */
    for (int i = 0;  i < maxSaved;  ++i) {
        ML_AllocWrite(msp, i+1, saved[i]);
    }
    /* copy the stack roots starting from the bottom */
    for (int i = 0;  i <= tos;  ++i) {
        if (stk[i].kind == VK_OBJ) {
            ML_AllocWrite(msp, maxSaved+1+i, stk[i].val.ml);
        } else {
            /* we store unit for non-object stack slots */
            ML_AllocWrite(msp, maxSaved+1+i, ML_unit);
        }
    }
    return ML_Alloc(msp, nRoots);
}

/* restore the roots after a GC */
PVT void RestoreRoots (ml_val_t roots, int maxSaved, ml_val_t *saved, int tos, StkItem_t *stk)
{
    ml_val_t *p = (ml_val_t *)roots;

    /* restore the saved array */
    for (int i = 0;  i < maxSaved;  ++i, p++) {
        saved[i] = *p;
    }

    /* restore the objects in the stack. */
    for (int i = 0;  i <= tos;  ++i, p++) {
        if (stk[i].kind == VK_OBJ) {
            stk[i].val.ml = *p;
        }
    }
}

/* BuildLiterals:
 *
 * NOTE: we allocate all of the objects in the first generation, and allocate
 * the vector of literals in the allocation space.
 */
ml_val_t BuildLiterals (ml_state_t *msp, Byte_t *code, int len)
{
    int         pc = 0;
#ifdef DEBUG_LITERALS
    int         depth = 0;
#endif
    Unsigned32_t magic, maxDepth, wordSz, maxSaved;
    ml_val_t    res;
    Int32_t     availSpace, spaceReq;
    Unsigned32_t ui;

/* A check that the available space is sufficient for the literal object that
 * we are about to allocate.  Note that the cons cell has already been accounted
 * for in availSpace (but not in spaceReq).
 */
#define GC_CHECK                                                                                \
    do {                                                                                        \
        if (spaceReq > availSpace) {                                                            \
            GC_MESSAGE                                                                          \
            InvokeGCWithRoots (msp, 0, (ml_val_t *)&code, &stk, &saved, NIL(ml_val_t *));       \
            availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr) - CONS_SZB;      \
        }                                                                                       \
        else                                                                                    \
            availSpace -= spaceReq;                                                             \
    } while (0)

#ifdef DEBUG_LITERALS
    SayDebug("BuildLiterals: code = %p, len = %d\n", (void *)code, len);
#endif
    if (len <= 8) return ML_nil;

    magic = GetU32Arg(code+pc); pc += 4;
    maxDepth = GetU32Arg(code+pc); pc += 4;

    if (magic == V1_MAGIC) {
#ifdef DEBUG_LITERALS
        SayDebug("BuildLiterals: VERSION 1\n");
#endif
        return BuildLiteralsV1 (msp, code, pc, len);
    }
    else if (magic == V2_MAGIC) {
#ifdef DEBUG_LITERALS
        SayDebug("BuildLiterals: VERSION 2\n");
#endif
        return BuildLiteralsV2 (msp, code, pc, len);
    }
    else if (magic != V3_MAGIC) {
        Die("bogus literal magic number %#x", magic);
    }
#ifdef DEBUG_LITERALS
        SayDebug("BuildLiterals: VERSION 3\n");
#endif

  /* get the rest of the header */
    wordSz = GetU32Arg(code+pc); pc += 4;
    maxSaved = GetU32Arg(code+pc); pc += 4;

    if (wordSz != 64) {
        Die("expected word size = 64, but found %d\n", wordSz);
    }

  /* We represent the saved array as a C array of ML values.  When we do a GC, we
   * copy these into a heap-allocated root record.
   */
    ml_val_t *saved;
    /* allocate space for the saved area */
    if (maxSaved > 0) {
        saved = NEW_VEC(ml_val_t, maxSaved);
        for (int i = 0;  i < maxSaved;  ++i) {
            saved[i] = ML_unit;
        }
    }
    else {
        saved = NIL(ml_val_t *);
    }

    /* allocate space for the stack */
    StkItem_t *stk = NEW_VEC(StkItem_t, maxDepth);
    int tos = -1;

#ifdef DEBUG_LITERALS
    SayDebug("BuildLiterals: avail = %d bytes; maxDepth = %d, maxSaved = %d\n",
        (int)availSpace, (int)maxDepth, (int)maxSaved);
#endif
    while (TRUE) {
        availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
        ASSERT(pc < len);
        ASSERT(availSpace <= (Int32_t)((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr));

        /* an upper bound on the amount of space needed for the stack and saved
         * literal
        if (availSpace < 512 * WORD_SZB) {
            if (NeedGC(msp, FREE_REQ_SZB)) {
                GC_MESSAGE
                InvokeGCWithRoots (msp, 0, (ml_val_t *)&code, &stk, &saved, NIL(ml_val_t *));
            }
            availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
        }
        availSpace -= CONS_SZB; /* space for stack cons cell */

#ifdef DEBUG_LITERALS
        int startPC = pc;
#endif

/* top of stack */
#define TOP     (stk+tos)
#define PUSH    (&stk[++tos])
#define POP     (&stk[tos--])

    /* get the next instruction */
        Byte_t opcode = code[pc++];

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
            /* push opcode as tagged int */
            StoreTaggedInt (PUSH, INT_CtoML(int)opcode);
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
            /* push (opcode - 64) as tagged int */
            StoreTaggedInt (PUSH, INT_CtoML((int)opcode - 64);
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
            /* push (opcode - 64) as raw int64 */
            StoreRawInt64 (PUSH, (Int64_t)opcode - 64);
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
            /* push (opcode - 128) as raw int64 */
            StoreRawInt64 (PUSH, (Int64_t)opcode - 128);
            break;
          case 0x80: /* INT63(b) */
            StoreTaggedInt (PUSH, (Int64_t)GetI8Arg(&(code[pc])));  pc += 1;
            break;
          case 0x81: /* INT63(h) */
            StoreTaggedInt (PUSH, (Int64_t)GetI16Arg(&(code[pc])));  pc += 2;
            break;
          case 0x82: /* INT63(w) */
            StoreTaggedInt (PUSH, (Int64_t)GetI32Arg(&(code[pc])));  pc += 4;
            break;
          case 0x83: /* INT63(l) */
            StoreTaggedInt (PUSH, (Int64_t)GetI64Arg(&(code[pc])));  pc += 8;
            break;
          case 0x84: /* REAL32 */
            StoreReal32(PUSH, GetR32Arg(&(code[pc])));  pc += 4;
            break;
          case 0x85: /* REAL64 */
            StoreReal64(PUSH, GetR64Arg(&(code[pc])));  pc += 8;
            break;
          case 0x86:
          case 0x87: {
                /* BIGINT(sign, uh) */
                bool_t sign = ((opcode & 1) == 1);
                int nDigits = GetU16Arg(&(code[pc]));  pc += 2;
/* TODO: build a list from the digits */
	        Die("BIGINT -- not supported yet");
            } break;
          case 0x88: /* STR8(0) */
            /* push the empty string */
            StoreMLValue(PUSH, ML_string0);
            break;
          case 0x89: { /* STR8(ub) */
                int len = GetU8Arg(&(code[pc]));  pc += 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0x8A: { /* STR8(uh) */
                int len = GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0x8B: { /* STR8(uw) */
                Unsigned32_t len = GetU32Arg(&(code[pc]));  pc += 4;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
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
            StoreRawInt64 (PUSH, (Int64_t)GetI8Arg(&(code[pc])));  pc += 1;
            break;
          case 0x9D: /* RAWINT(64,16) */
            StoreRawInt64 (PUSH, (Int64_t)GetI16Arg(&(code[pc])));  pc += 2;
            break;
          case 0x9E: /* RAWINT(64,32) */
            StoreRawInt64 (PUSH, (Int64_t)GetI32Arg(&(code[pc])));  pc += 4;
            break;
          case 0x9F: /* RAWINT(64,64) */
            StoreRawInt64 (PUSH, (Int64_t)GetI64Arg(&(code[pc])));  pc += 8;
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
          case 0xAD: {
                /* RECORD */
                int len = (int)(opcode & 0xF) + 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xAE: { /* RECORD(ub) */
                int len = GetU8Arg(&(code[pc]));  pc += 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xAF: { /* RECORD(uh) */
                int len = GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
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
          case 0xBD: {
                /* RAW */
                int len = (int)(opcode & 0xF) + 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xBE: { /* RAW(ub) */
                int len = GetU8Arg(&(code[pc]));  pc += 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xBF: { /* RAW(uh) */
                int len = GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xC0: { /* MIXED(ub, ub) */
                int ptrLen = GetU8Arg(&(code[pc]));  pc += 1;
                int rawLen = GetU8Arg(&(code[pc]));  pc += 1;
                ASSERT (ptrLen + rawLen <= tos + 1);
/* TODO */
            } break;
          case 0xC1{ : /* MIXED(uh, uh) */
                int ptrLen = GetU16Arg(&(code[pc]));  pc += 2;
                int rawLen = GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (ptrLen + rawLen <= tos + 1);
/* TODO */
            } break;
          /* 0xC2 -- 0xC7 UNUSED */
          case 0xC8:
          case 0xC9:
          case 0xCA:
          case 0xCB:
          case 0xCC:
          case 0xCD:
          case 0xCE:
          case 0xCF:
          case 0xD0:
          case 0xD1:
          case 0xD2:
          case 0xD3:
          case 0xD4: {
                /* VEC */
                int len = (int)(opcode & 0xF);
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xD5: { /* VEC(ub) */
                /* VEC */
                int len = GetU8Arg(&(code[pc]));  pc += 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xD6: { /* VEC(uh) */
                /* VEC */
                int len = GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xD7: { /* VEC(uw) */
                /* VEC */
                Unsigned32_t len = GetU32Arg(&(code[pc]));  pc += 4;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xD8:
          case 0xD9:
          case 0xDA:
          case 0xDB: {
                /* RAWVEC(sz, ub) */
                int elemSz = 1 << (opcode & 0x7);
                int len = GetU8Arg(&(code[pc]));  pc += 1;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xDC:
          case 0xDD:
          case 0xDE:
          case 0xDF: {
                /* RAWVEC(sz, uh) */
                int elemSz = 1 << (opcode & 0x7);
                int len = GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (len <= tos + 1);
/* TODO */
            } break;
          case 0xE0:
          case 0xE1:
          case 0xE2:
          case 0xE3: {
                /* RAWVEC(sz, uw) */
                int elemSz = 1 << (opcode & 0x7);
                Unsigned32_t len = GetU32Arg(&(code[pc]));  pc += 4;
                ASSERT (len <= tos + 1);
/* TODO */
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
                ASSERT(slot < maxSaved);
                saved[slot] = TOP->val.ml;
            } break;
          case 0xEF: { /* STORE(uh) */
                int slot = (int)GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT (TOP->kind == VK_OBJ);
                ASSERT(slot < maxSaved);
                saved[slot] = TOP->val.ml;
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
                ASSERT(slot < maxSaved);
                StoreMLValue(PUSH, saved[slot]);
            } break;
          case 0xF7: { /* LOAD(uh) */
                int slot = (int)GetU16Arg(&(code[pc]));  pc += 2;
                ASSERT(slot < maxSaved);
                StoreMLValue(PUSH, saved[slot]);
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
            }
          case 0xFF: { /* RETURN */
                ASSERT(tos == 0);
                ASSERT(TOP->kind == VK_OBJ);
                ml_val_t res = TOP->val.ml;
                /* free memory */
                if (saved != NIL(ml_val_t *)) { FREE(saved); }
                FREE(stk);
                return res;
            }
          default:
            Die ("BuildLiterals: bogus literal opcode #%04x @ %d", (int)opcode, pc-1);
        } /* switch */
    } /* while */

} /* end of BuildLiterals */
