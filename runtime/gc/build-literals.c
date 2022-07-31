/*! \file build-literals.c
 *
 * This file implements a simple bytecode interpreter that implements a language
 * for initializing a record of compile-time constant values.
 *
 * This code needs to agree with the code generator in base/CPS/main/new-literals.sml
 *
 * See dev-notes/new-literals.md for a description of the bytecode.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "heap.h"
#include <string.h>
#include <inttypes.h>

/* printf formats for Int_t/Word_t types */
#ifdef SIZE_64
#  define PRINT		PRId64
#  define PRWORD	PRIu64
#else
#  define PRINT		PRId32
#  define PRWORD	PRIu32
#endif

#define V1_MAGIC	0x19981022
#define V2_MAGIC	0x20190921

/* Codes for literal machine instructions (version 2) */
enum {
    UNUSED = 0,
    INT,
    INT32,
    INT64,
    BIGINT,
    IVEC,
    IVEC8,
    IVEC16,
    IVEC32,
    IVEC64,
    REAL32,
    REAL64,
    RVEC32,
    RVEC64,
    STR8,
    RECORD,
    VECTOR,
    RAW,	/* word-sized raw data */
    RAW32,	/* 32-bit aligned raw data; will be padded to 64-bits on 64-bit targets */
    RAW64,	/* 64-bit aligned raw data */
    CONCAT,
    SAVE,
    LOAD,
    RETURN,
    INVALID	/* invalid opcode */
};

/* argument types; if present, the argument may either be a literal value or
 * a count of number of values to include in a vector/record.  In the latter
 * case we specify the sign and size of the literal in the instruction stream
 * and the size of that the instruction requires.
 */
enum {
    NO_ARG,		/* no argument */
    IMMED_ARG,		/* get argument from immedArg field (32 bits) */
    I8_ARG,		/* one-byte signed argument */
    U8_ARG,		/* one-byte unsigned argument */
    I16_ARG,		/* two-byte signed argument */
    U16_ARG,		/* two-byte unsigned argument */
    I32_ARG,		/* four-byte signed argument */
    U32_ARG,		/* four-byte unsigned argument */
    I64_ARG,		/* eight-byte signed argument (64 bits) */
    INT_ARG		/* target-sized integer argument */
};

static struct instr_info {
	unsigned char oper;		/* the type of operation */
	unsigned char argKind;		/* the kind of argument */
	signed char immedArg;		/* immediate argument (if present) */
        unsigned char pad;		/* pad to four bytes */
    } InstrInfo[256] = {
	/* 0x00 */ { INT, IMMED_ARG, 0, 0 },
	/* 0x01 */ { INT, IMMED_ARG, 1, 0 },
	/* 0x02 */ { INT, IMMED_ARG, 2, 0 },
	/* 0x03 */ { INT, IMMED_ARG, 3, 0 },
	/* 0x04 */ { INT, IMMED_ARG, 4, 0 },
	/* 0x05 */ { INT, IMMED_ARG, 5, 0 },
	/* 0x06 */ { INT, IMMED_ARG, 6, 0 },
	/* 0x07 */ { INT, IMMED_ARG, 7, 0 },
	/* 0x08 */ { INT, IMMED_ARG, 8, 0 },
	/* 0x09 */ { INT, IMMED_ARG, 9, 0 },
	/* 0x0A */ { INT, IMMED_ARG, 10, 0 },
	/* 0x0B */ { INT, IMMED_ARG, -1, 0 },
	/* 0x0C */ { INT, IMMED_ARG, -2, 0 },
	/* 0x0D */ { INT, IMMED_ARG, -3, 0 },
	/* 0x0E */ { INT, IMMED_ARG, -4, 0 },
	/* 0x0F */ { INT, IMMED_ARG, -5, 0 },
	/* 0x10 */ { INT, I8_ARG, 0, 0 },
	/* 0x11 */ { INT, I16_ARG, 0, 0 },
	/* 0x12 */ { INT, I32_ARG, 0, 0 },
#ifdef SIZE_64
	/* 0x13 */ { INT, I64_ARG, 0, 0 },
#else /* SIZE_32 */
	/* 0x13 */ { INVALID, NO_ARG, 0, 0 },
#endif
#ifdef SIZE_64
	/* 0x14 */ { INVALID, NO_ARG, 0, 0 },
	/* 0x15 */ { INVALID, NO_ARG, 0, 0 },
	/* 0x16 */ { INVALID, NO_ARG, 0, 0 },
	/* 0x17 */ { INT64, I8_ARG, 0, 0 },
	/* 0x18 */ { INT64, I16_ARG, 0, 0 },
	/* 0x19 */ { INT64, I32_ARG, 0, 0 },
	/* 0x1A */ { INT64, I64_ARG, 0, 0 },
#else /* SIZE_32 */
	/* 0x14 */ { INT32, I8_ARG, 0, 0 },
	/* 0x15 */ { INT32, I16_ARG, 0, 0 },
	/* 0x16 */ { INT32, I32_ARG, 0, 0 },
	/* 0x17 */ { INVALID, NO_ARG, 0, 0 },
	/* 0x18 */ { INVALID, NO_ARG, 0, 0 },
	/* 0x19 */ { INVALID, NO_ARG, 0, 0 },
	/* 0x1A */ { INVALID, NO_ARG, 0, 0 },
#endif
	/* 0x1B */ { BIGINT, U32_ARG, 0, 0 },
	/* 0x1C */ { IVEC, U8_ARG, 0, 0 },
	/* 0x1D */ { IVEC, U32_ARG, 0, 0 },
	/* 0x1E */ { IVEC8, U8_ARG, 0, 0 },
	/* 0x1F */ { IVEC8, U32_ARG, 0, 0 },
	/* 0x20 */ { IVEC16, U8_ARG, 0, 0 },
	/* 0x21 */ { IVEC16, U32_ARG, 0, 0 },
	/* 0x22 */ { IVEC32, U8_ARG, 0, 0 },
	/* 0x23 */ { IVEC32, U32_ARG, 0, 0 },
	/* 0x24 */ { IVEC64, U8_ARG, 0, 0 },
	/* 0x25 */ { IVEC64, U32_ARG, 0, 0 },
	/* 0x26 */ { REAL32, NO_ARG, 0, 0 },
	/* 0x27 */ { REAL64, NO_ARG, 0, 0 },
	/* 0x28 */ { RVEC32, U8_ARG, 0, 0 },
	/* 0x29 */ { RVEC32, U32_ARG, 0, 0 },
	/* 0x2A */ { RVEC64, U8_ARG, 0, 0 },
	/* 0x2B */ { RVEC64, U32_ARG, 0, 0 },
	/* 0x2C */ { STR8, U8_ARG, 0, 0 },
	/* 0x2D */ { STR8, INT_ARG, 0, 0 },
	/* 0x2E */ { UNUSED, 0, 0, 0 },
	/* 0x2F */ { UNUSED, 0, 0, 0 },
	/* 0x30 */ { RECORD, IMMED_ARG, 1, 0 },
	/* 0x31 */ { RECORD, IMMED_ARG, 2, 0 },
	/* 0x32 */ { RECORD, IMMED_ARG, 3, 0 },
	/* 0x33 */ { RECORD, IMMED_ARG, 4, 0 },
	/* 0x34 */ { RECORD, IMMED_ARG, 5, 0 },
	/* 0x35 */ { RECORD, IMMED_ARG, 6, 0 },
	/* 0x36 */ { RECORD, IMMED_ARG, 7, 0 },
	/* 0x37 */ { RECORD, U8_ARG, 0, 0 },
	/* 0x38 */ { RECORD, U32_ARG, 0, 0 },
	/* 0x39 */ { VECTOR, U8_ARG, 0, 0 },
	/* 0x3A */ { VECTOR, U32_ARG, 0, 0 },
	/* 0x3B */ { RAW, IMMED_ARG, 1, 0 },
	/* 0x3C */ { RAW, IMMED_ARG, 2, 0 },
	/* 0x3D */ { RAW, U8_ARG, 0, 0 },
	/* 0x3E */ { RAW, U32_ARG, 0, 0 },
	/* 0x3F */ { RAW32, U8_ARG, 0, 0 },
	/* 0x40 */ { RAW32, U32_ARG, 0, 0 },
	/* 0x41 */ { RAW64, U8_ARG, 0, 0 },
	/* 0x42 */ { RAW64, U32_ARG, 0, 0 },
	/* 0x43 */ { CONCAT, U16_ARG, 0, 0 },
	/* 0x44 */ { SAVE, U8_ARG, 0, 0 },
	/* 0x45 */ { SAVE, U16_ARG, 0, 0 },
	/* 0x46 */ { LOAD, U8_ARG, 0, 0 },
	/* 0x47 */ { LOAD, U16_ARG, 0, 0 },
	/* 0x48 */ { UNUSED, 0, 0, 0 },
	/* 0x49 */ { UNUSED, 0, 0, 0 },
	/* 0x4A */ { UNUSED, 0, 0, 0 },
	/* 0x4B */ { UNUSED, 0, 0, 0 },
	/* 0x4C */ { UNUSED, 0, 0, 0 },
	/* 0x4D */ { UNUSED, 0, 0, 0 },
	/* 0x4E */ { UNUSED, 0, 0, 0 },
	/* 0x4F */ { UNUSED, 0, 0, 0 },
	/* 0x50 */ { UNUSED, 0, 0, 0 },
	/* 0x51 */ { UNUSED, 0, 0, 0 },
	/* 0x52 */ { UNUSED, 0, 0, 0 },
	/* 0x53 */ { UNUSED, 0, 0, 0 },
	/* 0x54 */ { UNUSED, 0, 0, 0 },
	/* 0x55 */ { UNUSED, 0, 0, 0 },
	/* 0x56 */ { UNUSED, 0, 0, 0 },
	/* 0x57 */ { UNUSED, 0, 0, 0 },
	/* 0x58 */ { UNUSED, 0, 0, 0 },
	/* 0x59 */ { UNUSED, 0, 0, 0 },
	/* 0x5A */ { UNUSED, 0, 0, 0 },
	/* 0x5B */ { UNUSED, 0, 0, 0 },
	/* 0x5C */ { UNUSED, 0, 0, 0 },
	/* 0x5D */ { UNUSED, 0, 0, 0 },
	/* 0x5E */ { UNUSED, 0, 0, 0 },
	/* 0x5F */ { UNUSED, 0, 0, 0 },
	/* 0x60 */ { UNUSED, 0, 0, 0 },
	/* 0x61 */ { UNUSED, 0, 0, 0 },
	/* 0x62 */ { UNUSED, 0, 0, 0 },
	/* 0x63 */ { UNUSED, 0, 0, 0 },
	/* 0x64 */ { UNUSED, 0, 0, 0 },
	/* 0x65 */ { UNUSED, 0, 0, 0 },
	/* 0x66 */ { UNUSED, 0, 0, 0 },
	/* 0x67 */ { UNUSED, 0, 0, 0 },
	/* 0x68 */ { UNUSED, 0, 0, 0 },
	/* 0x69 */ { UNUSED, 0, 0, 0 },
	/* 0x6A */ { UNUSED, 0, 0, 0 },
	/* 0x6B */ { UNUSED, 0, 0, 0 },
	/* 0x6C */ { UNUSED, 0, 0, 0 },
	/* 0x6D */ { UNUSED, 0, 0, 0 },
	/* 0x6E */ { UNUSED, 0, 0, 0 },
	/* 0x6F */ { UNUSED, 0, 0, 0 },
	/* 0x70 */ { UNUSED, 0, 0, 0 },
	/* 0x71 */ { UNUSED, 0, 0, 0 },
	/* 0x72 */ { UNUSED, 0, 0, 0 },
	/* 0x73 */ { UNUSED, 0, 0, 0 },
	/* 0x74 */ { UNUSED, 0, 0, 0 },
	/* 0x75 */ { UNUSED, 0, 0, 0 },
	/* 0x76 */ { UNUSED, 0, 0, 0 },
	/* 0x77 */ { UNUSED, 0, 0, 0 },
	/* 0x78 */ { UNUSED, 0, 0, 0 },
	/* 0x79 */ { UNUSED, 0, 0, 0 },
	/* 0x7A */ { UNUSED, 0, 0, 0 },
	/* 0x7B */ { UNUSED, 0, 0, 0 },
	/* 0x7C */ { UNUSED, 0, 0, 0 },
	/* 0x7D */ { UNUSED, 0, 0, 0 },
	/* 0x7E */ { UNUSED, 0, 0, 0 },
	/* 0x7F */ { UNUSED, 0, 0, 0 },
	/* 0x80 */ { UNUSED, 0, 0, 0 },
	/* 0x81 */ { UNUSED, 0, 0, 0 },
	/* 0x82 */ { UNUSED, 0, 0, 0 },
	/* 0x83 */ { UNUSED, 0, 0, 0 },
	/* 0x84 */ { UNUSED, 0, 0, 0 },
	/* 0x85 */ { UNUSED, 0, 0, 0 },
	/* 0x86 */ { UNUSED, 0, 0, 0 },
	/* 0x87 */ { UNUSED, 0, 0, 0 },
	/* 0x88 */ { UNUSED, 0, 0, 0 },
	/* 0x89 */ { UNUSED, 0, 0, 0 },
	/* 0x8A */ { UNUSED, 0, 0, 0 },
	/* 0x8B */ { UNUSED, 0, 0, 0 },
	/* 0x8C */ { UNUSED, 0, 0, 0 },
	/* 0x8D */ { UNUSED, 0, 0, 0 },
	/* 0x8E */ { UNUSED, 0, 0, 0 },
	/* 0x8F */ { UNUSED, 0, 0, 0 },
	/* 0x90 */ { UNUSED, 0, 0, 0 },
	/* 0x91 */ { UNUSED, 0, 0, 0 },
	/* 0x92 */ { UNUSED, 0, 0, 0 },
	/* 0x93 */ { UNUSED, 0, 0, 0 },
	/* 0x94 */ { UNUSED, 0, 0, 0 },
	/* 0x95 */ { UNUSED, 0, 0, 0 },
	/* 0x96 */ { UNUSED, 0, 0, 0 },
	/* 0x97 */ { UNUSED, 0, 0, 0 },
	/* 0x98 */ { UNUSED, 0, 0, 0 },
	/* 0x99 */ { UNUSED, 0, 0, 0 },
	/* 0x9A */ { UNUSED, 0, 0, 0 },
	/* 0x9B */ { UNUSED, 0, 0, 0 },
	/* 0x9C */ { UNUSED, 0, 0, 0 },
	/* 0x9D */ { UNUSED, 0, 0, 0 },
	/* 0x9E */ { UNUSED, 0, 0, 0 },
	/* 0x9F */ { UNUSED, 0, 0, 0 },
	/* 0xA0 */ { UNUSED, 0, 0, 0 },
	/* 0xA1 */ { UNUSED, 0, 0, 0 },
	/* 0xA2 */ { UNUSED, 0, 0, 0 },
	/* 0xA3 */ { UNUSED, 0, 0, 0 },
	/* 0xA4 */ { UNUSED, 0, 0, 0 },
	/* 0xA5 */ { UNUSED, 0, 0, 0 },
	/* 0xA6 */ { UNUSED, 0, 0, 0 },
	/* 0xA7 */ { UNUSED, 0, 0, 0 },
	/* 0xA8 */ { UNUSED, 0, 0, 0 },
	/* 0xA9 */ { UNUSED, 0, 0, 0 },
	/* 0xAA */ { UNUSED, 0, 0, 0 },
	/* 0xAB */ { UNUSED, 0, 0, 0 },
	/* 0xAC */ { UNUSED, 0, 0, 0 },
	/* 0xAD */ { UNUSED, 0, 0, 0 },
	/* 0xAE */ { UNUSED, 0, 0, 0 },
	/* 0xAF */ { UNUSED, 0, 0, 0 },
	/* 0xB0 */ { UNUSED, 0, 0, 0 },
	/* 0xB1 */ { UNUSED, 0, 0, 0 },
	/* 0xB2 */ { UNUSED, 0, 0, 0 },
	/* 0xB3 */ { UNUSED, 0, 0, 0 },
	/* 0xB4 */ { UNUSED, 0, 0, 0 },
	/* 0xB5 */ { UNUSED, 0, 0, 0 },
	/* 0xB6 */ { UNUSED, 0, 0, 0 },
	/* 0xB7 */ { UNUSED, 0, 0, 0 },
	/* 0xB8 */ { UNUSED, 0, 0, 0 },
	/* 0xB9 */ { UNUSED, 0, 0, 0 },
	/* 0xBA */ { UNUSED, 0, 0, 0 },
	/* 0xBB */ { UNUSED, 0, 0, 0 },
	/* 0xBC */ { UNUSED, 0, 0, 0 },
	/* 0xBD */ { UNUSED, 0, 0, 0 },
	/* 0xBE */ { UNUSED, 0, 0, 0 },
	/* 0xBF */ { UNUSED, 0, 0, 0 },
	/* 0xC0 */ { UNUSED, 0, 0, 0 },
	/* 0xC1 */ { UNUSED, 0, 0, 0 },
	/* 0xC2 */ { UNUSED, 0, 0, 0 },
	/* 0xC3 */ { UNUSED, 0, 0, 0 },
	/* 0xC4 */ { UNUSED, 0, 0, 0 },
	/* 0xC5 */ { UNUSED, 0, 0, 0 },
	/* 0xC6 */ { UNUSED, 0, 0, 0 },
	/* 0xC7 */ { UNUSED, 0, 0, 0 },
	/* 0xC8 */ { UNUSED, 0, 0, 0 },
	/* 0xC9 */ { UNUSED, 0, 0, 0 },
	/* 0xCA */ { UNUSED, 0, 0, 0 },
	/* 0xCB */ { UNUSED, 0, 0, 0 },
	/* 0xCC */ { UNUSED, 0, 0, 0 },
	/* 0xCD */ { UNUSED, 0, 0, 0 },
	/* 0xCE */ { UNUSED, 0, 0, 0 },
	/* 0xCF */ { UNUSED, 0, 0, 0 },
	/* 0xD0 */ { UNUSED, 0, 0, 0 },
	/* 0xD1 */ { UNUSED, 0, 0, 0 },
	/* 0xD2 */ { UNUSED, 0, 0, 0 },
	/* 0xD3 */ { UNUSED, 0, 0, 0 },
	/* 0xD4 */ { UNUSED, 0, 0, 0 },
	/* 0xD5 */ { UNUSED, 0, 0, 0 },
	/* 0xD6 */ { UNUSED, 0, 0, 0 },
	/* 0xD7 */ { UNUSED, 0, 0, 0 },
	/* 0xD8 */ { UNUSED, 0, 0, 0 },
	/* 0xD9 */ { UNUSED, 0, 0, 0 },
	/* 0xDA */ { UNUSED, 0, 0, 0 },
	/* 0xDB */ { UNUSED, 0, 0, 0 },
	/* 0xDC */ { UNUSED, 0, 0, 0 },
	/* 0xDD */ { UNUSED, 0, 0, 0 },
	/* 0xDE */ { UNUSED, 0, 0, 0 },
	/* 0xDF */ { UNUSED, 0, 0, 0 },
	/* 0xE0 */ { UNUSED, 0, 0, 0 },
	/* 0xE1 */ { UNUSED, 0, 0, 0 },
	/* 0xE2 */ { UNUSED, 0, 0, 0 },
	/* 0xE3 */ { UNUSED, 0, 0, 0 },
	/* 0xE4 */ { UNUSED, 0, 0, 0 },
	/* 0xE5 */ { UNUSED, 0, 0, 0 },
	/* 0xE6 */ { UNUSED, 0, 0, 0 },
	/* 0xE7 */ { UNUSED, 0, 0, 0 },
	/* 0xE8 */ { UNUSED, 0, 0, 0 },
	/* 0xE9 */ { UNUSED, 0, 0, 0 },
	/* 0xEA */ { UNUSED, 0, 0, 0 },
	/* 0xEB */ { UNUSED, 0, 0, 0 },
	/* 0xEC */ { UNUSED, 0, 0, 0 },
	/* 0xED */ { UNUSED, 0, 0, 0 },
	/* 0xEE */ { UNUSED, 0, 0, 0 },
	/* 0xEF */ { UNUSED, 0, 0, 0 },
	/* 0xF0 */ { UNUSED, 0, 0, 0 },
	/* 0xF1 */ { UNUSED, 0, 0, 0 },
	/* 0xF2 */ { UNUSED, 0, 0, 0 },
	/* 0xF3 */ { UNUSED, 0, 0, 0 },
	/* 0xF4 */ { UNUSED, 0, 0, 0 },
	/* 0xF5 */ { UNUSED, 0, 0, 0 },
	/* 0xF6 */ { UNUSED, 0, 0, 0 },
	/* 0xF7 */ { UNUSED, 0, 0, 0 },
	/* 0xF8 */ { UNUSED, 0, 0, 0 },
	/* 0xF9 */ { UNUSED, 0, 0, 0 },
	/* 0xFA */ { UNUSED, 0, 0, 0 },
	/* 0xFB */ { UNUSED, 0, 0, 0 },
	/* 0xFC */ { UNUSED, 0, 0, 0 },
	/* 0xFD */ { UNUSED, 0, 0, 0 },
	/* 0xFE */ { UNUSED, 0, 0, 0 },
	/* 0xFF */ { RETURN, NO_ARG, 0, 0 }
    };

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
#ifdef SIZE_64
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
#endif /* SIZE_64 */
STATIC_INLINE double GetR32Arg (Byte_t *code)
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
#define GetRawArg	GetU64Arg
#else /* SIZE_32 */
#define GetRawArg	GetU32Arg
#endif

/* the size of a list cons cell in bytes */
#define CONS_SZB	(WORD_SZB*3)

/* the amount of free space that we want in the allocation arena; this value must be
 * less than MIN_ALLOC_SZB (defined in include/ml-limits.h)
 */
#define FREE_REQ_SZB    64*ONE_K

/* for backward compatibility */
ml_val_t BuildLiteralsV1 (ml_state_t *msp, Byte_t *lits, int pc, int len);

#ifdef DEBUG_LITERALS
#  define GC_MESSAGE	SayDebug("BuildLiterals: invoke GC\n");
#else
#  define GC_MESSAGE
#endif

/* BuildLiterals:
 *
 * NOTE: we allocate all of the objects in the first generation, and allocate
 * the vector of literals in the allocation space.
 */
ml_val_t BuildLiterals (ml_state_t *msp, Byte_t *code, int len)
{
    int		pc = 0;
#ifdef DEBUG_LITERALS
    int		depth = 0;
#endif
    Unsigned32_t magic, maxDepth, wordSz, maxSaved;
    ml_val_t	stk;
    ml_val_t	res;
    Int32_t	availSpace, spaceReq;
    Unsigned32_t ui;
  /* we represent the saved array using a mutable data array that is allocated in
   * the heap.  This means that we need to create store-list entries when we update
   * it.
   */
    ml_val_t	saved;

/* A check that the available space is sufficient for the literal object that
 * we are about to allocate.  Note that the cons cell has already been accounted
 * for in availSpace (but not in spaceReq).
 */
#define GC_CHECK										\
    do {											\
	if (spaceReq > availSpace) {								\
	    GC_MESSAGE										\
	    InvokeGCWithRoots (msp, 0, (ml_val_t *)&code, &stk, &saved, NIL(ml_val_t *));	\
	    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr) - CONS_SZB;	\
	}											\
	else											\
	    availSpace -= spaceReq;								\
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
    else if (magic != V2_MAGIC) {
	Die("bogus literal magic number %#x", magic);
    }
#ifdef DEBUG_LITERALS
        SayDebug("BuildLiterals: VERSION 2\n");
#endif

  /* get the rest of the V2 header */
    wordSz = GetU32Arg(code+pc); pc += 4;
    maxSaved = GetU32Arg(code+pc); pc += 4;

#ifdef SIZE_64
    if (wordSz != 64) {
	Die("expected word size = 64, but found %d\n", wordSz);
    }
#else /* SIZE_32 */
    if (wordSz != 32) {
	Die("expected word size = 32, but found %d\n", wordSz);
    }
#endif

    if (maxSaved > 0) {
	saved = ML_AllocArrayData (msp, maxSaved, ML_nil);
    }
    else {
	saved = ML_nil;
    }

    stk = ML_nil;
    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
#ifdef DEBUG_LITERALS
    SayDebug("BuildLiterals: avail = %d bytes; maxDepth = %d, maxSaved = %d\n",
	(int)availSpace, (int)maxDepth, (int)maxSaved);
#endif
    while (TRUE) {
	ASSERT(pc < len);
	ASSERT(availSpace <= (Int32_t)((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr));
	if (availSpace < 512 * WORD_SZB) {
	    if (NeedGC(msp, FREE_REQ_SZB)) {
	        GC_MESSAGE
		InvokeGCWithRoots (msp, 0, (ml_val_t *)&code, &stk, &saved, NIL(ml_val_t *));
	    }
	    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
	}
	availSpace -= CONS_SZB;	/* space for stack cons cell */

#ifdef DEBUG_LITERALS
	int startPC = pc;
#endif

#ifdef DEBUG_LITERALS
#  define PUSH(arg)	do { LIST_cons(msp, stk, (arg), stk); depth++; } while(0)
#else
#  define PUSH(arg)	LIST_cons(msp, stk, (arg), stk)
#endif

    /* get the next instruction */
        Byte_t opcode = code[pc++];

    /* get the argument (if any) */
	union {
	    Word_t uArg;
	    Int_t iArg;
	} arg;
        switch (InstrInfo[opcode].argKind) {
	  case NO_ARG:
	    break;
	  case IMMED_ARG:
	    arg.iArg = (Int_t)InstrInfo[opcode].immedArg;
	    break;
	  case I8_ARG:
	    arg.iArg = (Int_t)GetI8Arg(&(code[pc]));  pc += 1;
	    break;
	  case U8_ARG:
	    arg.uArg = (Word_t)GetU8Arg(&(code[pc]));  pc += 1;
	    break;
	  case I16_ARG:
	    arg.iArg = (Int_t)GetI16Arg(&(code[pc]));  pc += 2;
	    break;
	  case U16_ARG:
	    arg.uArg = (Word_t)GetU16Arg(&(code[pc]));  pc += 2;
	    break;
	  case I32_ARG:
	    arg.iArg = (Int_t)GetI32Arg(&(code[pc]));  pc += 4;
	    break;
	  case U32_ARG:
	    arg.uArg = (Word_t)GetU32Arg(&(code[pc]));  pc += 4;
	    break;
#ifdef SIZE_64
	  case I64_ARG:
	    arg.iArg = (Int_t)GetI64Arg(&(code[pc]));  pc += 8;
	    break;
#endif
	  case INT_ARG:
#ifdef SIZE_64
	    arg.iArg = (Int_t)GetI64Arg(&(code[pc]));  pc += 8;
#else /* SIZE_32 */
	    arg.iArg = (Int_t)GetI32Arg(&(code[pc]));  pc += 4;
#endif
	}

    /* handle the operation */
	switch (InstrInfo[opcode].oper) {
	  case UNUSED:
	    break;

	  case INT:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: INT(%" PRINT ")\n", startPC, depth, arg.iArg);
#endif
	    PUSH (INT_CtoML(arg.iArg));
	    break;

#ifdef SIZE_32
	  case INT32:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: INT32(%" PRINT ")\n", startPC, depth, arg.iArg);
#endif
	    res = INT32_CtoML(msp, arg.iArg);
	    PUSH (res);
	    availSpace -= 2*WORD_SZB;
	    break;
#endif

#ifdef SIZE_64
	  case INT64:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: INT64(" PRINT ")\n", startPC, depth, arg.iArg);
#endif
	    res = ML_AllocWord64(msp, arg.iArg);
	    PUSH (res);
	    availSpace -= 2*WORD_SZB;
	    break;
#endif

	  case BIGINT:
	    Die("BIGINT -- not supported yet");
	    break;

	  case IVEC:
	    Die("IVEC -- not supported yet");
	    break;

	  case IVEC8:
	    Die("IVEC8 -- not supported yet");
	    break;

	  case IVEC16:
	    Die("IVEC16 -- not supported yet");
	    break;

	  case IVEC32:
	    Die("IVEC32 -- not supported yet");
	    break;

	  case IVEC64:
	    Die("IVEC64 -- not supported yet");
	    break;

	  case REAL32:
	    Die("REAL32 -- not supported yet");
	    break;

	  case REAL64:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: REAL64(%f)\n", startPC, depth, GetR64Arg(&(code[pc])));
#endif
	    REAL64_ALLOC(msp, res, GetR64Arg(&(code[pc])));  pc += 8;
	    availSpace -= WORD_SZB + REALD_SZB;
#ifdef ALIGN_REALDS
	    availSpace -= WORD_SZB;
#endif
	    break;

	  case RVEC32:
	    Die("RVEC32 -- not supported yet");
	    break;

	  case RVEC64:
	    Die("RVEC64 -- not supported yet");
	    break;

	  case STR8:
#ifdef DEBUG_LITERALS
	SayDebug("[%04d/%4d]: STR8(%" PRWORD ") [...]", startPC, depth, arg.uArg);
#endif
	    if (arg.uArg == 0) {
#ifdef DEBUG_LITERALS
	SayDebug("\n");
#endif
		PUSH (ML_string0);
		break;
	    }
	    ui = BYTES_TO_WORDS(arg.uArg+1);  /* include space for '\0' */
	  /* the space request includes space for the data-object header word and
	   * the sequence header object.
	   */
	    spaceReq = WORD_SZB*(ui+1+3);
/* FIXME: for large strings, we should be allocating them in the 1st generation */
	    GC_CHECK;
	  /* allocate the data object */
	    ML_AllocWrite(msp, 0, MAKE_DESC(ui, DTAG_raw));
	    ML_AllocWrite (msp, ui, 0);  /* so word-by-word string equality works */
	    res = ML_Alloc (msp, ui);
#ifdef DEBUG_LITERALS
	SayDebug(" @ %p (%d words)\n", (void *)res, ui);
#endif
	    memcpy (PTR_MLtoC(void, res), &(code[pc]), arg.uArg); pc += arg.uArg;
	  /* allocate the header object */
	    SEQHDR_ALLOC(msp, res, DESC_string, res, arg.uArg);
	  /* push on stack */
	    PUSH (res);
	    availSpace -= spaceReq;
	    break;

	  case RECORD:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: RECORD(%" PRWORD ") [", startPC, depth, arg.uArg);
#endif
	    if (arg.uArg == 0) {
#ifdef DEBUG_LITERALS
	    SayDebug("]\n");
#endif
		PUSH (ML_unit);
		break;
	    }
	    else {
		spaceReq = WORD_SZB*(arg.uArg+1);
		GC_CHECK;
		ML_AllocWrite(msp, 0, MAKE_DESC(arg.uArg, DTAG_record));
	    }
	  /* top of stack is last element in record */
	    for (ui = arg.uArg;  ui > 0;  ui--) {
		ML_AllocWrite(msp, ui, LIST_hd(stk));
		stk = LIST_tl(stk);
#ifdef DEBUG_LITERALS
		depth--;
#endif
	    }
	    res = ML_Alloc(msp, arg.uArg);
#ifdef DEBUG_LITERALS
	    SayDebug("...] @ %p\n", (void *)res);
#endif
	    PUSH (res);
	    availSpace -= spaceReq;
	    break;

	  case VECTOR:
#ifdef DEBUG_LITERALS
	SayDebug("[%04d/%4d]: VECTOR(%" PRWORD ") [", startPC, depth, arg.uArg);
#endif
	    if (arg.uArg == 0) {
#ifdef DEBUG_LITERALS
	SayDebug("]\n");
#endif
		PUSH (ML_vector0);
		break;
	    }
	  /* the space request includes space for the data-object header word and
	   * the sequence header object.
	   */
	    spaceReq = WORD_SZB*(arg.uArg + (1 + 3));
/* FIXME: for large vectors, we should be allocating them in the 1st generation */
	    GC_CHECK;
	  /* allocate the data object */
	    ML_AllocWrite(msp, 0, MAKE_DESC(arg.uArg, DTAG_vec_data));
	  /* top of stack is last element in vector */
	    for (ui = arg.uArg;  ui > 0;  ui--) {
		ML_AllocWrite(msp, ui, LIST_hd(stk));
		stk = LIST_tl(stk);
#ifdef DEBUG_LITERALS
		depth--;
#endif
	    }
	    res = ML_Alloc(msp, arg.uArg);
	  /* allocate the header object */
	    SEQHDR_ALLOC(msp, res, DESC_polyvec, res, arg.uArg);
#ifdef DEBUG_LITERALS
	SayDebug("...] @ %p\n", (void *)res);
#endif
	    PUSH (res);
	    availSpace -= spaceReq;
	    break;

	  case RAW: /* Word_t sized raw values */
#ifdef DEBUG_LITERALS
	    {
		int i, n;
	        SayDebug("[%04d/%4d]: RAW(%" PRWORD ") [%02xn",
		    startPC, depth, arg.uArg, code[pc]);
	        n = (WORD_SZB*arg.uArg > 8) ? 8 : WORD_SZB*arg.uArg;
		for (i = 1;  i < n;  i++) {
		    SayDebug(" %02x", code[pc+i]);
		}
		if (n < WORD_SZB*arg.uArg) {
		    SayDebug(" ...]\n");
		} else {
		    SayDebug("]\n");
		}
	    }
#endif
	    ASSERT(arg.uArg > 0);
	    spaceReq = WORD_SZB*arg.uArg + WORD_SZB;
/* FIXME: for large objects, we should be allocating them in the 1st generation */
	    GC_CHECK;
	    ML_AllocWrite (msp, 0, MAKE_DESC(arg.uArg, DTAG_raw));
	    for (ui = 1;  ui <= arg.uArg;  ui++) {
		ML_AllocWrite (msp, ui, (ml_val_t)GetRawArg(&(code[pc])));  pc += WORD_SZB;
	    }
	    res = ML_Alloc (msp, arg.uArg);
	    PUSH (res);
	    availSpace -= spaceReq;
	    break;

	  case RAW32:
	    Die("RAW32 -- not supported yet");
	    break;

	  case RAW64:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: RAW64(%" PRWORD ") [...]\n", startPC, depth, arg.uArg);
#endif
	    ASSERT(arg.uArg > 0);
	    spaceReq = 8*(arg.uArg+1);
/* FIXME: for large objects, we should be allocating them in the 1st generation */
	    GC_CHECK;
#ifdef ALIGN_REALDS
	  /* Force REALD_SZB alignment (descriptor is off by one word) */
	    msp->ml_allocPtr = (ml_val_t *)((Addr_t)(msp->ml_allocPtr) | WORD_SZB);
#endif
	  /* ui is the number of words */
	    ui = WORD64_SZW * arg.uArg;
	    ML_AllocWrite (msp, 0, MAKE_DESC(ui, DTAG_raw64));
	    res = ML_Alloc (msp, ui);
	    for (ui = 0;  ui < arg.uArg;  ui++) {
		PTR_MLtoC(double, res)[ui] = GetR64Arg(&(code[pc]));
		pc += 8;
	    }
	    PUSH (res);
	    availSpace -= spaceReq;
	    break;

	  case CONCAT:
	    break;

	  case SAVE: {
		ml_val_t *loc;
#ifdef DEBUG_LITERALS
		SayDebug("[%04d/%4d]: SAVE(%" PRWORD ") %p\n",
		    startPC, depth, arg.uArg, (void*)LIST_hd(stk));
#endif
		ASSERT(saved != ML_nil);
		ASSERT(stk != ML_nil);
		loc = PTR_MLtoC(ml_val_t, saved) + arg.uArg;
		*loc = LIST_hd(stk);
		ML_RecordUpdate (msp, loc);
	    } break;

	  case LOAD:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: LOAD(%" PRWORD ") %p\n",
		startPC, depth, arg.uArg, PTR_MLtoC(ml_val_t *, saved)[arg.uArg]);
#endif
	    ASSERT(saved != ML_nil);
	    PUSH (PTR_MLtoC(ml_val_t, saved)[arg.uArg]);
	    break;

	  case RETURN:
#ifdef DEBUG_LITERALS
	    SayDebug("[%04d/%4d]: RETURN(%p); depth = %d\n",
		startPC, depth, (void *)LIST_hd(stk), depth);
#endif
	    ASSERT(pc == len);
	    ASSERT((stk != ML_nil) && (LIST_tl(stk) == ML_nil));
	    return (LIST_hd(stk));

	  default:
	    Die ("BuildLiterals: bogus literal opcode #%x @ %d", opcode, pc-1);
	} /* switch */
    } /* while */

} /* end of BuildLiterals */
