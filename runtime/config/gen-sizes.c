/*! \file gen-sizes.c
 *
 * \author John Reppy
 *
 * This program generates the "ml-sizes.h" header file; this file is
 * usable in both C and assembly files.
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include <stdlib.h>
#include <stdio.h>
#include "gen.h"

#define NIL(ty)	((ty)0)

#if SIZE_64
#  define WORD_SZB	8
#else
#  define WORD_SZB	4
#endif
#  define ADDR_SZB	sizeof(char *)

static union {
    char	    bytes[sizeof(unsigned long)];
    unsigned long   l;
} U;

int ilog2 (int x)
{
    int		i, j;

    for (i = 1, j = 2;  j <= x;  i++, j += j)
	continue;

    return i-1;

} /* end of ilog2 */

int main (void)
{
    char	*i16, *i32, *i64;
    FILE	*f;

    i16 = i32 = i64 = NIL(char *);

    if (sizeof(short) == 2) {
	i16 = "short";
    }
    if (sizeof(int) == 4) {
	i32 = "int";
    } else if (sizeof(long) == 4) {
	i32 = "long";
    }
    if (sizeof(long) == 8) {
	i64 = "long";
    } else if (sizeof(long long) == 8) {
	i64 = "long long";
    }

    if (i16 == NIL(char *)) {
	fprintf(stderr, "gen-sizes: Error -- no 16-bit integer type\n");
	exit (1);
    }
    if (i32 == NIL(char *)) {
	fprintf(stderr, "gen-sizes: Error -- no 32-bit integer type\n");
	exit (1);
    }
    if (i64 == NIL(char *)) {
	fprintf(stderr, "gen-sizes: Error -- no 64-bit integer type\n");
	exit (1);
    }

    f = OpenFile ("ml-sizes.h", "_ML_SIZES_");

    fprintf (f, "#define WORD_SZB           %d\n", (int)WORD_SZB);
    fprintf (f, "#define ADDR_SZB           %d\n", (int)ADDR_SZB);
    fprintf (f, "#define REALD_SZB          %d\n", (int)sizeof(double));
    fprintf (f, "#define BITS_PER_WORD      %d\n", 8*WORD_SZB);
    fprintf (f, "#define LOG_BITS_PER_WORD  %d\n", ilog2(8*WORD_SZB));
    fprintf (f, "#define LOG_BYTES_PER_WORD %d\n", ilog2(WORD_SZB));
    fprintf (f, "\n");

    U.bytes[0] = 0x01;
    U.bytes[sizeof(unsigned long)-1] = 0x02;
    switch (U.l & 0xFF) {
      case 0x01:
	fprintf(f, "#define BYTE_ORDER_LITTLE\n");
	break;
      case 0x02:
	fprintf(f, "#define BYTE_ORDER_BIG\n");
	break;
      default:
	fprintf(stderr, "gen-sizes: Error -- unable to determine endianess\n");
	exit(1);
    } /* end of switch */
    fprintf (f, "\n");

  /* the C part */
    fprintf (f, "#ifndef _ASM_\n");

    fprintf (f, "typedef %s Int16_t;\n", i16);
    fprintf (f, "typedef unsigned %s Unsigned16_t;\n", i16);
    fprintf (f, "typedef %s Int32_t;\n", i32);
    fprintf (f, "typedef unsigned %s Unsigned32_t;\n", i32);
    fprintf (f, "typedef %s Int64_t;\n", i64);
    fprintf (f, "typedef unsigned %s Unsigned64_t;\n", i64);
    fprintf (f, "\n");
    fprintf (f, "typedef unsigned char Byte_t;\n");
#if SIZE_64
    fprintf (f, "typedef Unsigned64_t Word_t;\n");
    fprintf (f, "typedef Int64_t      Int_t;\n");
    fprintf (f, "typedef Unsigned64_t Addr_t;\n");
#else /* SIZE_32 */
    fprintf (f, "typedef Unsigned32_t Word_t;\n");
    fprintf (f, "typedef Int32_t      Int_t;\n");
    fprintf (f, "typedef Unsigned32_t Addr_t;\n");
#endif

    fprintf (f, "#endif\n");

    CloseFile (f, "_ML_SIZES_");

    exit (0);
}
