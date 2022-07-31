/* automatically generates Sizes.sml */

#include <stdio.h>

typedef struct { char c; } min_struct;
typedef union { char c; } min_union;
typedef long long longlong;
typedef long double longdouble;
typedef char* pointer;

#define OFFSET(ty)						\
    {								\
	struct {						\
	    char a;						\
	    ty   b;						\
	} x;							\
	printf("  ");			                        \
	printf(#ty " = {bits = %d, align = %d},\n",			\
	    sizeof(ty)*8,						\
	    ((unsigned long)&(x.b) - (unsigned long)&(x.a))*8);	\
    }

main ()
{
    printf("(* This file was automatically generated using size.c.\n");
    printf(" * It contains information about c data sizes and layout.\n\n");
    printf(" * Limitations:\n");
    printf(" *   1. write proper test for bitFieldAlignment.\n");
    printf(" *   2. include date and system information in this file?\n");
    printf(" *)\n\n");
    printf("val sizes = {    (*** all sizes in bits ***)\n");
    OFFSET(char)
    OFFSET(short)
    OFFSET(int)
    OFFSET(long)
    OFFSET(longlong)
    OFFSET(float)
    OFFSET(double)
    OFFSET(longdouble)
    OFFSET(pointer)
    OFFSET(min_struct)
    OFFSET(min_union)
    printf("  onlyPackBitFields = false,\n");
    printf("  ignoreUnnamedBitFieldAlignment = true\n}\n");
}
