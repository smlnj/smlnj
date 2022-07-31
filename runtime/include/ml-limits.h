/*! \file ml-limits.h
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Various limits and default settings for the SML/NJ run-time system.
 */

#ifndef _ML_LIMITS_
#define _ML_LIMITS_

#include "ml-base.h"

/* default image: NULL (means: try to find in-core image using dlopen/dlsym) */
#ifndef DFLT_IMAGE
#define DFLT_IMAGE		NULL
#endif

/* the maximum length of a boot-file pathname */
#ifndef MAX_BOOT_PATH_LEN
#  define MAX_BOOT_PATH_LEN	512
#endif

/* the maximum number of boot files */
#ifndef MAX_NUM_BOOT_FILES
#  define MAX_NUM_BOOT_FILES	1024
#endif

/** Default heap sizes **/
#ifndef DFLT_NGENS
#  define DFLT_NGENS	5
#endif
#define MAX_NGENS	14		    /* should agree with MAX_NUM_GENS in */
					    /* arena-id.h. */
#define DFLT_CACHE_GEN	2		    /* Cache from-space for gens 1 & 2 */
#ifndef DFLT_ALLOC
#  define DFLT_ALLOC	(512*ONE_K)
#endif
#define MIN_ALLOC_SZB	(128*ONE_K)
#ifdef OLD_POLICY
#define RATIO_UNIT	16		    /* ratios are measured in 1/16ths */
#define DFLT_RATIO1	(7*(RATIO_UNIT/2))  /* gen-1 arenas are small */
#define DFLT_RATIO	(3*RATIO_UNIT)
#define MAX_SZ1(NSZ)	(5*(NSZ))
#endif
#define DFLT_RATIO1	20
#define DFLT_RATIO2	10
#define DFLT_RATIO	5
#define MAX_SZ1(NSZ)	(6*(NSZ))

/* the generation to allocate code objects in */
#define CODE_ALLOC_GEN	2

/* the size (in words) of a "small object."  The C allocation routines allocate
 * small objects in the allocation space, while large objects are allocated
 * in the first generation.
 */
#define SMALL_OBJ_SZW	512

/* This is the size (in bytes) of the allocation buffer.  If A is the value
 * of the limit pointer, then A[HEAP_BUF_SZ-1] is the address of the next
 * store-vector location.
 */
#define HEAP_BUF_SZ	(1024 + 128)
#define HEAP_BUF_SZB	(HEAP_BUF_SZ*WORD_SZB)

/* the number of callee-save misc registers */
#define CALLEESAVE	3

/* the number of SML roots; these are the live registers across a GC invocation
 * and include stdarg, stdcont, stdclos, exncont, varptr, callee saves, linkreg,
 * and the pc register.
 */
#define NUM_SML_ROOTS   (7 + CALLEESAVE)

/* The maximum number of global C variables that can be roots. */
#define  MAX_C_ROOTS	8

/* maximum number of additional roots that can be passed to GC */
#define NUM_EXTRA_ROOTS 16

/* The number of potential GC roots. This includes space for C global roots,
 * ML roots, and the terminating null pointer.
 */
#define NUM_GC_ROOTS	(NUM_SML_ROOTS + MAX_C_ROOTS + NUM_EXTRA_ROOTS)

#endif /* !_ML_LIMITS_ */
