/*! \file old-literals.c
 *
 * This is the version 1 literals builder.  We keep it around to ease
 * the transition to the new scheme, but it can be removed from the runtime
 * after the compiler switches over to the new scheme.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "heap.h"
#include <string.h>

/* Codes for literal machine instructions (version 1):
 *   INT(i)		0x01 <i>
 *   RAW32[i]		0x02 <i>
 *   RAW32[i1,..,in]	0x03 <n> <i1> ... <in>
 *   RAW64[r]		0x04 <r>
 *   RAW64[r1,..,rn]	0x05 <n> <r1> ... <rn>
 *   STR[c1,..,cn]	0x06 <n> <c1> ... <cn>
 *   LIT(k)		0x07 <k>		-- push stk[k] (for sharing)
 *   VECTOR(n)		0x08 <n>
 *   RECORD(n)		0x09 <n>
 *   RETURN		0xff
 */
#define I_INT		0x01
#define I_RAW32		0x2
#define I_RAW32L	0x3
#define I_RAW64		0x4
#define I_RAW64L	0x5
#define I_STR		0x6
#define I_LIT		0x7
#define I_VECTOR	0x8
#define I_RECORD	0x9
#define I_RETURN	0xff

#define _B0(p)		((p)[pc])
#define _B1(p)		((p)[pc+1])
#define _B2(p)		((p)[pc+2])
#define _B3(p)		((p)[pc+3])

#define GET32(p)	\
    ((_B0(p) << 24) | (_B1(p) << 16) | (_B2(p) << 8) | _B3(p))

/* the size of a list cons cell in bytes */
#define CONS_SZB	(WORD_SZB*3)

/* the amount of free space that we want in the allocation arena; this value must be
 * less than MIN_ALLOC_SZB (defined in include/ml-limits.h)
 */
#define FREE_REQ_SZB    64*ONE_K

/* GetDouble:
 */
PVT double GetDouble (Byte_t *p)
{
    int		i;
    union {
	double		d;
	Byte_t		b[sizeof(double)];
    }		u;

#ifdef BYTE_ORDER_LITTLE
    for (i = sizeof(double)-1;  i >= 0;  i--) {
	u.b[i] = *p++;
    }
#else
    for (i = 0;  i < sizeof(double);  i++) {
	u.b[i] = p[i];
    }
#endif

    return u.d;

} /* end of GetDouble */

/* OldLiterals:
 *
 * The Version 1 build literals function.  We assume that the header has already
 * been consumed in the version check (see build-literals.c), which means that the
 * `pc` argument should point to the first command.
 */
ml_val_t BuildLiteralsV1 (ml_state_t *msp, Byte_t *lits, int pc, int len)
{
    ml_val_t	stk, res;
    Int32_t	i, j, n;
    Int32_t	availSpace, spaceReq;
    double	d;

#ifdef DEBUG_LITERALS
    SayDebug("BuildLiteralsV1: lits = %p, len = %d\n", (void *)lits, len);
#endif
    if (len <= 0) return ML_nil;

/* A check that the available space is sufficient for the literal object that
 * we are about to allocate.  Note that the cons cell has already been accounted
 * for in availSpace (but not in spaceReq).
 */
#define GC_CHECK									\
    do {										\
	if (spaceReq > availSpace) {							\
	    InvokeGCWithRoots (msp, 0, (ml_val_t *)&lits, &stk, NIL(ml_val_t *));	\
	    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr) - CONS_SZB; \
	}										\
    } while (0)

    stk = ML_nil;
    while (TRUE) {
	ASSERT(pc < len);
      /* ensure that there is at least 1Kb of available space -- enough for fixed-size
       * objects.
       */
        availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
	if (availSpace < ONE_K) {
	    if (NeedGC(msp, FREE_REQ_SZB))
		InvokeGCWithRoots (msp, 0, (ml_val_t *)&lits, &stk, NIL(ml_val_t *));
	    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
	}
	switch (lits[pc++]) {
	  case I_INT:
	    i = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: INT(%d)\n", pc-5, i);
#endif
	    LIST_cons(msp, stk, INT_CtoML(i), stk);
	    break;
	  case I_RAW32:
	    i = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: RAW32[%d]\n", pc-5, i);
#endif
	    res = INT32_CtoML(msp, i);
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_RAW32L:
	    n = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: RAW32L(%d) [...]\n", pc-5, n);
#endif
	    ASSERT(n > 0);
	    spaceReq = CONS_SZB + WORD_SZB + 4 * n;
	    ASSERT((spaceReq & (WORD_SZB-1)) == 0);
/* FIXME: for large objects, we should be allocating them in the 1st generation */
	    GC_CHECK;
	    ML_AllocWrite (msp, 0, MAKE_DESC(n, DTAG_raw));
	    for (j = WORD_SZB/4;  j <= n;  j++) {
		i = GET32(lits); pc += 4;
		ML_AllocWrite32 (msp, j, i);
	    }
	    res = ML_Alloc (msp, n);
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_RAW64:
	    d = GetDouble(&(lits[pc]));  pc += 8;
	    REAL64_ALLOC(msp, res, d);
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: RAW64[%f] @ %#x\n", pc-5, d, res);
#endif
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_RAW64L:
	    n = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: RAW64L(%d) [...]\n", pc-5, n);
#endif
	    ASSERT(n > 0);
	  /* space request includes extra padding word */
	    spaceReq = CONS_SZB + 2 * WORD_SZB + 8 * n;
/* FIXME: for large objects, we should be allocating them in the 1st generation */
	    GC_CHECK;
#ifdef ALIGN_REALDS
	  /* Force REALD_SZB alignment (descriptor is off by one word) */
	    msp->ml_allocPtr = (ml_val_t *)((Addr_t)(msp->ml_allocPtr) | WORD_SZB);
#endif
	    j = 2*n; /* number of words */
	    ML_AllocWrite (msp, 0, MAKE_DESC(j, DTAG_raw64));
	    res = ML_Alloc (msp, j);
	    for (j = 0;  j < n;  j++) {
		PTR_MLtoC(double, res)[j] = GetDouble(&(lits[pc]));  pc += 8;
	    }
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_STR:
	    n = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: STR(%d) [...]", pc-5, n);
#endif
	    if (n == 0) {
#ifdef DEBUG_LITERALS
SayDebug("\n");
#endif
		LIST_cons(msp, stk, ML_string0, stk);
		break;
	    }
	    j = BYTES_TO_WORDS(n+1);  /* include space for '\0' */
	  /* the space request includes space for the data-object header word and
	   * the sequence header object.
	   */
	    spaceReq = WORD_SZB*(j+1+3);
/* FIXME: for large strings, we should be allocating them in the 1st generation */
	    GC_CHECK;
	  /* allocate the data object */
	    ML_AllocWrite(msp, 0, MAKE_DESC(j, DTAG_raw));
	    ML_AllocWrite (msp, j, 0);  /* so word-by-word string equality works */
	    res = ML_Alloc (msp, j);
#ifdef DEBUG_LITERALS
SayDebug(" @ %p (%d words)\n", (void *)res, j);
#endif
	    memcpy (PTR_MLtoC(void, res), &lits[pc], n); pc += n;
	  /* allocate the header object */
	    SEQHDR_ALLOC(msp, res, DESC_string, res, n);
	  /* push on stack */
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_LIT:
	    n = GET32(lits); pc += 4;
	    for (j = 0, res = stk;  j < n;  j++) {
		res = LIST_tl(res);
	    }
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: LIT(%d) = %p\n", pc-5, n, (void *)LIST_hd(res));
#endif
	    LIST_cons(msp, stk, LIST_hd(res), stk);
	    break;
	  case I_VECTOR:
	    n = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: VECTOR(%d) [", pc-5, n);
#endif
	    if (n == 0) {
#ifdef DEBUG_LITERALS
SayDebug("]\n");
#endif
		LIST_cons(msp, stk, ML_vector0, stk);
		break;
	    }
	  /* the space request includes space for the data-object header word and
	   * the sequence header object.
	   */
	    spaceReq = WORD_SZB*(n+1+3);
/* FIXME: for large vectors, we should be allocating them in the 1st generation */
	    GC_CHECK;
	  /* allocate the data object */
	    ML_AllocWrite(msp, 0, MAKE_DESC(n, DTAG_vec_data));
	  /* top of stack is last element in vector */
	    for (j = n;  j > 0;  j--) {
		ML_AllocWrite(msp, j, LIST_hd(stk));
		stk = LIST_tl(stk);
	    }
	    res = ML_Alloc(msp, n);
	  /* allocate the header object */
	    SEQHDR_ALLOC(msp, res, DESC_polyvec, res, n);
#ifdef DEBUG_LITERALS
SayDebug("...] @ %p\n", (void *)res);
#endif
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_RECORD:
	    n = GET32(lits); pc += 4;
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: RECORD(%d) [", pc-5, n);
#endif
	    if (n == 0) {
#ifdef DEBUG_LITERALS
SayDebug("]\n");
#endif
		LIST_cons(msp, stk, ML_unit, stk);
		break;
	    }
	    else {
		spaceReq = WORD_SZB*(n+1);
		GC_CHECK;
		ML_AllocWrite(msp, 0, MAKE_DESC(n, DTAG_record));
	    }
	  /* top of stack is the last element in the record */
	    for (j = n;  j > 0;  j--) {
		ML_AllocWrite(msp, j, LIST_hd(stk));
		stk = LIST_tl(stk);
	    }
	    res = ML_Alloc(msp, n);
#ifdef DEBUG_LITERALS
SayDebug("...] @ %p\n", (void *)res);
#endif
	    LIST_cons(msp, stk, res, stk);
	    break;
	  case I_RETURN:
	    ASSERT(pc == len);
#ifdef DEBUG_LITERALS
SayDebug("[%2d]: RETURN(%p)\n", pc-5, (void *)LIST_hd(stk));
#endif
	    return (LIST_hd(stk));
	    break;
	  default:
	    Die ("bogus literal opcode #%x @ %d", lits[pc-1], pc-1);
	} /* switch */
    } /* while */

} /* end of BuildLiteralsV1 */

