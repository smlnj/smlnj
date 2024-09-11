/*! \file ml-objects.c
 *
 * \author John Reppy
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code to allocate and manipulate ML objects.
 *
 * MP Note: when invoking the GC, we add the requested size to reqSizeB,
 * so that multiple processors can request space at the same time.
 */

#include "ml-base.h"
#include "heap.h"
#include "ml-objects.h"
#include "ml-limits.h"
#include <string.h>

/* A macro to check for necessary GC */
#define IFGC(ap, szb)	\
	if ((! isACTIVE(ap)) || (AVAIL_SPACE(ap) <= (szb)))

#ifdef COLLECT_STATS
#define COUNT_ALLOC(msp, nbytes)	{	\
	heap_t		*__h = msp->ml_heap;	\
	CNTR_INCR(&(__h->numAlloc), (nbytes));	\
    }
#else
#define COUNT_ALLOC(msp, nbytes)	/* null */
#endif


/* ML_CString:
 *
 * Allocate an ML string using a C string as an initializer.  We assume
 * that the string is small and can be allocated in the allocation
 * arena.
 */
ml_val_t ML_CString (ml_state_t *msp, const char *v)
{
    int		len = ((v == NIL(char *)) ? 0 : strlen(v));

    if (len == 0)
	return ML_string0;
    else {
	Word_t n = BYTES_TO_WORDS(len+1);  /* count "\0" too */
	ml_val_t res;

	res = ML_AllocRaw (msp, n);
      /* zero the last word to allow fast (word) string comparisons, and to
       * guarantee 0 termination.
       */
	PTR_MLtoC(Word_t, res)[n-1] = 0;
	strcpy (PTR_MLtoC(char, res), v);

	SEQHDR_ALLOC (msp, res, DESC_string, res, len);

	return res;
    }

} /* end of ML_CString */

/* ML_CStringList:
 *
 * Given a NIL terminated array of char *, build a list of ML strings.
 */
ml_val_t ML_CStringList (ml_state_t *msp, char **strs)
{
/** NOTE: we should do something about possible GC!!! **/
    int		i;
    ml_val_t	p, s;

    for (i = 0;  strs[i] != NIL(char *);  i++)
	continue;

    p = LIST_nil;
    while (i-- > 0) {
	s = ML_CString(msp, strs[i]);
	LIST_cons(msp, p, s, p);
    }

    return p;

} /* end of ML_CStringList */

/* ML_AllocString:
 *
 * Allocate an uninitialized ML string of length > 0.  This string is
 * guaranteed to be padded to word size with 0 bytes, and to be 0 terminated.
 */
ml_val_t ML_AllocString (ml_state_t *msp, Word_t len)
{
    Word_t	nwords = BYTES_TO_WORDS(len+1);
    ml_val_t	res;

    ASSERT(len > 0);

    res = ML_AllocRaw (msp, nwords);

  /* zero the last word to allow fast (word) string comparisons, and to
   * guarantee 0 termination.
   */
    PTR_MLtoC(Word_t, res)[nwords-1] = 0;

    SEQHDR_ALLOC (msp, res, DESC_string, res, len);

    return res;

} /* end of ML_AllocString. */

/* ML_AllocRaw:
 *
 * Allocate an uninitialized chunk of raw data.
 */
ml_val_t ML_AllocRaw (ml_state_t *msp, Word_t nwords)
{
    ml_val_t	desc = MAKE_DESC(nwords, DTAG_raw);
    ml_val_t	res;
    Word_t	szb;

    ASSERT(nwords > 0);

    if (nwords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];

	szb = WORD_SZB*(nwords + 1);
        IFGC (ap, szb+msp->ml_heap->allocSzB) {
          /* we need to do a GC */
            ap->reqSizeB += szb;
            InvokeGC (msp, 1);
            ap->reqSizeB = 0;
        }
        *(ap->nextw++) = desc;
        res = PTR_CtoML(ap->nextw);
        ap->nextw += nwords;
        ASSERT(ap->nextw < ap->tospTop);
	COUNT_ALLOC(msp, szb);
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, nwords);
    }

    return res;

} /* end of ML_AllocRaw. */

/* ML_ShrinkRaw:
 *
 * Shrink a freshly allocated raw-data vector.  This is used by the input routines
 * that must allocate space for input that may be excessive.
 */
void ML_ShrinkRaw (ml_state_t *msp, ml_val_t v, Word_t nWords)
{
    int		oldNWords = OBJ_LEN(v);

    if (nWords == oldNWords)
	return;

    ASSERT((nWords > 0) && (nWords < oldNWords));

    if (oldNWords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];
	ASSERT(ap->nextw - oldNWords == PTR_MLtoC(ml_val_t, v));
	ap->nextw -= (oldNWords - nWords);
    }
    else {
	ASSERT(msp->ml_allocPtr - oldNWords == PTR_MLtoC(ml_val_t, v));
	msp->ml_allocPtr -= (oldNWords - nWords);
    }

    PTR_MLtoC(ml_val_t, v)[-1] = MAKE_DESC(nWords, DTAG_raw);

} /* end of ML_ShrinkRaw */

/* ML_AllocRaw64:
 *
 * Allocate an uninitialized chunk of 64-bit aligned raw data.
 */
ml_val_t ML_AllocRaw64 (ml_state_t *msp, Word_t nelems)
{
    Word_t      nwords = DOUBLES_TO_WORDS(nelems);
    ml_val_t	desc = MAKE_DESC(nwords, DTAG_raw64);
    ml_val_t	res;
    Word_t	szb;

    if (nwords > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[STRING_INDX];
	szb = WORD_SZB*(nwords + 1);
#ifdef ALIGN_REALDS
	szb += WORD_SZB;  /* alignment padding */
#endif
        IFGC (ap, szb+msp->ml_heap->allocSzB) {
          /* we need to do a GC */
            ap->reqSizeB += szb;
            InvokeGC (msp, 1);
            ap->reqSizeB = 0;
        }
#ifdef ALIGN_REALDS
      /* Force REALD_SZB alignment (descriptor is off by one word) */
#  ifdef CHECK_HEAP
        if (((Addr_t)ap->nextw & WORD_SZB) == 0) {
            *(ap->nextw) = (ml_val_t)0;
            ap->nextw++;
        }
#  else
        ap->nextw = (ml_val_t *)(((Addr_t)ap->nextw) | WORD_SZB);
#  endif
#endif
        *(ap->nextw++) = desc;
        res = PTR_CtoML(ap->nextw);
        ap->nextw += nwords;
	COUNT_ALLOC(msp, szb);
    }
    else {
#ifdef ALIGN_REALDS
      /* Force REALD_SZB alignment */
	msp->ml_allocPtr = (ml_val_t *)((Addr_t)(msp->ml_allocPtr) | WORD_SZB);
#endif
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, nwords);
    }

    return res;

} /* end of ML_AllocRaw64 */

/* ML_AllocCode:
 *
 * Allocate an uninitialized ML code object.  Assume that len > 1.
 */
ml_val_t ML_AllocCode (ml_state_t *msp, Word_t len)
{
    heap_t	    *heap = msp->ml_heap;
    int		    allocGen = (heap->numGens < CODE_ALLOC_GEN)
			? heap->numGens
			: CODE_ALLOC_GEN;
    gen_t	    *gen = heap->gen[allocGen-1];
    bigobj_desc_t   *dp;

    dp = BO_Alloc (heap, allocGen, len);
    ASSERT(dp->gen == allocGen);
    dp->next = gen->bigObjs[CODE_INDX];
    gen->bigObjs[CODE_INDX] = dp;
    dp->objc = CODE_INDX;
    COUNT_ALLOC(msp, len);

    return PTR_CtoML(dp->obj);

} /* end of ML_AllocCode. */

/* ML_AllocBytearray:
 *
 * Allocate an uninitialized ML bytearray.  Assume that len > 0.
 */
ml_val_t ML_AllocBytearray (ml_state_t *msp, Word_t len)
{
    Word_t nwords = BYTES_TO_WORDS(len);
    ml_val_t res;

    res = ML_AllocRaw (msp, nwords);

  /* zero the last word to allow fast (word) string comparisons, and to
   * guarantee 0 termination.
   */
    PTR_MLtoC(Word_t, res)[nwords-1] = 0;

    SEQHDR_ALLOC (msp, res, DESC_word8arr, res, len);

    return res;

} /* end of ML_AllocBytearray. */

/* ML_AllocRealdarray:
 *
 * Allocate an uninitialized ML realarray.  Assume that len > 0.
 */
ml_val_t ML_AllocRealdarray (ml_state_t *msp, Word_t len)
{
    ml_val_t	res;

    res = ML_AllocRaw64 (msp, len);

    SEQHDR_ALLOC (msp, res, DESC_real64arr, res, len);

    return res;

} /* end of ML_AllocRealdarray. */

/* ML_AllocArrayData:
 *
 * Allocate a mutable data array using initVal as an initial value.  Assume
 * that len > 0.
 */
ml_val_t ML_AllocArrayData (ml_state_t *msp, Word_t len, ml_val_t initVal)
{
    ml_val_t	res, *p;
    ml_val_t	desc = MAKE_DESC(len, DTAG_arr_data);
    int		i;
    Word_t	szb;

    if (len > SMALL_OBJ_SZW) {
	arena_t	*ap = msp->ml_heap->gen[0]->arena[ARRAY_INDX];
	int	gcLevel = (isBOXED(initVal) ? 0 : -1);

	szb = WORD_SZB*(len + 1);
        if (! isACTIVE(ap)
        || (AVAIL_SPACE(ap) <= szb+msp->ml_heap->allocSzB))
            gcLevel = 1;
        if (gcLevel >= 0) {
          /* we need to do a GC (and preserve initVal) */
            ml_val_t	root = initVal;
            ap->reqSizeB += szb;
            InvokeGCWithRoots (msp, gcLevel, &root, NIL(ml_val_t *));
            initVal = root;
            ap->reqSizeB = 0;
        }
        ASSERT(ap->nextw == ap->sweep_nextw);
        *(ap->nextw++) = desc;
        res = PTR_CtoML(ap->nextw);
        ap->nextw += len;
        ap->sweep_nextw = ap->nextw;
	COUNT_ALLOC(msp, szb);
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, len);
    }

    for (p = PTR_MLtoC(ml_val_t, res), i = 0;  i < len; i++) {
	*p++ = initVal;
    }

    return res;

} /* end of ML_AllocArrayData. */

/* ML_AllocArray:
 *
 * Allocate an ML array using initVal as an initial value.  Assume
 * that len > 0.
 */
ml_val_t ML_AllocArray (ml_state_t *msp, Word_t len, ml_val_t initVal)
{
    ml_val_t	res;

    res = ML_AllocArrayData (msp, len, initVal);

    SEQHDR_ALLOC (msp, res, DESC_polyarr, res, len);

    return res;

} /* end of ML_AllocArray. */

/* ML_AllocVector:
 *
 * Allocate an ML vector, using the list initVal as an initializer.
 * Assume that len > 0.
 */
ml_val_t ML_AllocVector (ml_state_t *msp, Word_t len, ml_val_t initVal)
{
    ml_val_t	desc = MAKE_DESC(len, DTAG_vec_data);
    ml_val_t	res, *p;

    if (len > SMALL_OBJ_SZW) {
      /* Since we want to avoid pointers from the 1st generation record space
       * into the allocation space, we need to do a GC (and preserve initVal)
       */
	arena_t		*ap = msp->ml_heap->gen[0]->arena[RECORD_INDX];
	ml_val_t	root = initVal;
	int		gcLevel = 0;
	Word_t		szb;

	szb = WORD_SZB*(len + 1);
        if (! isACTIVE(ap)
        || (AVAIL_SPACE(ap) <= szb+msp->ml_heap->allocSzB))
            gcLevel = 1;
        ap->reqSizeB += szb;
        InvokeGCWithRoots (msp, gcLevel, &root, NIL(ml_val_t *));
        initVal = root;
        ap->reqSizeB = 0;
        ASSERT(ap->nextw == ap->sweep_nextw);
        *(ap->nextw++) = desc;
        res = PTR_CtoML(ap->nextw);
        ap->nextw += len;
        ap->sweep_nextw = ap->nextw;
	COUNT_ALLOC(msp, szb);
    }
    else {
	ML_AllocWrite (msp, 0, desc);
	res = ML_Alloc (msp, len);
    }

    for (
	p = PTR_MLtoC(ml_val_t, res);
	initVal != LIST_nil;
	initVal = LIST_tl(initVal)
    )
	*p++ = LIST_hd(initVal);

    SEQHDR_ALLOC (msp, res, DESC_polyvec, res, len);

    return res;

} /* end of ML_AllocVector. */


/* ML_SysConst:
 *
 * Find the system constant with the given id in tbl, and allocate a pair
 * to represent it.  If the constant is not present, then return the
 * pair (~1, "<UNKNOWN>").
 */
ml_val_t ML_SysConst (ml_state_t *msp, sysconst_tbl_t *tbl, int id)
{
    ml_val_t	name, res;
    int		i;

    for (i = 0;  i < tbl->numConsts;  i++) {
	if (tbl->consts[i].id == id) {
	    name = ML_CString (msp, tbl->consts[i].name);
	    REC_ALLOC2 (msp, res, INT_CtoML(id), name);
	    return res;
	}
    }
  /* here, we did not find the constant */
    name = ML_CString (msp, "<UNKNOWN>");
    REC_ALLOC2 (msp, res, INT_CtoML(-1), name);
    return res;

} /* end of ML_SysConst */


/* ML_SysConstList:
 *
 * Generate a list of system constants from the given table.
 */
ml_val_t ML_SysConstList (ml_state_t *msp, sysconst_tbl_t *tbl)
{
    int		i;
    ml_val_t	name, sysConst, list;
    Addr_t	availSpace, reqSpace;

    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
    for (list = LIST_nil, i = tbl->numConsts;  --i >= 0;  ) {
      /* required space for string header+data (4 words + string bytes), pair (3 words),
       * cons (3 words).
       */
	reqSpace = (4 + 3 + 3) * WORD_SZB + BYTES_TO_WORDS(strlen(tbl->consts[i].name) + 1);
	if (reqSpace >= availSpace) {
	    InvokeGCWithRoots (msp, 0, (ml_val_t *)&list, NIL(ml_val_t *));
	    availSpace = ((size_t)msp->ml_limitPtr - (size_t)msp->ml_allocPtr);
	}
	name = ML_CString (msp, tbl->consts[i].name);
	REC_ALLOC2 (msp, sysConst, INT_CtoML(tbl->consts[i].id), name);
	LIST_cons(msp, list, sysConst, list);
	availSpace -= reqSpace;
    }

    return list;

} /* end of ML_SysConstList */


/* ML_AllocCData:
 *
 * Allocate a 64-bit aligned raw data object (to store abstract C data).
 */
ml_val_t ML_AllocCData (ml_state_t *msp, Word_t nbytes)
{
    ml_val_t	obj;

    obj = ML_AllocRaw64 (msp, (nbytes+7) >> 3);

    return obj;

} /* end of ML_AllocCData */


/* ML_CData:
 *
 * Allocate a 64-bit aligned raw data object and initialize it to the given C data.
 */
ml_val_t ML_CData (ml_state_t *msp, void *data, Word_t nbytes)
{
    ml_val_t	obj;

    if (nbytes == 0)
	return ML_unit;
    else {
	obj = ML_AllocRaw64 (msp, (nbytes+7) >> 3);
	memcpy (PTR_MLtoC(void, obj), data, nbytes);

	return obj;
    }

} /* end of ML_CData */
