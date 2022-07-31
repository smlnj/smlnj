/*! \file minor-gc.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the code for doing minor collections (i.e., collecting the
 * allocation arena).
 */

#include "ml-base.h"
#include "ml-limits.h"
#include "ml-state.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "card-map.h"
#include "heap.h"
#include "tags.h"
#include "copy-loop.h"

#ifdef GC_STATS
extern long	numUpdates;
extern long	numBytesAlloc;
extern long	numBytesCopied;
#endif

/** store list operations */
#define STL_nil		ML_unit
#define STL_hd(p)	REC_SELPTR(ml_val_t, p, 0)
#define STL_tl(p)	REC_SEL(p, 1)

/* local routines */
PVT void MinorGC_ScanStoreList (heap_t *heap, ml_val_t stl);
PVT void MinorGC_SweepToSpace (gen_t *gen1);
PVT ml_val_t MinorGC_ForwardObj (gen_t *gen1, ml_val_t v);
PVT ml_val_t MinorGC_FwdSpecial (gen_t *gen1, ml_val_t *obj, ml_val_t desc);

#ifdef VERBOSE
extern char	*ArenaName[];
#endif

/* Check a word for a allocation space reference */
#ifndef NO_GC_INLINE
#define MinorGC_CheckWord(allocBase, allocSz, g1, p)	{			\
	ml_val_t	__w = *(p);						\
	if (isBOXED(__w) && (((Addr_t)__w - (allocBase)) < (allocSz))) {	\
	    *(p) = MinorGC_ForwardObj(g1, __w);					\
	}									\
    }
#else
PVT void MinorGC_CheckWord (Addr_t allocBase, Addr_t allocSz, gen_t *g1, ml_val_t *p)
{
    ml_val_t	w = *(p);
    if (isBOXED(w) && (((Addr_t)w - allocBase) < allocSz)) {
	ASSERT(ADDR_TO_PAGEID(BIBOP, w) == AID_NEW);
	*(p) = MinorGC_ForwardObj(g1, w);
    }
}
#endif


/* MinorGC:
 *
 * Do a collection of the allocation space.
 */
void MinorGC (ml_state_t *msp, ml_val_t **roots)
{
    heap_t	*heap = msp->ml_heap;
    gen_t	*gen1 = heap->gen[0];
#ifdef GC_STATS
    long	nbytesAlloc, nbytesCopied, nUpdates=numUpdates;
    Addr_t	gen1Top[NUM_ARENAS];
    int		i;
    {
	nbytesAlloc = (Addr_t)(msp->ml_allocPtr) - (Addr_t)(heap->allocBase);
	CNTR_INCR(&(heap->numAlloc), nbytesAlloc);
	for (i = 0;  i < NUM_ARENAS;  i++)
	    gen1Top[i] = (Addr_t)(gen1->arena[i]->nextw);
    }
#elif defined(VM_STATS)
    {
	Addr_t	    nbytesAlloc;
	nbytesAlloc = ((Addr_t)(msp->ml_allocPtr) - (Addr_t)(heap->allocBase));
	CNTR_INCR(&(heap->numAlloc), nbytesAlloc);
    }
#endif

#ifdef VERBOSE
{
  int i;
  SayDebug ("Generation 1 before MinorGC:\n");
  for (i = 0;  i < NUM_ARENAS;  i++) {
    SayDebug ("  %s: base = %p, oldTop = %p, nextw = %p\n",
      ArenaName[i+1], gen1->arena[i]->tospBase,
      gen1->arena[i]->oldTop, gen1->arena[i]->nextw);
  }
}
#endif

#ifdef CHECK_HEAP
    CheckBIBOP (heap);
#endif

  /* scan the standard roots */
    {
	ml_val_t	*rp;
	Addr_t		allocBase = (Addr_t)heap->allocBase;
	Addr_t		allocSz = heap->allocSzB;

	while ((rp = *roots++) != NIL(ml_val_t *)) {
	    MinorGC_CheckWord(allocBase, allocSz, gen1, rp);
	}
    }

  /* Scan the store list */
    {
	ml_val_t	stl = msp->ml_storePtr;
	if (stl != STL_nil) {
	    MinorGC_ScanStoreList (heap, stl);
	    msp->ml_storePtr = STL_nil;
	}
    }

  /* Sweep the first generation to-space */
    MinorGC_SweepToSpace (gen1);
    heap->numMinorGCs++;

  /* Handle weak pointers */
    if (heap->weakList != NIL(ml_val_t *))
	ScanWeakPtrs (heap);

#ifdef VERBOSE
{
  int i;
  SayDebug ("Generation 1 after MinorGC:\n");
  for (i = 0;  i < NUM_ARENAS;  i++) {
    SayDebug ("  %s: base = %p, oldTop = %p, nextw = %p\n",
      ArenaName[i+1], gen1->arena[i]->tospBase,
      gen1->arena[i]->oldTop, gen1->arena[i]->nextw);
  }
}
#endif

#ifdef GC_STATS
    {
	int	nbytes;

	nbytesCopied = 0;
	for (i = 0;  i < NUM_ARENAS;  i++) {
	    nbytes = ((Word_t)(gen1->arena[i]->nextw) - gen1Top[i]);
	    nbytesCopied += nbytes;
	    CNTR_INCR(&(heap->numCopied[0][i]), nbytes);
	}
    }
#endif

#ifdef CHECK_HEAP
    CheckHeap(heap, 1);
#endif

} /* end of MinorGC. */


/* MinorGC_ScanStoreList:
 *
 * Scan the store list.  The store list pointer (stl) is guaranteed to
 * be non-null.
 */
PVT void MinorGC_ScanStoreList (heap_t *heap, ml_val_t stl)
{
    ml_val_t	*addr, w;
    gen_t	*gen1 = heap->gen[0];
    bibop_t	bibop = BIBOP;
#ifdef GC_STATS
    int		nUpdates = 0;
#endif

  /* Scan the store list */
    do {
#ifdef GC_STATS
	nUpdates++;
#endif
	addr = STL_hd(stl);
	stl = STL_tl(stl);
	w = *addr;
	if (isBOXED(w)) {
	    aid_t	srcId = ADDR_TO_PAGEID(bibop, addr);
	  /* We can ignore updates to objects in new-space, and to references
	   * in the runtime system references (ie, UNMAPPED)
	   */
	    if ((srcId != AID_NEW) && (! isUNMAPPED(srcId))) {
	      /* srcGen is the generation of the updated cell; dstGen is the
	       * generation of the object that the cell points to.
	       */
		int	srcGen = EXTRACT_GEN(srcId);
		aid_t	dstId = ADDR_TO_PAGEID(bibop, w);
		int	dstGen = EXTRACT_GEN(dstId);

		if (IS_BIGOBJ_AID(dstId)) {
		    int		    i;
		    bigobj_region_t *region;
		    bigobj_desc_t   *dp;
		    if (dstGen >= srcGen)
			continue;
		  /* find the beginning of the region containing the code object */
		    i = BIBOP_ADDR_TO_INDEX(w);
		    while (! BO_IS_HDR(dstId)) {
			--i;
			dstId = INDEX_TO_PAGEID(bibop, i);
		    }
		    region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);
		    dp = ADDR_TO_BODESC(region, w);
		    dstGen = dp->gen;
		}
		else {
		    if (dstGen == ALLOC_GEN) {
		      /* The refered to object is in allocation space, and will be
		       * forwarded to the first generation.
		       */
			dstGen = 1;
			*addr = MinorGC_ForwardObj(gen1, w);
		    }
		}
		if (srcGen > dstGen) {
		  /* mark the card containing "addr" */
#ifndef BIT_CARDS
		    MARK_CARD(heap->gen[srcGen-1]->dirty, addr, dstGen);
#else
		    MARK_CARD(heap->gen[srcGen-1]->dirty, addr);
#endif
		}
	    }
	}
    } while (stl != STL_nil);

#ifdef GC_STATS
    numUpdates += nUpdates;
#endif

} /* end MinorGC_ScanStoreList */


/* MinorGC_SweepToSpace:
 *
 * Sweep the first generation's to-space.  Note, that since there are
 * no younger objects, we don't have to do anything special for the
 * array space.
 */
PVT void MinorGC_SweepToSpace (gen_t *gen1)
{
    Addr_t	allocBase = (Addr_t)gen1->heap->allocBase;
    Addr_t	allocSz = gen1->heap->allocSzB;
    bool_t	swept;

#define MinorGC_SweepToSpArena(indx)	{				\
	arena_t		*__ap = gen1->arena[(indx)];			\
	ml_val_t	*__p, *__q;					\
	__p = __ap->sweep_nextw;					\
	if (__p < __ap->nextw) {					\
	    swept = TRUE;						\
	    do {							\
		for (__q = __ap->nextw;  __p < __q;  __p++) {		\
		    MinorGC_CheckWord(allocBase, allocSz, gen1, __p);	\
		}							\
	    } while (__q != __ap->nextw);				\
	    __ap->sweep_nextw = __q;					\
	}								\
    } /* MinorGC_SweepToSpArena */

    do {
	swept = FALSE;

      /* Sweep the record, pair and array arenas */
	MinorGC_SweepToSpArena(RECORD_INDX);
	MinorGC_SweepToSpArena(PAIR_INDX);
	MinorGC_SweepToSpArena(ARRAY_INDX);

    } while (swept);

} /* end of MinorGC_SweepToSpace. */

/* MinorGC_ForwardObj:
 *
 * Forward an object from the allocation space to the first generation.
 */
PVT ml_val_t MinorGC_ForwardObj (gen_t *gen1, ml_val_t v)
{
    ml_val_t	*obj = PTR_MLtoC(ml_val_t, v);
    ml_val_t	*new_obj, desc;
    Word_t	len;
    arena_t	*arena;

    desc = obj[-1];
    switch (GET_TAG(desc)) {
      case DTAG_record:
	len = GET_LEN(desc);
#ifdef NO_PAIR_STRIP
	arena = gen1->arena[RECORD_INDX];
#else
	if (len == 2) {
	    arena = gen1->arena[PAIR_INDX];
	    new_obj = arena->nextw;
	    arena->nextw += 2;
	    new_obj[0] = obj[0];
	    new_obj[1] = obj[1];
	  /* setup the forward pointer in the old pair */
	    obj[-1] = DESC_forwarded;
	    obj[0] = (ml_val_t)(Addr_t)new_obj;
	    return PTR_CtoML(new_obj);
	}
	else {
	    arena = gen1->arena[RECORD_INDX];
        }
#endif
	break;
      case DTAG_vec_hdr:
      case DTAG_arr_hdr:
	len = 2;
	arena = gen1->arena[RECORD_INDX];
	break;
      case DTAG_arr_data:
	len = GET_LEN(desc);
	arena = gen1->arena[ARRAY_INDX];
	break;
/* 64BIT: on 64-bit machines, we can treat DTAG_raw and DTAG_raw64 the same */
      case DTAG_raw:
	len = GET_LEN(desc);
	arena = gen1->arena[STRING_INDX];
	break;
      case DTAG_raw64:
	len = GET_LEN(desc);
	arena = gen1->arena[STRING_INDX];
#ifdef ALIGN_REALDS
#  ifdef CHECK_HEAP
	if (((Addr_t)arena->nextw & WORD_SZB) == 0) {
	    *(arena->nextw) = (ml_val_t)0;
	    arena->nextw++;
	}
#  else
	arena->nextw = (ml_val_t *)(((Addr_t)arena->nextw) | WORD_SZB);
#  endif
#endif
	break;
      case DTAG_special:
	return MinorGC_FwdSpecial (gen1, obj, desc);
      case DTAG_forward:
	return PTR_CtoML(FOLLOW_FWDOBJ(obj));
      default:
	Die ("bad object tag %d, obj = %p, desc = %p", GET_TAG(desc), obj, desc);
    } /* end of switch */

  /* Allocate and initialize a to-space copy of the object */
    new_obj = arena->nextw;
    arena->nextw += (len + 1);
    *new_obj++ = desc;
    ASSERT(arena->nextw <= arena->tospTop);

    COPYLOOP(obj, new_obj, len);

  /* set up the forward pointer, and return the new object. */
    obj[-1] = DESC_forwarded;
    obj[0] = (ml_val_t)(Addr_t)new_obj;

    return PTR_CtoML(new_obj);

} /* end of MinorGC_ForwardObj */


/* MinorGC_FwdSpecial:
 *
 * Forward a special object (suspension, weak pointer, ...).
 */
PVT ml_val_t MinorGC_FwdSpecial (gen_t *gen1, ml_val_t *obj, ml_val_t desc)
{
    arena_t	*arena = gen1->arena[ARRAY_INDX];
    ml_val_t	*new_obj = arena->nextw;

    arena->nextw += SPECIAL_SZW;  /* all specials are two words */

    switch (GET_LEN(desc)) {
      case SPCL_evaled_susp:
      case SPCL_unevaled_susp:
	*new_obj++ = desc;
	*new_obj = *obj;
	break;
      case SPCL_weak: {
	    ml_val_t	v = *obj;
#ifdef DEBUG_WEAK_PTRS
SayDebug ("MinorGC: weak [%p ==> %p] --> %p", obj, new_obj+1, v);
#endif
	    if (! isBOXED(v)) {
#ifdef DEBUG_WEAK_PTRS
SayDebug (" unboxed\n");
#endif
	      /* weak references to unboxed objects are never nullified */
		*new_obj++ = DESC_weak;
		*new_obj = v;
	    }
	    else {
		aid_t		aid = ADDR_TO_PAGEID(BIBOP, v);
		ml_val_t	*vp = PTR_MLtoC(ml_val_t, v);

		if (aid == AID_NEW) {
		    if (vp[-1] == DESC_forwarded) {
		      /* Reference to an object that has already been forwarded.
		       * NOTE: we have to put the pointer to the non-forwarded
		       * copy of the object (i.e, v) into the to-space copy
		       * of the weak pointer, since the GC has the invariant
		       * it never sees to-space pointers during sweeping.
		       */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" already forwarded to %p\n", PTR_CtoML(FOLLOW_FWDOBJ(vp)));
#endif
			*new_obj++ = DESC_weak;
			*new_obj = v;
		    }
		    else {
		      /* the forwarded version of weak objects are threaded
		       * via their descriptor fields.  We mark the object
		       * reference field to make it look like an unboxed value,
		       * so that the to-space sweeper does not follow the weak
		       * reference.
		       */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" forward\n");
#endif
			*new_obj = MARK_PTR(PTR_CtoML(gen1->heap->weakList));
			gen1->heap->weakList = new_obj++;
			*new_obj = MARK_PTR(vp);
		    }
		}
		else {
		  /* reference to an older object */
#ifdef DEBUG_WEAK_PTRS
SayDebug (" old object\n");
#endif
		    *new_obj++ = DESC_weak;
		    *new_obj = v;
		}
	    }
	} break;
      case SPCL_null_weak: /* shouldn't happen in the allocation arena */
      default:
	Die ("strange/unexpected special object @ %p; desc = %p\n", obj, desc);
    } /* end of switch */

    obj[-1] = DESC_forwarded;
    obj[0] = (ml_val_t)(Addr_t)new_obj;

    return PTR_CtoML(new_obj);

} /* end of MinorGC_FwdSpecial */
