/*! \file init-gc.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The GC initialization code.
 */

#ifdef PAUSE_STATS		/* GC pause statistics are UNIX dependent */
#  include "ml-unixdep.h"
#endif

#include <stdarg.h>
#include <string.h>
#include "ml-base.h"
#include "ml-options.h"
#include "ml-limits.h"
#include "memory.h"
#include "ml-state.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cntr.h"
#include "heap.h"
#include "heap-monitor.h"
#include "ml-globals.h"
#include "ml-timer.h"
#include "gc-stats.h"

PVT int		DfltRatios[MAX_NUM_GENS] = {
	DFLT_RATIO1,	DFLT_RATIO2,	DFLT_RATIO,	DFLT_RATIO,
	DFLT_RATIO,	DFLT_RATIO,	DFLT_RATIO
    };

bibop_t		BIBOP;
#ifdef SIZE_64
l2_bibop_t	UnmappedL2;
#endif

#ifdef COLLECT_STATS /** should this go into gc-stats.c ??? **/
bool_t		StatsOn = TRUE;	/* if TRUE, then generate stats */
int		StatsFD = -1;	/* the file descriptor to write the data to */
stat_rec_t	StatsBuf[STATS_BUF_SZ];	/* buffer of data */
int		NStatsRecs;	/* the number of records in the buffer */
#endif


/* ParseHeapParams:
 *
 * Parse and heap parameters from the command-line arguments.
 */
heap_params_t *ParseHeapParams (char **argv)
{
    char	    option[MAX_OPT_LEN], *optionArg;
    bool_t	    errFlg = FALSE;
    char	    *arg;
    heap_params_t   *params;

    if ((params = NEW_OBJ(heap_params_t)) == NIL(heap_params_t *)) {
	Die("unable to allocate heap_params");
    }

  /* We use 0 or "-1" to signify that the default value should be used. */
    params->allocSz = 0;
    params->numGens = -1;
    params->cacheGen = -1;

#define MATCH(opt)	(strcmp(opt, option) == 0)
#define CHECK(opt)	{						\
	if (optionArg[0] == '\0') {					\
	    errFlg = TRUE;						\
	    Error("missing argument for \"%s\" option\n", opt);		\
	    continue;							\
	}								\
    } /* CHECK */

    while ((arg = *argv++) != NIL(char *)) {
	if (isRuntimeOption(arg, option, &optionArg)) {
	    if (MATCH("alloc")) { /* set allocation size */
		int allocSz = 0;
		CHECK("alloc");
		allocSz = GetSzOption(ONE_K, optionArg);
		if (allocSz < 0) {
		    errFlg = TRUE;
		    Error ("bad argument for \"@SMLalloc\" option\n");
		}
		if (allocSz < MIN_ALLOC_SZB) {
		    Error ("argument for \"@SMLalloc\" option too small; using %dk\n",
			MIN_ALLOC_SZB/ONE_K);
		    params->allocSz = MIN_ALLOC_SZB;
		}
		else
		    params->allocSz = allocSz;
	    }
	    else if (MATCH("ngens")) {
		CHECK("ngens");
		params->numGens = atoi(optionArg);
		if (params->numGens < 1)
		    params->numGens = 1;
		else if (params->numGens > MAX_NGENS)
		    params->numGens = MAX_NGENS;
	    }
	    else if (MATCH("vmcache")) {
		CHECK("vmcache");
		params->cacheGen = atoi(optionArg);
		if (params->cacheGen < 0)
		    params->cacheGen = 0;
		else if (params->cacheGen > MAX_NGENS)
		    params->cacheGen = MAX_NGENS;
	    }
	    else if (MATCH("unlimited-heap"))
		UnlimitedHeap = TRUE;
	}
	if (errFlg)
	    return NIL(heap_params_t *);
    } /* while */

    return params;

} /* end of ParseHeapParams */

/* InitBibop:
 *
 * Initialize the big-bag-of-pages map.
 */
bibop_t InitBibop ()
{
    bibop_t bibop;
    size_t bibopSz;
    int i;

#ifdef SIZE_64
  /* initialize the level-2 table for unmapped regions */
    for (i = 0;  i < BIBOP_L2_SZ;  i++) {
	UnmappedL2.tbl[i] = AID_UNMAPPED;
    }
    UnmappedL2.numMapped = 0;

    bibopSz = BIBOP_L1_SZ * sizeof(l2_bibop_t *);
#else
    bibopSz = BIBOP_SZ * sizeof(aid_t);
#endif

    if ((bibop = MALLOC(bibopSz)) == NIL(bibop_t)) {
	Die("InitBibop: unable to allocate Bibop");
    }

#ifdef SIZE_64
#ifdef VERBOSE
    SayDebug("InitBibop: UnmappedL2 = %p\n", &UnmappedL2);
#endif
    for (i = 0;  i < BIBOP_L1_SZ;  i++) {
	bibop[i] = &UnmappedL2;
    }
#else
    for (i = 0;  i < BIBOP_SZ;  i++) {
	bibop[i] = AID_UNMAPPED;
    }
#endif

    return bibop;
}

/* FreeBibop:
 *
 * deallocate memory for a Bibop.
 */
void FreeBibop (bibop_t bibop)
{
#ifdef SIZE_64
    int i;
    for (i = 0;  i < BIBOP_L1_SZ;  i++) {
	if (bibop[i] != &UnmappedL2) {
	    FREE(bibop[i]);
	}
    }
#endif

    FREE(bibop);

}

/* InitHeap:
 *
 * Create and initialize the heap.
 */
void InitHeap (ml_state_t *msp, bool_t isBoot, heap_params_t *params)
{
    int		i, j, ratio, max_sz;
    heap_t	*heap;
    gen_t	*gen;
    mem_obj_t	*baseObj;
    ml_val_t	*allocBase;

    if (params->allocSz == 0) params->allocSz = DFLT_ALLOC;
    if (params->numGens < 0) params->numGens = DFLT_NGENS;
    if (params->cacheGen < 0) params->cacheGen = DFLT_CACHE_GEN;

  /* First we initialize the underlying memory system */
    MEM_InitMemory ();

  /* allocate the base memory object that holds the allocation space */
    {
	baseObj = MEM_AllocMemObj (params->allocSz, FALSE);
	if (baseObj == NIL(mem_obj_t *)) {
	    Die ("unable to allocate memory object for allocation spaces");
	}
	allocBase = (ml_val_t *)MEMOBJ_BASE(baseObj);
    }

  /* initialize the BIBOP */
    BIBOP = InitBibop();

  /* initialize heap descriptor */
    heap = NEW_OBJ(heap_t);
    memset ((char *)heap, 0, sizeof(heap_t));
    for (i = 0;  i < MAX_NUM_GENS;  i++) {
	ratio = DfltRatios[i];
	if (i == 0)
	    max_sz = MAX_SZ1(params->allocSz);
	else {
	    max_sz = (5*max_sz)/2;
	    if (max_sz > 64*ONE_MEG) max_sz = 64*ONE_MEG;
	}
	gen		=
	heap->gen[i]	= NEW_OBJ(gen_t);
	gen->heap	= heap;
	gen->genNum	= i+1;
	gen->numGCs	= 0;
	gen->lastPrevGC	= 0;
	gen->ratio	= ratio;
	gen->toObj	= NIL(mem_obj_t *);
	gen->fromObj	= NIL(mem_obj_t *);
	gen->cacheObj	= NIL(mem_obj_t *);
	gen->dirty	= NIL(card_map_t *);
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    gen->arena[j] = NEW_OBJ(arena_t);
	    gen->arena[j]->tospSizeB = 0;
	    gen->arena[j]->reqSizeB = 0;
	    gen->arena[j]->maxSizeB = max_sz;
	    gen->arena[j]->id = MAKE_AID(i+1, j+1, 0);
	}
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++) {
	    gen->bigObjs[j] = NIL(bigobj_desc_t *);
	}
    }
    for (i = 0;  i < params->numGens;  i++) {
	int	k = (i == params->numGens-1) ? i : i+1;
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    heap->gen[i]->arena[j]->nextGen = heap->gen[k]->arena[j];
	}
    }
    heap->numGens		= params->numGens;
    heap->cacheGen		= params->cacheGen;
    heap->numMinorGCs		= 0;
    heap->numBORegions		= 0;
    heap->bigRegions		= NIL(bigobj_region_t *);
    heap->freeBigObjs		= NEW_OBJ(bigobj_desc_t);
    heap->freeBigObjs->obj	= (Addr_t)0;
    heap->freeBigObjs->sizeB	= 0;
    heap->freeBigObjs->state	= BO_FREE;
    heap->freeBigObjs->prev	= heap->freeBigObjs;
    heap->freeBigObjs->next	= heap->freeBigObjs;
    heap->weakList		= NIL(ml_val_t *);
#ifdef VERBOSE
    SayDebug("Free Big Objects list header = %p\n", heap->freeBigObjs);
#endif

  /* initialize new space */
    heap->baseObj = baseObj;
    heap->allocBase = allocBase;
    heap->allocSzB = params->allocSz;
    MarkRegion (BIBOP, (ml_val_t *)MEMOBJ_BASE(baseObj), MEMOBJ_SZB(heap->baseObj), AID_NEW);
#ifdef VERBOSE
    SayDebug ("NewSpace = [%p, %p:%p), %d bytes\n",
	heap->allocBase, HEAP_LIMIT(heap),
	(void *)((Addr_t)heap->allocBase+params->allocSz), params->allocSz);
#endif

#ifdef GC_STATS
    ClearGCStats (heap);
#endif
#if defined(COLLECT_STATS)
    if (StatsFD > 0) {
	stat_hdr_t	hdr;
	CNTR_ZERO(&(heap->numAlloc));
	hdr.mask = STATMASK_ALLOC|STATMASK_NGENS|STATMASK_START|STATMASK_STOP;
	hdr.isNewRuntime = 1;
	hdr.allocSzB = params->allocSz;
	hdr.numGens = params->numGens;
	gettimeofday (&(hdr.startTime), NIL(struct timezone *));
	write (StatsFD, (char *)&hdr, sizeof(stat_hdr_t));
    }
#endif

    if (isBoot) {
      /* Create the first generation's to-space. */
	for (i = 0;  i < NUM_ARENAS;  i++) {
	    heap->gen[0]->arena[i]->tospSizeB = RND_MEMOBJ_SZB(2 * heap->allocSzB);
	}
	if (NewGeneration(heap->gen[0]) == FAILURE) {
	    Die ("unable to allocate initial first generation space\n");
	}
	for (i = 0;  i < NUM_ARENAS;  i++) {
	    heap->gen[0]->arena[i]->oldTop = heap->gen[0]->arena[i]->tospBase;
	}
    }

  /* initialize the GC related parts of the ML state */
    msp->ml_heap	= heap;
    msp->ml_allocPtr	= (ml_val_t *)(msp->ml_allocArena);
    msp->ml_limitPtr	= HEAP_LIMIT(heap);

#ifdef CHECK_HEAP
    CheckBIBOP (heap);
    SayDebug("****** GC initialization done ******\n");
#endif /* CHECK_HEAP */

} /* end of InitHeap */


#ifdef GC_STATS
/* ClearGCStats:
 */
void ClearGCStats (heap_t *heap)
{
    int		i, j;

    CNTR_ZERO(&(heap->numAlloc));
    for (i = 0;  i < MAX_NUM_GENS;  i++) {
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    CNTR_ZERO(&(heap->numCopied[i][j]));
	}
    }

} /* end of ClearStats */
#endif
