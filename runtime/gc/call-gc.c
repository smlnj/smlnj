/*! \file call-gc.c
 *
 * The main interface between the GC and the rest of the run-time system.
 * These are the routines used to invoke the GC.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifdef PAUSE_STATS		/* GC pause statistics are UNIX dependent */
#  include "ml-unixdep.h"
#endif

#include <stdarg.h>
#include "ml-base.h"
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
#include "vproc-state.h"
#include "ml-signals.h"
#include "profile.h"

#ifdef C_CALLS
/* This is a list of pointers into the C heap locations that hold
 * pointers to ML functions. This list is not part of any ML data
 * structure(s).  (also see gc/major-gc.c and c-libs/c-calls/c-calls-fns.c)
 */
extern ml_val_t		CInterfaceRootList;
#endif


/* InvokeGC:
 *
 * Invoke a garbage collection.  A garbage collection always involves
 * collecting the allocation space.  In addition, if level is greater than
 * 0, or if the first generation is full after the minor collection, then
 * a major collection of one or more generations is performed (at least
 * level generations are collected).
 */
void InvokeGC (ml_state_t *msp, int level)
{
    ml_val_t	*roots[NUM_GC_ROOTS];	/* registers and globals */
    ml_val_t	**rootsPtr = roots;
    heap_t	*heap;
    int		i;

    ASSIGN(ProfCurrent, PROF_MINOR_GC);

    START_GC_PAUSE(msp->ml_heap);

#ifdef C_CALLS
    *rootsPtr++ = &CInterfaceRootList;
#endif

  /* Gather the roots */
    for (i = 0;  i < NumCRoots;  i++) {
	*rootsPtr++ = CRoots[i];
    }
    *rootsPtr++ = &(msp->ml_linkReg);
    *rootsPtr++ = &(msp->ml_arg);
    *rootsPtr++ = &(msp->ml_cont);
    *rootsPtr++ = &(msp->ml_closure);
    *rootsPtr++ = &(msp->ml_exnCont);
    *rootsPtr++ = &(msp->ml_varReg);
    *rootsPtr++ = &(msp->ml_calleeSave[0]);
    *rootsPtr++ = &(msp->ml_calleeSave[1]);
    *rootsPtr++ = &(msp->ml_calleeSave[2]);
    *rootsPtr = NIL(ml_val_t *);

    MinorGC (msp, roots);

    heap = msp->ml_heap;

  /* Check for major GC */
    if (level == 0) {
	gen_t	*gen1 = heap->gen[0];
	Word_t	sz = msp->ml_allocArenaSzB;

	for (i = 0;  i < NUM_ARENAS;  i++) {
	    arena_t *arena = gen1->arena[i];
	    if (isACTIVE(arena) && (AVAIL_SPACE(arena) < sz)) {
		level = 1;
		break;
	    }
	}
    }

    if (level > 0) {
	ASSIGN(ProfCurrent, PROF_MAJOR_GC);
	*rootsPtr = NIL(ml_val_t *);
	MajorGC (msp, roots, level);
    }
    else {
	HeapMon_UpdateHeap (heap, 1);
    }

  /* reset the allocation space */
    msp->ml_allocPtr	= heap->allocBase;
    msp->ml_limitPtr    = HEAP_LIMIT(heap);

    STOP_GC_PAUSE();

  /* conditionally signal a GC signal */
    GCSignal (msp->ml_vproc, level);

    ASSIGN(ProfCurrent, PROF_RUNTIME);

} /* end of InvokeGC */


/* InvokeGCWithRoots:
 *
 * Invoke a garbage collection with possible additional roots.  The list of
 * additional roots should be NIL terminated.  A garbage collection always
 * involves collecting the allocation space.  In addition, if level is greater
 * than 0, or if the first generation is full after the minor collection, then
 * a major collection of one or more generations is performed (at least level
 * generations are collected).
 *
 * NOTE: the MP version of this may be broken, since if a processor calls this
 * but isn't the collecting process, then the extra roots are lost.
 */
void InvokeGCWithRoots (ml_state_t *msp, int level, ...)
{
    ml_val_t	*roots[NUM_GC_ROOTS];
    ml_val_t	**rootsPtr = roots, *p;
    heap_t	*heap;
    int		i;
    va_list	ap;

    ASSIGN(ProfCurrent, PROF_MINOR_GC);

    START_GC_PAUSE(msp->ml_heap);

#ifdef C_CALLS
    *rootsPtr++ = &CInterfaceRootList;
#endif

  /* record extra roots from param list */
    va_start (ap, level);
    while ((p = va_arg(ap, ml_val_t *)) != NIL(ml_val_t *)) {
	*rootsPtr++ = p;
    }
    va_end(ap);

  /* Gather the roots */
    for (i = 0;  i < NumCRoots;  i++)
	*rootsPtr++ = CRoots[i];
    *rootsPtr++ = &(msp->ml_arg);
    *rootsPtr++ = &(msp->ml_cont);
    *rootsPtr++ = &(msp->ml_closure);
    *rootsPtr++ = &(msp->ml_exnCont);
    *rootsPtr++ = &(msp->ml_varReg);
    *rootsPtr++ = &(msp->ml_calleeSave[0]);
    *rootsPtr++ = &(msp->ml_calleeSave[1]);
    *rootsPtr++ = &(msp->ml_calleeSave[2]);
    *rootsPtr = NIL(ml_val_t *);

    MinorGC (msp, roots);

    heap = msp->ml_heap;

  /* Check for major GC */
    if (level == 0) {
	gen_t	*gen1 = heap->gen[0];
	Word_t	sz = msp->ml_allocArenaSzB;

	for (i = 0;  i < NUM_ARENAS;  i++) {
	    arena_t *arena = gen1->arena[i];
	    if (isACTIVE(arena) && (AVAIL_SPACE(arena) < sz)) {
		level = 1;
		break;
	    }
	}
    }

    if (level > 0) {
	ASSIGN(ProfCurrent, PROF_MAJOR_GC);
	*rootsPtr++ = &(msp->ml_linkReg);
	*rootsPtr++ = &(msp->ml_pc);
	*rootsPtr = NIL(ml_val_t *);
	MajorGC (msp, roots, level);
    }
    else {
	HeapMon_UpdateHeap (heap, 1);
    }

  /* reset the allocation space */
    msp->ml_allocPtr	= heap->allocBase;
    msp->ml_limitPtr    = HEAP_LIMIT(heap);

    STOP_GC_PAUSE();

  /* conditionally signal a GC signal */
    GCSignal (msp->ml_vproc, level);

    ASSIGN(ProfCurrent, PROF_RUNTIME);

} /* end of InvokeGCWithRoots */

/* NeedGC:
 *
 * Check to see if a GC is required, or if there is enough heap space for
 * nbytes worth of allocation.  Return TRUE, if GC is required, FALSE
 * otherwise.
 */
bool_t NeedGC (ml_state_t *msp, Word_t nbytes)
{
    if (((Addr_t)(msp->ml_allocPtr)+nbytes) >= (Addr_t)HEAP_LIMIT(msp->ml_heap))
	return TRUE;
    else
	return FALSE;

} /* end of NeedGC */
