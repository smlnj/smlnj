/* gc-counter.c
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Some counters for measuring allocation rates, etc.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "vproc-state.h"
#include "gc.h"
#include "heap.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_RunT_gc_counter_reset : bool -> unit
 *
 * reset the counters.  If the flag is true, the we force a GC of all
 * generations before resetting.
 */
ml_val_t _ml_RunT_gc_counter_reset (ml_state_t *msp, ml_val_t arg)
{
    heap_t      *heap = msp->ml_heap;

    /* check if a full GC is requested */
    if (arg == ML_true) {
        // collect all generations
        InvokeGC (msp, heap->numGens);
    } else {
        // minor collection
        InvokeGC (msp, 0);
    }

    ResetGCStats (heap);

    return ML_unit;
}

/* _ml_RunT_gc_counter_read : unit
 *      -> (word * word * word option * word * word * word list)
 *
 * read the counters.  The results are:
 *
 *  - scaling factor for counts (word)
 *  - scaled allocation count (word)
 *  - optional store-list count (word option)
 *  - scaled first-generation allocation count (word)
 *  - scaled count of promotions to first generation (word)
 *  - # of collections in a list `[n0, n1, n2, ...]`, where ni is the number of
 *    times generation i has been collected since the "reset" call. (word list)
 */
ml_val_t _ml_RunT_gc_counter_read (ml_state_t *msp, ml_val_t arg)
{
    UNUSED_UNIT_PARAM(arg);

    gc_stats_t  stats;

    GetGCStats (msp, &stats);

    ml_val_t scale = INT_CtoML(stats.bytesPerCnt);
    ml_val_t nAlloc = INT_CtoML(stats.allocCnt);
#ifdef COUNT_STORE_LIST
    ml_val_t nStores;
    OPTION_SOME(msp, nStores, INT_CtoML(stats.storeCnt));
#else
    ml_val_t nStores = OPTION_NONE;
#endif
    ml_val_t nFirstAlloc = INT_CtoML(stats.allocFirstCnt);
    ml_val_t nPromote = INT_CtoML(stats.promoteCnt[0]);

    /* allocate number of GCs list */
    ml_val_t lp = LIST_nil;
    if (stats.numGCs[0] > 0) {
        /* allocate the list of number of collections */
        int n = 0;
        for (int i = 1;  i <= msp->ml_heap->numGens;  ++i) {
            if (stats.numGCs[i] > 0) { n = i; } else break;
        }
        for (int i = n;  i >= 0;  --i) {
            LIST_cons(msp, lp, INT_CtoML(stats.numGCs[i]), lp);
        }
    }

    /* allocate result tuple */
    ml_val_t res = ML_Alloc6(msp, scale, nAlloc, nStores, nFirstAlloc, nPromote, lp);

    return res;

}
