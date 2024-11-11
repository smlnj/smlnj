/* gc.h
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * The external interface to the garbage collector.
 *
 */

#ifndef _GC_
#define _GC_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif
#ifndef _ML_LIMITS_
#include "ml-limits.h"
#endif
#ifndef _CNTR_
#include "cntr.h"
#endif

/* typedef struct heap heap_t; */	/* from ml-base.h */

extern void InitHeap (ml_state_t *msp, bool_t isBoot, heap_params_t *params);
extern void InvokeGC (ml_state_t *msp, int level);
extern void InvokeGCWithRoots (ml_state_t *msp, int level, ...);
extern bool_t NeedGC (ml_state_t *msp, Word_t nbytes);

extern int GetObjGen (ml_val_t obj);
extern ml_val_t RecordConcat (ml_state_t *msp, ml_val_t r1, ml_val_t r2);

Byte_t *BO_AddrToCodeObjTag (Word_t pc);

#ifdef HEAP_MONITOR
extern status_t HeapMon_Init (heap_t *heap);
#else
#define HeapMon_Init(A)
#endif

/* Allocation and GC statistics */
typedef struct {
    Word_t allocCnt;            /* count of allocation in nursery (includes store
                                 * list allocations).
                                 */
    Word_t allocFirstCnt;       /* count of allocations in the first generation (i.e.,
                                 * for large sequences).
                                 */
    Word_t promoteCnt[MAX_NGENS];
                                /* count of data copied into the generations from
                                 * the same of previous generation.
                                 */
    Unsigned32_t numGCs[MAX_NGENS+1];
                                /* count of collections */
    Unsigned32_t bytesPerCnt;   /* the number of bytes per "unit" in the allocation
                                 * counters.
                                 */
} gc_stats_t;

extern void ResetGCStats (heap_t *heap);
extern void GetGCStats (ml_state_t *msp, gc_stats_t *statsOut);

#endif /* !_GC_ */
