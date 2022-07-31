/* gc-ctl.c
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * General interface for GC control functions.
 */

#include "ml-base.h"
#include <string.h>
#include "ml-values.h"
#include "ml-state.h"
#include "vproc-state.h"
#include "memory.h"
#include "heap.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

#define STREQ(s1, s2)	(strcmp((s1), STR_MLtoC(s2)) == 0)

PVT void SetVMCache (ml_state_t *msp, ml_val_t cell);
PVT void DoGC (ml_state_t *msp, ml_val_t cell, ml_val_t *next);
PVT void AllGC (ml_state_t *msp, ml_val_t *next);


/* _ml_RunT_gc_ctl : (string * int ref) list -> unit
 *
 * Current control operations:
 *
 *   ("SetVMCache", ref n)	- sets VM cache level to n; returns old cache
 *				  level.
 *   ("DoGC", ref n)		- does a GC of the first "n" generations
 *   ("AllGC", _)		- collects all generations.
 *   ("Messages", ref 0)	- turn GC messages off
 *   ("Messages", ref n)	- turn GC messages on (n > 0)
 *   ("SigThreshold", ref n)    - set GC signal threshold to n (>= 0)
 */
ml_val_t _ml_RunT_gc_ctl (ml_state_t *msp, ml_val_t arg)
{
    while (arg != LIST_nil) {
	ml_val_t	cmd = LIST_hd(arg);
	ml_val_t	oper = REC_SEL(cmd, 0);
	ml_val_t	cell = REC_SEL(cmd, 1);

	arg = LIST_tl(arg);

	if (STREQ("SetVMCache", oper))
	    SetVMCache (msp, cell);
	else if (STREQ("DoGC", oper))
	    DoGC (msp, cell, &arg);
	else if (STREQ("AllGC", oper))
	    AllGC (msp, &arg);
	else if (STREQ("Messages", oper)) {
	    if (INT_MLtoC(DEREF(cell)) > 0)
		GCMessages = TRUE;
	    else
		GCMessages = FALSE;
	}
	else if (STREQ("LimitHeap", oper)) {
          /* NOTE: this control is not needed once we have dynamically sized areans! */
	    if (INT_MLtoC(DEREF(cell)) > 0)
		UnlimitedHeap = FALSE;
	    else
		UnlimitedHeap = TRUE;
	}
        else if (STREQ("SigThreshold", oper)) {
            int threshold = INT_MLtoC(DEREF(cell));
            if (threshold < 0) threshold = 0;
            msp->ml_vproc->vp_gcSigThreshold = threshold;
        }
    }

    return ML_unit;

} /* end of _ml_RunT_gc_ctl */


/* SetVMCache:
 *
 * Set the VM cache generation, return the old level.
 */
PVT void SetVMCache (ml_state_t *msp, ml_val_t arg)
{
    int		level = INT_MLtoC(DEREF(arg));
    heap_t	*heap = msp->ml_heap;

    if (level < 0)
	level = 0;
    else if (level > MAX_NUM_GENS)
	level = MAX_NUM_GENS;

    if (level < heap->cacheGen) {
      /* Free any cached memory objects. */
	int		i;
	for (i = level;  i < heap->cacheGen;  i++)
	    MEM_FreeMemObj (heap->gen[i]->cacheObj);
    }

    ASSIGN(arg, INT_CtoML(heap->cacheGen));
    heap->cacheGen = level;

} /* end of SetVMCache */


/* DoGC:
 *
 * Force a garbage collection of the given level.
 */
PVT void DoGC (ml_state_t *msp, ml_val_t arg, ml_val_t *next)
{
    heap_t	*heap = msp->ml_heap;
    int		level = INT_MLtoC(DEREF(arg));

    if (level < 0)
	level = 0;
    else if (heap->numGens < level)
	level = heap->numGens;

    InvokeGCWithRoots (msp, level, next, NIL(ml_val_t *));

} /* end of DoGC */


/* AllGC:
 *
 * Force a garbage collection of all generations.
 */
PVT void AllGC (ml_state_t *msp, ml_val_t *next)
{
    InvokeGCWithRoots (msp, msp->ml_heap->numGens, next, NIL(ml_val_t *));

} /* end of AllGC */

