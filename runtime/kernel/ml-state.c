/* ml-state.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 */

#include <stdarg.h>
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "system-signals.h"
#include "tags.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "gc.h"
#include "ml-timer.h"
#include "ml-limits.h"


vproc_state_t	*VProc[1];
int		NumVProcs;


/* local routines */
PVT void InitVProcState (vproc_state_t *vsp);


/* AllocMLState:
 */
ml_state_t *AllocMLState (bool_t isBoot, heap_params_t *heapParams)
{
    ml_state_t	*msp = NIL(ml_state_t *);

    if (((VProc[0] = NEW_OBJ(vproc_state_t)) == NIL (vproc_state_t *))
    ||  ((msp = NEW_OBJ(ml_state_t)) == NIL(ml_state_t *))) {
	Die ("unable to allocate ML state vector");
    }
    VProc[0]->vp_state = msp;

  /* allocate and initialize the heap data structures */
    InitHeap (msp, isBoot, heapParams);

    InitVProcState (VProc[0]);
    NumVProcs = 1;

  /* initialize the timers */
    ResetTimers (VProc[0]);

    return msp;

} /* end of AllocMLState */

void FreeMLState (ml_state_t *msp)
{
    FreeHeap (msp->ml_heap);

    FREE(msp);
    FREE(VProc[0]);
}

/* InitVProcState:
 */
PVT void InitVProcState (vproc_state_t *vsp)
{
    int		i;

    vsp->vp_heap			= vsp->vp_state->ml_heap;
    vsp->vp_state->ml_vproc		= vsp;
    vsp->vp_inMLFlag			= FALSE;
    vsp->vp_handlerPending		= FALSE;
    vsp->vp_inSigHandler		= FALSE;
    vsp->vp_totalSigCount.nReceived	= 0;
    vsp->vp_totalSigCount.nHandled	= 0;
    vsp->vp_sigCode			= 0;
    vsp->vp_sigCount			= 0;
    vsp->vp_nextPendingSig		= MIN_SYSTEM_SIG;
    vsp->vp_gcSigState			= ML_SIG_IGNORE;
    vsp->vp_gcSigThreshold              = 1;  /* by default, we ignore minor collections */
    vsp->vp_gcTime0			= NEW_OBJ(Time_t);
    vsp->vp_gcTime			= NEW_OBJ(Time_t);

    for (i = 0;  i < SIGMAP_SZ;  i++) {
	vsp->vp_sigCounts[i].nReceived = 0;
	vsp->vp_sigCounts[i].nHandled = 0;
    }

  /* initialize the ML state, including the roots */
    InitMLState (vsp->vp_state);
    vsp->vp_state->ml_arg		= ML_unit;
    vsp->vp_state->ml_cont		= ML_unit;
    vsp->vp_state->ml_closure		= ML_unit;
    vsp->vp_state->ml_linkReg		= ML_unit;
    vsp->vp_state->ml_pc		= ML_unit;
    vsp->vp_state->ml_exnCont		= ML_unit;
    vsp->vp_state->ml_varReg		= ML_unit;
    vsp->vp_state->ml_calleeSave[0]	= ML_unit;
    vsp->vp_state->ml_calleeSave[1]	= ML_unit;
    vsp->vp_state->ml_calleeSave[2]	= ML_unit;

} /* end of InitVProcState */

/* InitMLState:
 *
 * Initialize the ML State vector.  Note that we do not initialize the root
 * registers here, since this is sometimes called when the roots are live (from
 * ML_ApplyFn).
 */
void InitMLState (ml_state_t *msp)
{
    msp->ml_storePtr		= ML_unit;

} /* end of InitMLState. */

/* SaveCState:
 *
 *    Build a return closure that will save a collection of ML values
 * being used by C.  The ML values are passed by reference, with NIL
 * as termination.
 */
void SaveCState (ml_state_t *msp, ...)
{
    va_list	    ap;
    int		    n, i;
    ml_val_t	    *vp;

  /* count the number of values to be saved */
    va_start (ap, msp);
    for (n = 0; (vp = va_arg(ap, ml_val_t *)) != NIL(ml_val_t *);  n++)
	continue;
    va_end (ap);

    va_start (ap, msp);
  /* NOTE: we use a DTAG_arr_data to ensure that if n == 2, we don't lose our
   * header in a GC before RestoreCState is called.
   */
    ML_AllocWrite (msp, 0, MAKE_DESC(n, DTAG_arr_data));
    for (i = 1;  i <= n;  i++) {
	vp = va_arg (ap, ml_val_t *);
        ML_AllocWrite (msp, i, *vp);
    }
    msp->ml_calleeSave[0]   = ML_Alloc(msp, n);
    msp->ml_cont	    = PTR_CtoML(return_c);
    va_end (ap);

} /* end of SaveCState */

/* RestoreCState:
 *
 *    Restore a collection of ML values from the return closure.
 */
void RestoreCState (ml_state_t *msp, ...)
{
    va_list	ap;
    int		n, i;
    ml_val_t	*vp;
    ml_val_t	savedState;

    va_start (ap, msp);
    savedState = msp->ml_calleeSave[0];
    n = OBJ_LEN(savedState);
    for (i = 0;  i < n;  i++) {
	vp = va_arg (ap, ml_val_t *);
	*vp = REC_SEL(savedState, i);
    }
    va_end (ap);

} /* end of RestoreCState */

