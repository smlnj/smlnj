/* gettime.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-timer.h"
#include "cfun-proto-list.h"

/* _ml_Time_gettime : unit -> Int64.int * Int64.int * Int64.int * Int64.int
 *
 * Return the total CPU time, total system time, gc CPU time, and gc system
 * time used by this process so far.
 */
ml_val_t _ml_Time_gettime (ml_state_t *msp, ml_val_t arg)
{
    Time_t		t, s;
    ml_val_t		usrT, sysT, gcUsrT, gcSysT, res;
    vproc_state_t	*vsp = msp->ml_vproc;

    GetCPUTime (&t, &s);

    usrT = ML_AllocNanoseconds(msp, t.seconds, t.uSeconds);
    sysT = ML_AllocNanoseconds(msp, s.seconds, s.uSeconds);
    gcUsrT = ML_AllocNanoseconds(msp, vsp->vp_gcUsrTime->seconds, vsp->vp_gcUsrTime->uSeconds);
    gcSysT = ML_AllocNanoseconds(msp, vsp->vp_gcSysTime->seconds, vsp->vp_gcSysTime->uSeconds);

    REC_ALLOC4(msp, res, usrT, sysT, gcUsrT, gcSysT);

    return res;

} /* end of _ml_Time_gettime */
