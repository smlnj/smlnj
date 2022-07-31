/* times.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/times.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_times: unit -> int * int * int * int * int
 *
 * Return process and child process times, in clock ticks.
 */
ml_val_t _ml_P_ProcEnv_times (ml_state_t *msp, ml_val_t arg)
{
    clock_t      t;
    struct tms   ts;
    ml_val_t     v, e, u, s, cu, cs;

    t = times (&ts);

    if (t == -1)
	return RAISE_SYSERR(msp, -1);

/* FIXME: we should do the conversion to 64-bit nanoseconds here and then
 * return the result as a 64-bit value
 */
    e = INT32_CtoML(msp, t);
    u = INT32_CtoML(msp, ts.tms_utime);
    s = INT32_CtoML(msp, ts.tms_stime);
    cu = INT32_CtoML(msp, ts.tms_cutime);
    cs = INT32_CtoML(msp, ts.tms_cstime);
    REC_ALLOC5(msp, v, e, u, s, cu, cs);

    return v;

} /* end of _ml_P_ProcEnv_times */
