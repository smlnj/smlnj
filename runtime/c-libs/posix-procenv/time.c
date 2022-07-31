/* time.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <time.h>

/* _ml_P_ProcEnv_time: unit -> Int32.int
 *
 * Return time in seconds from 00:00:00 UTC, January 1, 1970
 */
ml_val_t _ml_P_ProcEnv_time (ml_state_t *msp, ml_val_t arg)
{
    time_t      t;

    t = time (NIL(time_t*));

    return INT32_CtoML(msp, t);

} /* end of _ml_P_ProcEnv_time */

