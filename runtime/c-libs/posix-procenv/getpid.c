/* getpid.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_getpid : unit -> int
 *
 * Return the process id of the current process.
 */
ml_val_t _ml_P_ProcEnv_getpid (ml_state_t *msp, ml_val_t arg)
{
    return INT_CtoML(getpid());

} /* end of _ml_P_ProcEnv_getpid */
