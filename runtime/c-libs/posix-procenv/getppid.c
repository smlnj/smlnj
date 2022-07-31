/* getppid.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_getppid : unit -> int
 *
 * Return the process id of the parent process.
 */
ml_val_t _ml_P_ProcEnv_getppid (ml_state_t *msp, ml_val_t arg)
{
    return INT_CtoML(getppid());

} /* end of _ml_P_ProcEnv_getppid */
