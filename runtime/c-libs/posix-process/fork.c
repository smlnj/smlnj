/* fork.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_fork : unit -> int
 *
 * Fork a new process.
 */
ml_val_t _ml_P_Process_fork (ml_state_t *msp, ml_val_t arg)
{
    UNUSED_UNIT_PARAM(arg);

    int sts = fork();

    CHK_RETURN (msp, sts)

} /* end of _ml_P_Process_fork */
