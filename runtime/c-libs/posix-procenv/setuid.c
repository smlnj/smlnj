/* setuid.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_setuid: SysWord.word -> unit
 *
 * Set user id
 */
ml_val_t _ml_P_ProcEnv_setuid (ml_state_t *msp, ml_val_t arg)
{
    int         sts;

    sts = setuid(SYSWORD_MLtoC(arg));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_ProcEnv_setuid */

