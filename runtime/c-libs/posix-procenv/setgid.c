/* setgid.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_setgid: SysWord.word -> unit
 *
 * Set group id
 */
ml_val_t _ml_P_ProcEnv_setgid (ml_state_t *msp, ml_val_t arg)
{
    int         sts;

    sts = setgid(SYSWORD_MLtoC(arg));

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_ProcEnv_setgid */

