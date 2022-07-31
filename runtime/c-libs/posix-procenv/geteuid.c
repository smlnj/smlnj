/* geteuid.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include <unistd.h>
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_P_ProcEnv_geteuid: unit -> SysWord.word
 *
 * Return effective user id
 */
ml_val_t _ml_P_ProcEnv_geteuid (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	p;

    SYSWORD_ALLOC (msp, p, (SysWord_t)(geteuid()));
    return p;

} /* end of _ml_P_ProcEnv_geteuid */

