/* getuid.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-objects.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_ProcEnv_getuid: unit -> SysWord.word
 *
 * Return user id
 */
ml_val_t _ml_P_ProcEnv_getuid (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	p;

    SYSWORD_ALLOC (msp, p, (Word_t)(getuid()));
    return p;

} /* end of _ml_P_ProcEnv_getuid */

