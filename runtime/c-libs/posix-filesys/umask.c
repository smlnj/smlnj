/* umask.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_umask : SysWord.word -> SysWord.word
 *
 * Set and get file creation mask
 * Assumes umask never fails.
 */
ml_val_t _ml_P_FileSys_umask (ml_state_t *msp, ml_val_t arg)
{
    mode_t		omask;
    ml_val_t            p;

    omask = umask(SYSWORD_MLtoC(arg));
    SYSWORD_ALLOC (msp, p, (SysWord_t)omask);

    return p;

} /* end of _ml_P_FileSys_umask */
