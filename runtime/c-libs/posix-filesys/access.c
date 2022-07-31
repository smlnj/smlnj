/* access.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include INCLUDE_TYPES_H
#include <sys/stat.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_P_FileSys_access : (string * SysWord.word) -> bool
 *                         name     access_mode
 *
 * Determine accessibility of a file.
 */
ml_val_t _ml_P_FileSys_access (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	path = REC_SEL(arg, 0);
    ml_val_t	ml_mode = REC_SEL(arg, 1);
    mode_t	mode = SYSWORD_MLtoC(ml_mode);
    int		sts;

    sts = access (STR_MLtoC(path), mode);

    return (sts == 0) ? ML_true : ML_false;

} /* end of _ml_P_FileSys_access */
