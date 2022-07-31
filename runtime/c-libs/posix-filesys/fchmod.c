/* fchmod.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <sys/stat.h>

/* _ml_P_FileSys_fchmod : (fd * SysWord.word) -> unit
 *                         fd   mode
 *
 * Change mode of file
 */
ml_val_t _ml_P_FileSys_fchmod (ml_state_t *msp, ml_val_t arg)
{
    int		fd = REC_SELINT(arg, 0);
    ml_val_t	ml_mode = REC_SEL(arg, 1);
    mode_t	mode = SYSWORD_MLtoC(ml_mode);
    int		sts;

    sts = fchmod (fd, mode);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_fchmod */
