/* mkfifo.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <sys/stat.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"


/* _ml_P_FileSys_mkfifo : (string * word) -> unit
 *                         name     mode
 *
 * Make a FIFO special file.
 */
ml_val_t _ml_P_FileSys_mkfifo (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	path = REC_SEL(arg, 0);
    ml_val_t	ml_mode = REC_SEL(arg, 1);
    mode_t	mode = SYSWORD_MLtoC(ml_mode);
    int		sts;

    sts = mkfifo (STR_MLtoC(path), mode);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_mkfifo */
