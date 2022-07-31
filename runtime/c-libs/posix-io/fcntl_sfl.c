/* fcntl_sfl.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_sfl : int * word -> unit
 *
 * Set file status flags
 */
ml_val_t _ml_P_IO_fcntl_sfl (ml_state_t *msp, ml_val_t arg)
{
    int		sts;
    int		fd0 = REC_SELINT(arg, 0);
    ml_val_t	ml_flag = REC_SEL(arg, 1);
    SysWord_t	flag = SYSWORD_MLtoC(ml_flag);

    sts = fcntl(fd0, F_SETFL, flag);

    CHK_RETURN_UNIT(msp,sts)

} /* end of _ml_P_IO_fcntl_sfl */
