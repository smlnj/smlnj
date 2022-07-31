/* fcntl_sfd.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_sfd : int * SysWord.word -> unit
 *
 * Set the close-on-exec flag associated with the file descriptor.
 */
ml_val_t _ml_P_IO_fcntl_sfd (ml_state_t *msp, ml_val_t arg)
{
    int		sts;
    int		fd0 = REC_SELINT(arg, 0);
    ml_val_t	ml_flag = REC_SEL(arg, 1);
    SysWord_t	flag = SYSWORD_MLtoC(ml_flag);

    sts = fcntl(fd0, F_SETFD, flag);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_IO_fcntl_sfd */
