/* fcntl_gfl.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_gfl : int -> SysWord.word * SysWord.word
 *
 * Get file status flags and file access modes.
 */
ml_val_t _ml_P_IO_fcntl_gfl (ml_state_t *msp, ml_val_t arg)
{
    int             fd = INT_MLtoC(arg);
    int             flag;
    ml_val_t        flags, mode, obj;

    flag = fcntl(fd, F_GETFD);

    if (flag < 0)
	return RAISE_SYSERR(msp, flag);

    SYSWORD_ALLOC (msp, flags, (flag & (~O_ACCMODE)));
    SYSWORD_ALLOC (msp, mode, (flag & O_ACCMODE));
    REC_ALLOC2(msp, obj, flags, mode);

    return obj;

} /* end of _ml_P_IO_fcntl_gfl */
