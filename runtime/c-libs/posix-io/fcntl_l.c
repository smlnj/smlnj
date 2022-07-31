/* fcntl_l.c
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

/* _ml_P_IO_fcntl_l : int * int * flock_rep -> flock_rep
 *
 * where
 *
 *    flock_rep = int * int * Position.int * Position.int * int
 *
 * Handle record locking.
 */
ml_val_t _ml_P_IO_fcntl_l (ml_state_t *msp, ml_val_t arg)
{
    int              fd = REC_SELINT(arg, 0);
    int              cmd = REC_SELINT(arg, 1);
    ml_val_t         flock_rep = REC_SEL(arg, 2), obj;
    ml_val_t         start = REC_SEL(flock_rep, 2);
    ml_val_t         length = REC_SEL(flock_rep, 3);
    struct flock     flock;
    int              sts;

    flock.l_type = REC_SELINT(flock_rep, 0);
    flock.l_whence = REC_SELINT(flock_rep, 1);
    flock.l_start = (off_t)INT64_MLtoC(start);
    flock.l_len = (off_t)INT64_MLtoC(length);

    sts = fcntl(fd, cmd, &flock);

    if (sts < 0) {
	return RAISE_SYSERR(msp, sts);
    }

  /* allocate the 64-bit start and length values */
    INT64_ALLOC(msp, start, flock.l_start)
    INT64_ALLOC(msp, length, flock.l_len)

    ML_AllocWrite (msp, 0, MAKE_DESC (DTAG_record, 5));
    ML_AllocWrite (msp, 1, INT_CtoML(flock.l_type));
    ML_AllocWrite (msp, 2, INT_CtoML(flock.l_whence));
    ML_AllocWrite (msp, 3, start);
    ML_AllocWrite (msp, 4, length);
    ML_AllocWrite (msp, 5, INT_CtoML(flock.l_pid));
    obj = ML_Alloc (msp, 5);

    return obj;

} /* end of _ml_P_IO_fcntl_l */
