/*! \file utime.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <sys/time.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* FIXME: utimensat is the newest way to implement this mechanism */

/* _ml_P_FileSys_utime : (string * Word64.word * Word64.word) -> unit
 *                        name     actime(ns)    modtime(ns)
 *
 * Sets file access and modification times. If
 * actime = -1, then set both to current time.
 */
ml_val_t _ml_P_FileSys_utime (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t		path = REC_SEL(arg, 0);
    Unsigned64_t	actime = WORD64_MLtoC(REC_SEL(arg, 1));
    Unsigned64_t	modtime = WORD64_MLtoC(REC_SEL(arg, 2));
    int			sts;

    if (actime == 0xffffffffffffffff) {
	sts = utimes (STR_MLtoC(path), NIL(struct timeval *));
    }
    else {
        struct timeval	times[2];
	Unsigned64_t us = actime / 1000;	/* convert to microseconds */
	times[0].tv_sec = us / 1000000;
	times[0].tv_usec = us % 1000000;
	us = modtime / 1000;			/* convert to microseconds */
	times[1].tv_usec = us % 1000000;
	times[1].tv_sec = us / 1000000;
	sts = utimes (STR_MLtoC(path), times);
    }

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_utime */
