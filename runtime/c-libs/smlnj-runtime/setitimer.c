/*! \file setitimer.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: this implementation is UNIX specific right now; I would like to
 * define an OS abstraction layer for interval timers, which would cover
 * both alarm timers and profiling, but I need to look at what other systems
 * do first.
 */

#include "ml-base.h"
#ifdef OPSYS_UNIX
#  include "ml-unixdep.h"
#  include <sys/time.h>
#elif defined(OPSYS_WIN32)
#  include "win32-timers.h"
#endif
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_RunT_setitimer : Word64.word option -> unit
 *
 * Set the interval timer; NONE means disable the timer, otherwise
 * the time is specified in nanoseconds.
 */
ml_val_t _ml_RunT_setitimer (ml_state_t *msp, ml_val_t arg)
{
#ifdef HAS_SETITIMER
    struct itimerval	new_itv;
    int			sts;

    if (arg == OPTION_NONE) {
      /* turn the timer off */
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		=
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= 0;
    }
    else {
      /* turn the timer on */
	Unsigned64_t t = WORD64_MLtoC(OPTION_get(arg));
      /* converto to microseconds */
	t /= 1000;
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		= t / 1000000;
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= t % 1000000;
    }

    sts = setitimer (ITIMER_REAL, &new_itv, NIL(struct itimerval *));

    CHK_RETURN_UNIT(msp, sts);

#elif defined(OPSYS_WIN32)
    if (arg == OPTION_NONE) {
	if (win32StopTimer()) {
	    return ML_unit;
	} else {
	    return RAISE_ERROR(msp,"win32 setitimer: couldn't stop timer");
	}
    }
    else {
	Unsigned64_t t = WORD64_MLtoC(OPTION_get(arg));
      /* convert to milliseconds */
	t /= 1000000;

	if (t <= 0) {
	    return RAISE_ERROR(msp, "win32 setitimer: invalid resolution");
	}
	else {
	    if (win32StartTimer(t)) {
		return ML_unit;
	    } else {
		return RAISE_ERROR(msp,"win32 setitimer: couldn't start timer");
	    }
	}
    }

#else
    return RAISE_ERROR(msp, "setitimer not supported");
#endif

} /* end of _ml_RunT_setitimer */

