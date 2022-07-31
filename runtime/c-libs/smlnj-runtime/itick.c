/*! \file itick.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#ifdef OPSYS_UNIX
#  include "ml-unixdep.h"
#  include <time.h>
#  include <sys/time.h>
#elif defined(OPSYS_WIN32)
#  include "win32-timers.h"
#endif
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_RunT_itick : unit -> Word64.word
 *
 * Return the minimum interval supported by the interval timer.
 */
ml_val_t _ml_RunT_itick (ml_state_t *msp, ml_val_t arg)
{
#if defined(HAS_CLOCK_GETRES)
    struct timespec ts;

    if (clock_getres(CLOCK_REALTIME, &ts) == 0) {
	Unsigned64_t t = NS_PER_SEC * (Unsigned64_t)ts.tv_sec + (Unsigned64_t)ts.tv_nsec;
	return ML_AllocWord64(msp, t);
    }
    else {
	return RAISE_SYSERR(msp, 0);
    }

#elif defined(HAS_SETITIMER)
  /* we guess at 10ms, since that is what is documented.  It might be smaller than
   * that, but there doesn't seem to be a way to tell.
   */
    return ML_AllocWord64(msp, 10000000);

#elif defined(OPSYS_WIN32)
  /* 1 ms == 1000000 ns */
    return ML_AllocWord64(msp, 1000000);

#else
    return RAISE_ERROR(msp, "itick unimplemented");
#endif

} /* end of _ml_RunT_itick */
