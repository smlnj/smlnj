/* timeofday.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#  include "ml-osdep.h"
#if defined(OPSYS_WIN32)
#  include <windows.h>
#elif defined(HAS_GETTIMEOFDAY)
#  include <sys/time.h>
#else
#  error no timeofday mechanism
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Time_timeofday : unit -> Word64.word
 *
 * Return the UTC time of day in nanoseconds.
 */
ml_val_t _ml_Time_timeofday (ml_state_t *msp, ml_val_t arg)
{
#if defined(OPSYS_UNIX)
    struct timeval	t;

    gettimeofday (&t, NIL(struct timezone *));

    return ML_AllocNanoseconds(msp, t.tv_sec, t.tv_usec);
#elif defined(OPSYS_WIN32)
    FILETIME ft;
    ULARGE_INTEGER uli;
    Unsigned64_t ns;

    GetSystemTimeAsFileTime (&ft);

  /* convert to nanoseconds; FILETIME is in units of 100ns */
    uli.HighPart = ft.dwHighDateTime;
    uli.LowPart = ft.dwLowDateTime;
    ns = 100 * uli.QuadPart;

    return ML_AllocWord64(msp, ns);
#else
#  error no timeofday mechanism
#endif

} /* end of _ml_Time_timeofday */
