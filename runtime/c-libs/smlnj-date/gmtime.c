/*! \file gmtime.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "ml-c.h"

#if !defined(OPSYS_WIN32)

#include "unix-date.h"

/* _ml_Date_gmtime : Word64.word -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a UTC time value (in seconds), and converts it to a 9-tuple with
 * the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday,
 * tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_gmtime (ml_state_t *msp, ml_val_t arg)
{
    time_t	t = ns_to_time(WORD64_MLtoC(arg));
    struct tm	tmbuf;

    if (gmtime_r (&t, &tmbuf) == NULL) {
	return RAISE_SYSERR(msp, 0);
    }
    else {
	return _ml_alloc_tm (msp, &tmbuf);
    }

} /* end of _ml_Date_gmtime */

#else /* OPSYS_WIN32 */

#include "win32-date.h"

/* _ml_Date_gmtime : Word64.word -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a UTC time value (in nanoseconds), and converts it to a 9-tuple with
 * the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday,
 * tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_gmtime (ml_state_t *msp, ml_val_t arg)
{
    FILETIME	utcFT;
    SYSTEMTIME	utcST;

    ns_to_filetime (WORD64_MLtoC(arg), &utcFT);

    if (! FileTimeToSystemTime (&utcFT, &utcST)) {
	return RAISE_SYSERR(msp, 0);
    }

    return _ml_alloc_tm (msp, &utcST, 0); /* UTC is never adjusted for DST */

} /* end of _ml_Date_gmtime */

#endif
