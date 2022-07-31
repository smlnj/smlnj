/*! \file localtime.c
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

/* _ml_Date_localtime : Word64.word -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a UTC time value (in seconds), and converts it to local time represented
 * as a 9-tuple with the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year,
 * tm_wday, tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_localtime (ml_state_t *msp, ml_val_t arg)
{
    time_t	t = ns_to_time(WORD64_MLtoC(arg));
    struct tm	tmbuf;

    if (localtime_r (&t, &tmbuf) == NULL) {
	RAISE_SYSERR(msp,0);
    }
    else {
	return _ml_alloc_tm (msp, &tmbuf);
    }

} /* end of _ml_Date_localtime */

#else /* OPSYS_WIN32 */

#include "win32-date.h"

/* _ml_Date_localtime : Word64.word -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a UTC time value (in seconds), and converts it to local time represented
 * as a 9-tuple with the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year,
 * tm_wday, tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_localtime (ml_state_t *msp, ml_val_t arg)
{
    FILETIME utcFT, localFT;
    SYSTEMTIME localST, utcST;
    TIME_ZONE_INFORMATION tzInfo;
    BOOL isDST;

    ns_to_filetime (WORD64_MLtoC(arg), &utcFT);

  /* convert to local system time */
    if (! FileTimeToLocalFileTime(&utcFT, &localFT)) {
	return RAISE_SYSERR(msp, 0);
    }

  /* convert to system time */
    if (! FileTimeToSystemTime(&localFT, &localST)) {
	return RAISE_SYSERR(msp, 0);
    }

  /* need to figure out if localST is in DST; we do this by getting the local
   * timezone info for the given year and then check the range of dates
   */
    if (! GetTimeZoneInformationForYear(localST.wYear, NULL, &tzInfo)) {
	return RAISE_SYSERR(msp, 0);
    }
    if (tzInfo.StandardDate.wMonth == 0) {
      /* timezone does not support DST */
	isDST = FALSE;
    }
    else {
	/* TODO: test localST against tzInfo.StandardState and tzInfo.DaylightDate */
    }

    return _ml_alloc_tm (msp, &localST, isDST);

} /* end of _ml_Date_localtime */

#endif
