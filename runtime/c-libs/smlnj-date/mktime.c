/*! \file mktime.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-c.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

#if !defined(OPSYS_WIN32)

#include <string.h>
#include "unix-date.h"

/* _ml_Date_mktime : (int * int * int * int * int * int * int * int * int) -> Word64.word
 *
 * This takes a 9-tuple with the fields: tm_sec, tm_min, tm_hour, tm_mday,
 * tm_mon, tm_year, tm_wday, tm_yday, tm_isdst, and returns the corresponding
 * localtime value (in nanoseconds).
 */
ml_val_t _ml_Date_mktime (ml_state_t *msp, ml_val_t arg)
{
    struct tm	tm;
    time_t	t;

    memset (&tm, 0, sizeof(tm));
    tm.tm_sec	= REC_SELINT(arg, 0);
    tm.tm_min	= REC_SELINT(arg, 1);
    tm.tm_hour	= REC_SELINT(arg, 2);
    tm.tm_mday	= REC_SELINT(arg, 3);
    tm.tm_mon	= REC_SELINT(arg, 4);
    tm.tm_year	= REC_SELINT(arg, 5) - 1900;
    /* tm.tm_wday = REC_SELINT(arg, 6); */  /* ignored by mktime */
    /* tm.tm_yday = REC_SELINT(arg, 7); */  /* ignored by mktime */
    tm.tm_isdst	= REC_SELINT(arg, 8);

    t = mktime (&tm);

    if (t < 0) {
	return RAISE_ERROR(msp, "Invalid date");
    }
    else {
	return ML_AllocWord64(msp, time_to_ns(t));
    }

} /* end of _ml_Date_mktime */

#else /* OPSYS_WIN32 */

#include "win32-date.h"

/* _ml_Date_mktime : (int * int * int * int * int * int * int * int * int) -> Word64.word
 *
 * This takes a 9-tuple with the fields: tm_sec, tm_min, tm_hour, tm_mday,
 * tm_mon, tm_year, tm_wday, tm_yday, tm_isdst, and returns the corresponding
 * localtime value (in nanoseconds).
 */
ml_val_t _ml_Date_mktime (ml_state_t *msp, ml_val_t arg)
{
    SYSTEMTIME localST;
    FILETIME localFT, utcFT;

    localST.wSecond		= REC_SELINT(arg, 0);
    localST.wMinute		= REC_SELINT(arg, 1);
    localST.wHour		= REC_SELINT(arg, 2);
    localST.wDay		= REC_SELINT(arg, 3);
    localST.wMonth		= REC_SELINT(arg, 4);
    localST.wYear		= REC_SELINT(arg, 5);
    localST.wDayOfWeek		= REC_SELINT(arg, 6);
    localST.wMilliseconds	= 0;

  /* convert to UTC FILETIME */
    if (! TzSpecificLocalTimeToSystemTime(NULL, &localST, &utcFT)) {
	return RAISE_ERROR(msp, "Invalid date");
    }

  /* convert UTC FILETIME to local FILETIME */
    if (! FileTimeToLocalFileTime(&utcFT, &localFT)) {
	return RAISE_ERROR(msp, "Invalid date");
    }

    return ML_AllocWord64(msp, filetime_to_ns(&localFT));

} /* end of _ml_Date_mktime */

#endif
