/*! \file localoffset.c
 *
 * \author John Reppy
 *
 * Runtime support for determining the local offset in seconds from UTC.
 */

/*
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-osdep.h"
#include "ml-base.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "ml-c.h"

#if !defined(OPSYS_WIN32)

#include "unix-date.h"

#if defined(HAS_GETTIMEOFDAY)
#  include <sys/time.h>
#else
#  error no timeofday mechanism
#endif

/* LocalOffset:
 *
 * Helper function that takes a time t in seconds and returns the offset from UTC
 * of t in the local timezone as an ML Int32.int value.  This value reflects
 * not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect at time t).
 */
PVT ml_val_t LocalOffset (ml_state_t *msp, time_t t)
{
    struct tm	tmbuf;
    int		isDST;
    time_t	t2;

  /* get the local timezone's daylight saving's time info */
    if (localtime_r (&t, &tmbuf) == NULL) {
	return RAISE_SYSERR(msp, 0);
    }
    isDST = tmbuf.tm_isdst;

  /* convert to UTC and local tm structs */
    if (gmtime_r (&t, &tmbuf) == NULL) {
	return RAISE_SYSERR(msp, 0);
    }

  /* convert the UTC tm struct back into seconds using the local timezone info (including
   * the daylight savings time field from localTM).  The local offset will be the difference
   * between this value and the original time.
   */
    tmbuf.tm_isdst = isDST;
    t2 = mktime (&tmbuf);

    return INT32_CtoML(msp, t2 - t);

}

/* _ml_Date_localOffset : unit -> Int32.int
 *
 * Returns the offset from UTC of the current time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localOffset (ml_state_t *msp, ml_val_t arg)
{
    return LocalOffset (msp, time (NIL(time_t *)));

} /* end of _ml_Date_localoffset */

/* _ml_Date_localOffsetForTime : Word64.word -> Int32.int
 *
 * Returns the offset from UTC of the given time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localOffsetForTime (ml_state_t *msp, ml_val_t arg)
{
    return LocalOffset (msp, ns_to_time(WORD64_MLtoC(arg)));

} /* end of _ml_Date_localoffset */

#else /* OPSYS_WIN32 */

#include "win32-date.h"

/* _ml_Date_localOffset : unit -> Int32.int
 *
 * Returns the offset from UTC of the current time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localOffset (ml_state_t *msp, ml_val_t arg)
{
    SYSTEMTIME localST;
    FILETIME localFT, utcFT;

    GetLocalTime (&localST);
    if (! SystemTimeToFileTime (&localST, &localFT)) {
	return RAISE_SYSERR(msp, 0);
    }

    if (LocalFileTimeToFileTime (&localFT, &utcFT)) {
      /* compute offset (local - UTC) in seconds. */
	Int64_t localSec = (Int64_t)(filetime_to_100ns (&localFT) / 10000000);
	Int64_t utcSec = (Int64_t)(filetime_to_100ns (&utcFT) / 10000000);
	return INT32_CtoML(msp, (Int32_t)(localSec - utcSec));
    }
    else {
	return RAISE_SYSERR(msp, 0);
    }

} /* end of _ml_Date_localoffset */

/* _ml_Date_localOffsetForTime : Word64.word -> Int32.int
 *
 * Returns the offset from UTC of the given time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localOffsetForTime (ml_state_t *msp, ml_val_t arg)
{
    FILETIME localFT, utcFT;

    Unsigned64_t utcNsec = WORD64_MLtoC(arg);
    ns_to_filetime (utcNsec, &utcFT);

    if (FileTimeToLocalFileTime (&utcFT, &localFT)) {
      /* compute offset (local - UTC) in seconds. */
	Int64_t localSec = (Int64_t)(filetime_to_100ns (&localFT));
	Int64_t utcSec = (Int64_t)(utcNsec / NS_PER_SEC);
	return INT32_CtoML(msp, (Int32_t)(localSec - utcSec));
    }
    else {
	return RAISE_SYSERR(msp, 0);
    }

} /* end of _ml_Date_localoffset */

#endif
