/*! \file win32-date.c
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utility code for date functions on Windows
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "win32-date.h"

STATIC_INLINE BOOL divisable (WORD a, WORD b) { return (a % b) == 0; }

STATIC_INLINE BOOL isLeapYear (WORD y)
{
    return ((divisable(y, 4) && (! divisable(y, 100))) || divisable(y, 400));
}

/* compute the day of the year for a given SYSTEMTIME struct */
int _ml_year_day (const SYSTEMTIME *st)
{
    int yday = st->wDay;

  /* January */
    if (st->wMonth == 1) return yday;
    yday += 31;
  /* February */
    if (st->wMonth == 2) return yday;
    yday += (isLeapYear(st->wYear) ? 29 : 28);
  /* March */
    if (st->wMonth == 3) return yday;
    yday += 31;
  /* April */
    if (st->wMonth == 4) return yday;
    yday += 30;
  /* May */
    if (st->wMonth == 5) return yday;
    yday += 31;
  /* June */
    if (st->wMonth == 6) return yday;
    yday += 30;
  /* July */
    if (st->wMonth == 7) return yday;
    yday += 31;
  /* August */
    if (st->wMonth == 8) return yday;
    yday += 31;
  /* September */
    if (st->wMonth == 9) return yday;
    yday += 30;
  /* October */
    if (st->wMonth == 10) return yday;
    yday += 31;
  /* November */
    if (st->wMonth == 11) return yday;
    yday += 30;

    return yday;

}

ml_val_t _ml_alloc_tm (ml_state_t *msp, const SYSTEMTIME *st, BOOL isDST)
{
  /* The SYSTEMTIME struct has everything that we need except the day of the year */
    ML_AllocWrite(msp, 0, MAKE_DESC(DTAG_record, 9));
    ML_AllocWrite(msp, 1, INT_CtoML(st->wSecond));
    ML_AllocWrite(msp, 2, INT_CtoML(st->wMinute));
    ML_AllocWrite(msp, 3, INT_CtoML(st->wHour));
    ML_AllocWrite(msp, 4, INT_CtoML(st->wDay));
    ML_AllocWrite(msp, 5, INT_CtoML(st->wMonth - 1));		/* convert to 0..11 */
    ML_AllocWrite(msp, 6, INT_CtoML(st->wYear));
    ML_AllocWrite(msp, 7, INT_CtoML(st->wDayOfWeek));
    ML_AllocWrite(msp, 8, INT_CtoML(_ml_year_day(st)));
    ML_AllocWrite(msp, 9, INT_CtoML(isDST));

    return ML_Alloc(msp, 9);

}
