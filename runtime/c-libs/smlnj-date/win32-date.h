/*! \file win32-date.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common definitions for Windows versions of the Date functions.
 */

#ifndef _WIN32_DATE_H_
#define _WIN32_DATE_H_

#include "ml-base.h"
#include <windows.h>

/* convert a FILETIME to a 64-bit unsigned integer */
STATIC_INLINE Unsigned64_t filetime_to_100ns (FILETIME *ft)
{
    return ((Unsigned64_t)ft->dwHighDateTime << 32) | (Unsigned64_t)ft->dwLowDateTime;
}

/* convert an unsigned 64-bit nanoseconds value to a FILETIME value. */
STATIC_INLINE void ns_to_filetime (Unsigned64_t ns, FILETIME *ft)
{
    ns /= 100;	/* convert to 100ns units */

    ft->dwLowDateTime = (DWORD)ns;
    ft->dwHighDateTime = (DWORD)(ns >> 32);
}

/* convert a FILETIME in 100ns units to unsigned nanoseconds */
STATIC_INLINE Unsigned64_t filetime_to_ns (const FILETIME *ft)
{
    return 100 * filetime_to_100ns(ft);	/* convert to nanoseconds */
}

/* compute the day of the year from a SYSTEMTIME struct */
int _ml_year_day (const SYSTEMTIME *st);
ml_val_t _ml_alloc_tm (ml_state_t *msp, const SYSTEMTIME *st, BOOL isDST);

#endif /* !_WIN32_DATE_H_ */
