/*! \file win32-timers.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * header for win32 specific interface to interval timers.  See mach-dep/win32-timers.c
 * for the implementation.
 */

#ifndef _WIN32_TIMERS_H_
#define _WIN32_TIMERS_H_

extern bool_t win32StopTimer ();
extern bool_t win32StartTimer (int milli_secs);

#endif /* _WIN32_TIMERS_H_ */
