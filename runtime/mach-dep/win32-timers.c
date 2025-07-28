/*! \file win32-timers.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * win32 specific interface to times and interval timers.
 */

#include <windows.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-timer.h"
#include "win32-fault.h"
#include "win32-timers.h"

#include "signal-sysdep.h"
#include "system-signals.h"

#define SELF_VPROC	(VProc[0])

/* for interval timers */
#define WIN32_TIMER_DONE 0
#define WIN32_TIMER_COUNTING 1
#define WIN32_TIMER_HALTED 2

typedef struct {
    HANDLE handle;      /* handle for the thread that is waiting for the timer */
    DWORD id;           /* the timer thread's ID */
    int mSecs;          /* wait time in milliseconds */
    void (*action)();
} win32_timer_t;

/* global timer */
PVT struct _timeb start_timeb;
PVT win32_timer_t wt;

PVT void timer (win32_timer_t *ct)
{
    while (1) {
        /* wait for the requested time */
	Sleep (ct->mSecs);
	(*ct->action)();
    }
}

PVT BOOL create_win32_timer (win32_timer_t *ct, void (*f)(), int mSec, BOOL suspend)
{
  /* create a thread */
    ct->mSecs = mSec;
    ct->action = f;
    ct->handle = CreateThread(
        NULL,
        0,     /* default stack size */
        (LPTHREAD_START_ROUTINE) timer,
        ct,
        suspend ? CREATE_SUSPENDED : 0,
        &ct->id);

    return (ct->handle != NULL);

}

PVT BOOL destroy_win32_timer (win32_timer_t *ct)
{
    return TerminateThread(ct->handle,1);
}

PVT BOOL halt_win32_timer (win32_timer_t *ct)
{
    return SuspendThread(ct->handle) != -1;
}

PVT BOOL resume_win32_timer (win32_timer_t *ct)
{
    return ResumeThread(ct->handle) != -1;
}

bool_t win32StopTimer ()
{
    return halt_win32_timer (&wt);
}

bool_t win32StartTimer (int mSec)
{
    wt.mSecs = mSec;
    return resume_win32_timer (&wt);
}

PVT void win32_fake_sigalrm()
{
    vproc_state_t   *vsp = SELF_VPROC;

    if (SuspendThread (win32_ML_thread_handle) == -1) {
	Die ("win32_fake_sigalrm: unable to suspend ML thread");
    }

    win32_generic_handler (SIGALRM);

    if (ResumeThread (win32_ML_thread_handle) == -1) {
	Die ("win32_fake_sigalrm: unable to resume ML thread");
    }
}


/* InitTimers:
 *
 * system specific timer initialization
 */
void InitTimers ()
{
    if (!create_win32_timer (&wt, win32_fake_sigalrm, 0, TRUE)) {
	Die("InitTimers: unable to create_win32_timer");
    }
    _ftime (&start_timeb);

} /* end of InitTimers */


/* GetCPUTime:
 *
 * Get the elapsed user and/or system cpu times in a system independent way.
 */
void GetCPUTime (Time_t *usrT, Time_t *sysT)
{
    struct _timeb now_timeb, elapsed_timeb;

    _ftime(&now_timeb);
    if (now_timeb.millitm < start_timeb.millitm) {
	now_timeb.time--;
	ASSERT(now_timeb.time >= start_timeb.time);
	now_timeb.millitm += 1000;
	ASSERT(now_timeb.millitm > start_timeb.millitm);
    }
    elapsed_timeb.time = now_timeb.time - start_timeb.time;
    elapsed_timeb.millitm = now_timeb.millitm - start_timeb.millitm;
    if (usrT != NIL(Time_t *)) {
	usrT->seconds = (Int32_t) elapsed_timeb.time;
	usrT->uSeconds = ((Int32_t) elapsed_timeb.millitm) * 1000;
    }
    if (sysT != NIL(Time_t *)) {
	sysT->seconds = sysT->uSeconds = 0;
    }
}

/* end of win32-timers.c */
