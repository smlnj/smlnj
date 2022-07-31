/* setptimer.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this implementation is UNIX specific right now; I would like to
 * define an OS abstraction layer for interval timers, which would cover
 * both alarm timers and profiling, but I need to look at what other systems
 * do first.
 */

#ifdef OPSYS_UNIX
#  include "ml-unixdep.h"
#  include <sys/time.h>
#elif OPSYS_WIN32
#  include <windows.h>
#endif
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "cfun-proto-list.h"
#include "profile.h"

extern void Enable (void);
extern void Disable (void);

/* _ml_Prof_setptimer : bool -> unit
 *
 * Turn the profile timer on/off.
 */
ml_val_t _ml_Prof_setptimer (ml_state_t *msp, ml_val_t arg)
{
#ifdef HAS_SETITIMER
    struct itimerval	new_itv;
    int			sts;


    if (arg == ML_false) {
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		=
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= 0;
    }
    else if (ProfCntArray == ML_unit) {
	return RAISE_ERROR(msp, "no count array set");
    }
    else {
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		= 0;
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= PROFILE_QUANTUM_US;
    }

    sts = setitimer (ITIMER_VIRTUAL, &new_itv, NIL(struct itimerval *));

    CHK_RETURN_UNIT(msp, sts);
#elif OPSYS_WIN32
    if (arg == ML_false)
    {
      Disable();
    }
    else if (ProfCntArray == ML_unit)
    {
      return RAISE_ERROR(msp, "no count array set");
    }
    else
    {
      Enable();
    }

    return ML_unit;
#else
    return RAISE_ERROR(msp, "time profiling not supported");
#endif

} /* end of _ml_Prof_setptimer */

#ifdef OPSYS_WIN32
/* The pointer to the heap allocated array of call counts.
* When this pointer is ML_unit, then profiling is disabled.
*/
ml_val_t	ProfCntArray = ML_unit;
HANDLE g_hTimer = NULL;
HANDLE g_hPumpThread = NULL;
HANDLE g_hQueryThread = NULL;
#define TIMEOUT_VALUE 1000

/* local routines */
VOID CALLBACK TimerAPCProc(
                           LPVOID lpArg,               // Data value
                           DWORD dwTimerLowValue,      // Timer low value
                           DWORD dwTimerHighValue );    // Timer high value

/* This thread exists to provide a spot for the APC messages to be run */
DWORD WINAPI PumpThread( LPVOID lpParam ) 
{
    LARGE_INTEGER   liDueTime;

    if (g_hTimer == NULL)
    {
        g_hTimer = CreateWaitableTimer(NULL, FALSE, TEXT("SMLNJ_PROF"));

        // Set it to go off in one quantum for the first time.
        liDueTime.QuadPart = -1 * PROFILE_QUANTUM_US;

        SetWaitableTimer(
            g_hTimer,           // Handle to the timer object
            &liDueTime,       // When timer will become signaled
            (PROFILE_QUANTUM_US / 1000 ), // Profile quantum in uS; convert to mS
            TimerAPCProc,     // Completion routine
            lpParam,             // Argument to the completion routine
            FALSE );          // Do not restore a suspended system
    }

    while(g_hTimer != NULL) 
    {
        SleepEx(TIMEOUT_VALUE, TRUE);
    } 
    return 0;
}

FILETIME current, unused1, unused2, unused3;
LARGE_INTEGER oldTime, newTime;

void Enable ()
{
    if (g_hPumpThread == NULL)
    {
        HANDLE hThread = GetCurrentThread();

        oldTime.LowPart = 0;
        oldTime.HighPart = 0;

        DuplicateHandle(GetCurrentProcess(), 
                    hThread, 
                    GetCurrentProcess(),
                    &g_hQueryThread, 
                    THREAD_QUERY_INFORMATION,
                    FALSE,
                    DUPLICATE_CLOSE_SOURCE);

        g_hPumpThread = CreateThread(NULL, 0, PumpThread, g_hQueryThread, 0, NULL);
    }
}

void Disable ()
{
    if (g_hTimer != NULL)
    {
        CancelWaitableTimer(g_hTimer);
        CloseHandle(g_hTimer);
        g_hTimer = NULL;
        if (WaitForSingleObject(g_hPumpThread, TIMEOUT_VALUE*2) != WAIT_TIMEOUT)
        {
            CloseHandle(g_hPumpThread);
            g_hPumpThread = NULL;
            CloseHandle(g_hQueryThread);
            g_hQueryThread = NULL;
        }
    }
}

/*
 * Since there's no good way on a base Windows installation to create a thread-runtime
 * specific timer, what we do instead is create a timer that checks to see if the elapsed
 * quantum amount has expired on the VM thread's execution time.
 */
VOID CALLBACK TimerAPCProc(
                           LPVOID lpArg,               // Data value
                           DWORD dwTimerLowValue,      // Timer low value
                           DWORD dwTimerHighValue )    // Timer high value
{
    HANDLE hThread = (HANDLE)lpArg;
    GetThreadTimes(hThread, &unused1, &unused2, &unused3, &current);

    newTime.LowPart = current.dwLowDateTime;
    newTime.HighPart = current.dwHighDateTime;
    // Have to divide by ten because the thread times are in 100-ns units, not 10us.
    if ((newTime.QuadPart - oldTime.QuadPart) > (PROFILE_QUANTUM_US/10))
    {
        Word_t	*arr = GET_SEQ_DATAPTR(Word_t, ProfCntArray);
        int		indx = INT_MLtoC(DEREF(ProfCurrent));

        arr[indx]++;

        oldTime = newTime;
    }
}

#endif