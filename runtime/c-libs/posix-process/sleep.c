/*! \file sleep.c
 *
 * \author John Reppy
 *
 * Support for Posix.Process.sleep function
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>
#include <time.h>

/* _ml_P_Process_sleep : Word64.word -> Word64.word
 *
 * Suspend execution for interval, which is given in nanoseconds.  Returns 0 on
 * normal completion, or else returns the amount of remaining time when signaled.
 *
 * TODO: generalize to finer-grain sleeping (bug #173)
 */
ml_val_t _ml_P_Process_sleep (ml_state_t *msp, ml_val_t arg)
{
    Unsigned64_t t = WORD64_MLtoC(arg);

#if defined(HAS_NANOSLEEP)
    struct timespec sleepTime, remainingTime;
    sleepTime.tv_sec = (time_t)(t / NS_PER_SEC);
    sleepTime.tv_nsec = (long)(t % NS_PER_SEC);
    remainingTime.tv_sec = 0;
    remainingTime.tv_nsec = 0;
    if (nanosleep(&sleepTime, &remainingTime) == 0) {
	t = 0;
    }
    else {
	t = NS_PER_SEC * (Unsigned64_t)remainingTime.tv_sec
	    + (Unsigned64_t)remainingTime.tv_nsec;
    }
#else
    unsigned int sleepTime, remainingTime;
    sleepTime = (unsigned int)(t / NS_PER_SEC);
    remainingTime = sleep(sleepTime);
    t = NS_PER_SEC * (Unsigned64_t)remainingTime;
#endif

    return ML_AllocWord64(msp, t);

} /* end of _ml_P_Process_sleep */
