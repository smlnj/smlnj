/* vproc-state.h
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * This is the state of a virtual processor.
 */

#ifndef _VPROC_STATE_
#define _VPROC_STATE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#ifndef _ML_SIGNALS_
#include "ml-signals.h"
#endif

#ifndef _SYSTEM_SIGNALS_
#include "system-signals.h"
#endif

#ifndef _ML_TIMER_
#include "ml-timer.h"
#endif

/** The Virtual processor state vector **
 *
 * The fields that are accessed by the runtime assembly code are allocated at
 * word size to keep the assembly code simpler.
 */
struct vproc_state {
    heap_t	*vp_heap;	    /* The heap for this ML task */
    ml_state_t	*vp_state;	    /* The state of the ML task that is */
				    /* running on this VProc.  Eventually */
				    /* we will support multiple ML tasks */
				    /* per VProc. */
				    /* Signal related fields: */
    Word_t	vp_inMLFlag;		/* True while executing ML code */
    Word_t	vp_handlerPending;	/* Is there a signal handler pending? */
    Word_t	vp_inSigHandler;	/* Is an ML signal handler active? */
    sig_count_t	vp_totalSigCount;	/* summary count for all signals */
    sig_count_t	vp_sigCounts[SIGMAP_SZ]; /* counts of signals. */
    int		vp_sigCode;		/* the code and count of the next */
    int		vp_sigCount;		/* signal to handle. */
    int		vp_nextPendingSig;	/* the index in sigCounts of the next */
					/* signal to handle. */
    int		vp_gcSigState;		/* the state of the GC signal handler */
    int         vp_gcSigThreshold;      /* the generation threshold for generating a */
                                        /* GC signal (0 => all, 1 => gen 1, ...) */
    Time_t	*vp_gcTime0;	    /* The cumulative CPU time at the start of */
				    /* the last GC (see kernel/timers.c). */
    Time_t	*vp_gcTime;	    /* The cumulative GC time. */
    Addr_t	vp_limitPtrMask;   /* for raw-C-call interface */
};

#endif /* !_VPROC_STATE_ */

