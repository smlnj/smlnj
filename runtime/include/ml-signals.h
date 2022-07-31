/* ml-signals.h
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ML_SIGNALS_
#define _ML_SIGNALS_

typedef struct {		/* counters for pending signals; we keep two counters */
				/* to avoid race conditions */
    Word_t		nReceived;  /* the count of how many signals of this variety */
				    /* have been received. This counter is incremented */
				    /* the signal handler */
    Word_t		nHandled;   /* the count of how many of this kind of */
				    /* signal have been handled.  This counter */
				    /* is incremented by the main thread. */
} sig_count_t;

/* The state of ML signal handlers; these definitions must agree with
 * the values used in src/sml-nj/boot/smlnj/signals.sml.
 */
#define ML_SIG_IGNORE		0
#define ML_SIG_DEFAULT		1
#define ML_SIG_ENABLED		2

/** Utility functions **/
extern void GCSignal (vproc_state_t *vsp, int nGen);
extern void ChooseSignal (vproc_state_t *vsp);
extern ml_val_t MakeResumeCont (ml_state_t *msp, ml_val_t resume[]);
extern ml_val_t MakeHandlerArg (ml_state_t *msp, ml_val_t resume[]);
extern void LoadResumeState (ml_state_t *msp);

/* OS dependent implementations of signal operations. */
extern ml_val_t ListSignals (ml_state_t *msp);
extern void PauseUntilSignal (vproc_state_t *vsp);
extern void SetSignalState (vproc_state_t *vsp, int sigNum, int sigState);
extern int GetSignalState (vproc_state_t *vsp, int sigNum);
extern void SetSignalMask (ml_val_t sigList);
extern ml_val_t GetSignalMask (ml_state_t *msp);

#endif /* !_ML_SIGNALS_ */
