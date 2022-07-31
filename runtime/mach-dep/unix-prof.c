/*! \file unix-prof.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML Profiling support for Unix.
 */

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "profile.h"


/* The pointer to the heap allocated array of call counts.
 * When this pointer is ML_unit, then profiling is disabled.
 */
ml_val_t	ProfCntArray = ML_unit;

/* local routines */
#if defined(HAS_POSIX_SIGS) && defined(HAS_UCONTEXT)
PVT SigReturn_t ProfSigHandler (int sig, SigInfo_t info, void *scp);
#elif (defined(ARCH_PPC) && defined(OPSYS_LINUX))
PVT SigReturn_t ProfSigHandler (int sig, SigContext_t *scp);
#else
PVT SigReturn_t ProfSigHandler (int sig, SigInfo_t info, SigContext_t *scp);
#endif


/* EnableProfSignals:
 */
void EnableProfSignals ()
{
    SIG_SetHandler (SIGVTALRM, ProfSigHandler);

} /* end of EnableProfSignals */

/* DisableProfSignals:
 */
void DisableProfSignals ()
{
    SIG_SetHandler (SIGVTALRM, SIG_DFL);

} /* end of DisableProfSignals */

/* ProfSigHandler:
 *
 * The handler for SIGVTALRM signals.
 */
#if defined(HAS_POSIX_SIGS) && defined(HAS_UCONTEXT)
PVT SigReturn_t ProfSigHandler (int sig, SigInfo_t info, void *scp)
#elif (defined(ARCH_PPC) && defined(OPSYS_LINUX))
PVT SigReturn_t ProfSigHandler (int sig, SigContext_t *scp)
#else
PVT SigReturn_t ProfSigHandler (int sig, SigInfo_t info, SigContext_t *scp)
#endif
{
    Word_t	*arr = GET_SEQ_DATAPTR(Word_t, ProfCntArray);
    int		indx = INT_MLtoC(DEREF(ProfCurrent));

    arr[indx]++;

} /* end of ProfSigHandler */
