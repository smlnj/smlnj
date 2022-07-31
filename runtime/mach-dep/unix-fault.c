/*! \file unix-fault.c
 *
 * Common code for handling arithmetic traps and signals.
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common code for handling arithmetic traps.
 */

#if defined(__CYGWIN32__)

#include "cygwin-fault.c"

#else

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-globals.h"

#ifdef SIGNAL_DEBUG
#include "gc.h"		/* for BO_AddrToCodeObjTag */
#endif

/* this is temporary */
#define SELF_VPROC	(VProc[0])


/* local routines */
#ifdef SIG_OVERFLOW
#if defined(HAS_POSIX_SIGS) && defined(HAS_UCONTEXT)
PVT SigReturn_t FaultHandler (int sig, SigInfo_t code, void *scp);
#elif (defined(ARCH_PPC) && defined(OPSYS_LINUX))
PVT SigReturn_t FaultHandler (int sig, SigContext_t *scp);
#else
PVT SigReturn_t FaultHandler (int sig, SigInfo_t code, SigContext_t *scp);
#endif
#endif /* SIG_OVERFLOW */

/* InitFaultHandlers:
 */
void InitFaultHandlers (ml_state_t *msp)
{

  /** Set up the Overflow fault(s).  Note that on some systems (e.g., arm64),
   ** we do not use traps to signal overflow.
   **/
#ifdef SIG_OVERFLOW
    SIG_SetHandler (SIG_OVERFLOW, FaultHandler);
#endif
#ifdef SIG_OVERFLOW2
    SIG_SetHandler (SIG_OVERFLOW2, FaultHandler);
#endif

  /** Initialize the floating-point unit **/
    SIG_InitFPE ();

} /* end of InitFaultHandlers */


#ifdef SIG_OVERFLOW

/* FaultHandler:
 *
 * Handle arithmetic faults. Note that since floating-point arithmetic
 * is non-trapping in SML and since the compiler generates code to
 * explicitly test for division by zero, and arithmetic trap should be
 * mapped to Overflow.
 */
#if defined(HAS_POSIX_SIGS) && defined(HAS_UCONTEXT)

PVT SigReturn_t FaultHandler (int signal, siginfo_t *si, void *uc)
{
    ucontext_t	    *scp = (ucontext_t *)uc;
    Addr_t	    pc = (Addr_t)SIG_GetPC(scp);
    ml_state_t	    *msp = SELF_VPROC->vp_state;
    extern Word_t   request_fault[];

#ifdef SIGNAL_DEBUG
    SayDebug ("Fault handler: pc = %p, sig = %d, inML = %d\n",
	(void*)pc, signal, SELF_VPROC->vp_inMLFlag);
    if (SELF_VPROC->vp_inMLFlag) {
	SayDebug ("  source file: %s\n", (char *)BO_AddrToCodeObjTag(pc));
    }
#endif

    if (! SELF_VPROC->vp_inMLFlag) {
	Die ("bogus fault not in ML: pc = %p, sig = %d\n", (void*)pc, signal);
    }

#ifdef SIG_IS_OVERFLOW_TRAP
  /* verify that the signal actually comes from an overflow */
    if (! SIG_IS_OVERFLOW_TRAP(signal,pc)) {
	Die ("bogus overflow fault: pc = %p, sig = %d\n", (void*)pc, signal);
    }
#endif

    msp->ml_faultPC = pc;

    SIG_SetPC (scp, request_fault);

} /* end of FaultHandler */

#else

PVT SigReturn_t FaultHandler (
    int		    signal,
#if (defined(ARCH_PPC) && defined(OPSYS_LINUX))
    SigContext_t    *scp)
#else
    SigInfo_t	    info,
    SigContext_t    *scp)
#endif
{
    ml_state_t	    *msp = SELF_VPROC->vp_state;
    extern Word_t   request_fault[];
    int		    code = SIG_GetCode(info, scp);

#ifdef SIGNAL_DEBUG
    SayDebug ("Fault handler: sig = %d, inML = %d\n",
	signal, SELF_VPROC->vp_inMLFlag);
#endif

    if (! SELF_VPROC->vp_inMLFlag)
	Die ("bogus fault not in ML: sig = %d, code = %#x, pc = %#x)\n",
	    signal, SIG_GetCode(info, scp), SIG_GetPC(scp));

    msp->ml_faultPC = (Word_t)SIG_GetPC(scp);

    SIG_SetPC (scp, request_fault);

    SIG_ResetFPE (scp);

} /* end of FaultHandler */

#endif
#endif /* SIG_OVERFLOW */

#if ((defined(ARCH_RS6000) || defined(ARCH_PPC)) && defined(OPSYS_AIX))

/* SIG_GetCode:
 *
 * For  AIX, the overflow and divide by zero information is obtained
 * from information contained in the sigcontext structure.
 */
PVT int SIG_GetCode (SigInfo_t code, SigContext_t *scp)
{
    struct fp_sh_info	FPInfo;

    fp_sh_info (scp, &FPInfo, sizeof(struct fp_sh_info));

    return FPInfo.trap;

} /* end of SIG_GetCode */

#endif

#endif /* !defined(__CYGWIN32__) */
