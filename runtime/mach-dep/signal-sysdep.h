/* signal-sysdep.h
 *
 * COPYRIGHT (c) 2019 The SML/NJ Fellowship.
 * All rights reserved.
 *
 * O.S. and machine dependent signal definitions for UNIX systems:
 *
 *   typedef SigReturn_t        the return type of a signal handler.
 *   typedef SigInfo_t          the signal generation information passed to a
 *                              a signal handler.
 *   typedef SigContext_t       the context info passed to a signal handler.
 *   typedef SigMask_t		the representation of a set of signals
 *
 *   SIG_GetCode(info, scp)	extract the signal generation information
 *   SIG_GetPC(scp)		get the PC from the context
 *   SIG_SetPC(scp, addr)	set the PC in the context to the address
 *   SIG_SetHandler(sig, h)	set the signal handler
 *   SIG_SetDefault(sig)	set the handler for sig to SIG_DFL
 *   SIG_SetIgnore(sig)		set the handler for sig to SIG_IGN
 *   SIG_GetHandler(sig, h)	get the current handler into h
 *   SIG_ClearMask(mask)	clear the given signal mask.
 *   SIG_AddToMask(mask, sig)	Add the given signal to the mask.
 *   SIG_isSet(mask, sig)	Return true, if the signal is in the mask.
 *   SIG_SetMask(mask)		Set the signal mask.
 *   SIG_GetMask(mask)		Get the signal mask into the variable mask.
 *
 *   SIG_FAULT[12]		The signals used to detect faults.
 *
 *   SIG_InitFPE()		This macro is defined to be a routine for
 *				initializing the FPE hardware exception mechanism.
 *
 *   SIG_ResetFPE(scp)		This macro is defined to be a routine for resetting
 *				the signal handling state (or hardware status
 *				registers) on machines that require it; otherwise
 *				it is defined to the empty statement.
 *
 * There are two ways to force a GC when a signal occurs.  For some machines,
 * this is done in an assembly routine called ZeroLimitPtr; for others, this
 * can be done directly by manipulating the signal context.  The following
 * macros are used for this purpose:
 *
 *   USE_ZERO_LIMIT_PTR_FN	If set, then we use the ZeroLimitPtr function.
 *   SIG_SavePC(msp, scp)	Save the PC, so that ZeroLimitPtr can restore it.
 *
 *   SIG_ZeroLimitPtr(scp)	Set the limit pointer in the context to zero.
 *
 * NOTE: Currently SavedPC is a global (so that the asm code in adjust_limit
 * can access it).  Once we have a runtimeLink register that allows dynamic
 * access to the MLState, we can move SavedPC to the ML State vector.
 */

#ifndef _SIGNAL_SYSDEP_
#define _SIGNAL_SYSDEP_

#ifndef _ML_OSDEP_
#include "ml-osdep.h"
#endif

#ifndef _ML_BASE_
#include "ml-base.h"	/* for Addr_t */
#endif

#if defined(OPSYS_UNIX)
#  include <signal.h>
#endif

#if defined(HAS_UCONTEXT)
#ifdef __APPLE__
#  include <sys/ucontext.h>
#else
#  include <ucontext.h>
#endif
#ifdef INCLUDE_SIGINFO_H
#  include INCLUDE_SIGINFO_H
#endif

typedef void SigReturn_t;
typedef siginfo_t *SigInfo_t;
typedef ucontext_t SigContext_t;

#elif defined(HAS_SIGCONTEXT)

typedef int SigInfo_t;
typedef struct sigcontext SigContext_t;
#endif


#if defined(HAS_POSIX_SIGS)
/** POSIX signals **/
#  if defined(HAS_UCONTEXT)
#    define SIG_SetHandler(sig, h)	{       		\
	    struct sigaction __svec;        			\
	    sigfillset(&(__svec.sa_mask));  			\
	    __svec.sa_flags = SA_SIGINFO;			\
	    __svec.sa_sigaction = (h);        			\
	    sigaction ((sig), &__svec, 0);  			\
	}
#    define SIG_SetIgnore(sig)		{			\
	    struct sigaction __svec;        			\
	    __svec.sa_flags = 0;				\
	    __svec.sa_handler = SIG_IGN;        		\
	    sigaction ((sig), &__svec, 0);  			\
	}
#    define SIG_SetDefault(sig)		{			\
	    struct sigaction __svec;        			\
	    __svec.sa_flags = 0;				\
	    __svec.sa_handler = SIG_DFL;        		\
	    sigaction ((sig), &__svec, 0);  			\
	}
#  else
#    define SIG_SetHandler(sig, h)	{       		\
	    struct sigaction __svec;        			\
	    sigfillset(&(__svec.sa_mask));  			\
	    __svec.sa_flags = 0;			\
	    __svec.sa_handler = (h);        			\
	    sigaction ((sig), &__svec, 0);  			\
	}
#    define SIG_SetIgnore(sig)	SIG_SetHandler(sig, SIG_IGN)
#    define SIG_SetDefault(sig)	SIG_SetHandler(sig, SIG_DFL)
#endif
#define SIG_GetHandler(sig, h)  {				\
	struct sigaction __svec;				\
	sigaction ((sig), NIL(struct sigaction *), &__svec);	\
	(h) = __svec.sa_handler;				\
    }
typedef sigset_t SigMask_t;
#define SIG_ClearMask(mask) 	sigemptyset(&(mask))
#define SIG_AddToMask(mask, s)	sigaddset(&(mask), (s))
#define SIG_isSet(mask, s)	sigismember(&(mask), (s))
#define SIG_SetMask(mask)	sigprocmask(SIG_SETMASK, &(mask), NIL(sigset_t *))
#define SIG_GetMask(mask)	sigprocmask(SIG_SETMASK, NIL(sigset_t *), &(mask))

#elif defined(HAS_BSD_SIGS)
/** BSD signals **/
#define SIG_SetHandler(sig, h)	{       		\
	struct sigvec __svec;               		\
	__svec.sv_mask = 0xFFFFFFFF;        		\
	__svec.sv_flags = SV_INTERRUPT;			\
	__svec.sv_handler = (h);            		\
	sigvec ((sig), &__svec, 0);         		\
    }
#define SIG_SetIgnore(sig)	SIG_SetHandler(sig, SIG_IGN)
#define SIG_SetDefault(sig)	SIG_SetHandler(sig, SIG_DFL)
#define SIG_GetHandler(sig, h)  {			\
	struct sigvec __svec;				\
	sigvec ((sig), NIL(struct sigvec *), &__svec);	\
	(h) = __svec.sv_handler;			\
    }
typedef int SigMask_t;
#define SIG_ClearMask(mask)	((mask) = 0)
#define SIG_AddToMask(mask, s)	((mask) |= sigmask(s))
#define SIG_isSet(mask, s)	(((mask) & sigmask(s)) != 0)
#define SIG_SetMask(mask)	sigsetmask(mask)
#define SIG_GetMask(mask)	{			\
	int		__tmpMask;			\
	__tmpMask = 0xFFFFFFFF;				\
	(mask) = sigsetmask(__tmpMask);			\
	sigsetmask(mask);				\
    }
#elif defined(OPSYS_WIN32)
  /* no win32 signals yet */
#else
#  error no way to set signal handler
#endif


/** Machine/OS dependent stuff **/

#if defined(ARCH_SPARC)

extern void SetFSR(int);
  /* disable all FP exceptions */
#  define SIG_InitFPE()    SetFSR(0)

#  if defined(OPSYS_SOLARIS)
    /** SPARC, SOLARIS **/
#    define SIG_OVERFLOW	SIGFPE

#    define SIG_GetCode(info,scp)	((info)->si_code)

#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[REG_PC])
#    define SIG_SetPC(scp, addr)	{			\
	(scp)->uc_mcontext.gregs[REG_PC] = (Addr_t)(addr);	\
	(scp)->uc_mcontext.gregs[REG_nPC] = (Addr_t)(addr) + 4;	\
    }
#    define SIG_ZeroLimitPtr(scp)	\
	{ (scp)->uc_mcontext.gregs[REG_G4] = 0; }

#  endif

#elif defined(ARCH_PPC)
#  if defined (OPSYS_AIX)
    /** RS6000 or PPC, AIX **/
#    include <fpxcp.h>
#    define SIG_OVERFLOW		SIGTRAP

     PVT int SIG_GetCode (SigInfo_t info, SigContext_t *scp);
#    define SIG_GetPC(scp)	((scp)->sc_jmpbuf.jmp_context.iar)
#    define SIG_SetPC(scp, addr)	\
	{ (scp)->sc_jmpbuf.jmp_context.iar = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	\
	{ (scp)->sc_jmpbuf.jmp_context.gpr[15] = 0; }
#    define SIG_ResetFPE(scp)	{						\
	    SigContext_t	*__scp = (scp);					\
	    struct mstsave	*__scj = &(__scp->sc_jmpbuf.jmp_context);	\
	    fp_ctx_t		__flt_ctx;					\
	    __scj->xer &= 0x3fffffff;						\
	    fp_sh_trap_info (__scp, &__flt_ctx);				\
	    fp_sh_set_stat (__scp, (__flt_ctx.fpscr & ~__flt_ctx.trap));	\
	}
     typedef void SigReturn_t;

#  elif defined(OPSYS_DARWIN)
    /* PPC, Darwin */
#    define SIG_InitFPE()        set_fsr()
#    define SIG_ResetFPE(scp)
#    define SIG_OVERFLOW           SIGTRAP
   /* info about siginfo_t is missing in the include files 4/17/2001 */
#    define SIG_GetCode(info,scp) 0
  /* see /usr/include/mach/ppc/thread_status.h */
#    define SIG_GetPC(scp)		((scp)->uc_mcontext->ss.srr0)
#    define SIG_SetPC(scp, addr)	{(scp)->uc_mcontext->ss.srr0 = (Addr_t) addr;}
  /* The offset of 17 is hardwired from reverse engineering the contents of
   * sc_regs. 17 is the offset for register 15.
   */
#    define SIG_ZeroLimitPtr(scp)	{  (scp)->uc_mcontext->ss.r15 = 0; }

#  elif (defined(ARCH_PPC) && defined(OPSYS_LINUX))
    /* PPC, Linux */

#    include <signal.h>
     typedef struct sigcontext_struct SigContext_t;

#    define SIG_OVERFLOW          	SIGTRAP

#    define SIG_GetPC(scp)              ((scp)->regs->nip)
#    define SIG_SetPC(scp, addr)        { (scp)->regs->nip = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)       { ((scp)->regs->gpr[15] = 0); } /* limitptr = 15 (see src/runtime/mach-dep/PPC.prim.asm) */
#    define SIG_GetCode(info,scp)       ((scp)->regs->gpr[PT_FPSCR])
#    define SIG_ResetFPE(scp)           { (scp)->regs->gpr[PT_FPSCR] = 0x0; }
     typedef void SigReturn_t;

#  elif defined(OPSYS_OPENBSD)
   /** PPC, OpenBSD **/

#    define SIG_OVERFLOW			SIGTRAP
#    define SIG_GetPC(scp)              ((scp)->sc_frame.srr0)
#    define SIG_SetPC(scp, addr)        { (scp)->sc_frame.srr0 = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)       { ((scp)->sc_frame.fixreg[15] = 0); } /* limitptr = 15 (see src/runtime/mach-dep/PPC.prim.asm) */
#    define SIG_GetCode(info,scp)       (info)

    typedef void SigReturn_t;

#  endif /* ARCH_PPC */

#elif defined(ARCH_X86)

#  define LIMITPTR_X86OFFSET	3	/* offset (words) of limitptr in ML stack */
					/* frame (see X86.prim.asm) */
   extern Addr_t *ML_X86Frame;		/* used to get at limitptr */
   extern void FPEEnable ();		/* defined in X86.prim.asm */
#  define SIG_InitFPE()    FPEEnable()

  /** OS-specific definitions for x86 */
#  if defined(OPSYS_CYGWIN)
     /** x86, Cygwin -- see mach-dep/cygwin-fault.c */

#    define SIG_OVERFLOW		SIGFPE

#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_DARWIN)
    /** x86, Darwin **/
#    define SIG_OVERFLOW		SIGFPE

    /* see /usr/include/mach/i386/thread_status.h */
#    define SIG_GetCode(info,scp)	((info)->si_code)
#    if ((__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ - 1040) <= 0)
      /* Tiger */
#      define SIG_GetPC(scp)		((scp)->uc_mcontext->ss.eip)
#      define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext->ss.eip = (Addr_t) addr; }
#    else
     /* Leopard or later */
#      define SIG_GetPC(scp)		((scp)->uc_mcontext->__ss.__eip)
#      define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext->__ss.__eip = (Addr_t) addr; }
#    endif
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

#  elif defined(OPSYS_FREEBSD)
    /** x86, FreeBSD **/
#    define SIG_OVERFLOW		SIGFPE

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->uc_mcontext.mc_eip)
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext.mc_eip = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_LINUX)
    /** X86, LINUX **/
#    define INTO_OPCODE		0xce	/* the 'into' instruction is a single */
					/* instruction that signals Overflow */

#    define SIG_OVERFLOW		SIGSEGV

#    define SIG_GetCode(info,scp)	((scp)->uc_mcontext.gregs[REG_EIP])
/* for linux, SIG_GetCode simply returns the address of the fault */
#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[REG_EIP])
#    define SIG_SetPC(scp,addr)		{ (scp)->uc_mcontext.gregs[REG_EIP] = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

/* macro to check if SIGSEGV was caused by `into` instruction */
#    define SIG_IS_OVERFLOW_TRAP(sig,pc) \
	(((Byte_t*)pc)[-1] == 0xce)

#  elif defined(OPSYS_NETBSD2)
    /** x86, NetBSD (version 2.x) **/
#    define SIG_OVERFLOW		SIGFPE	/* maybe this should be SIGBUS? */

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_pc = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_NETBSD)
    /** x86, NetBSD (version 3.x) **/
#    define SIG_OVERFLOW		SIGFPE	/* maybe this should be SIGBUS? */

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		(_UC_MACHINE_PC(scp))
#    define SIG_SetPC(scp, addr)	{ _UC_MACHINE_SET_PC(scp, ((Addr_t) (addr))); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

#  elif defined(OPSYS_OPENBSD)
    /** x86, OpenBSD **/
#    define SIG_OVERFLOW		SIGFPE	/* maybe this should be SIGBUS? */

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->sc_pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_pc = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_SOLARIS)
     /** x86, Solaris */
#    define SIG_OVERFLOW		SIGFPE

#    define SIG_GetCode(info, scp)	((info)->si_code)
#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[EIP])
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext.gregs[EIP] = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

#  elif defined(OPSYS_WIN32)
#    define SIG_ZeroLimitPtr()		{ ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }

#  else
#    error "unknown OPSYS for x86"
#  endif

#elif defined(ARCH_AMD64)

#  define SIG_InitFPE()

#  if defined(OPSYS_CYGWIN)
     /** amd64, Cygwin -- see mach-dep/cygwin-fault.c */

#    define SIG_OVERFLOW		SIGFPE

#    define SIG_GetPC(scp)		((scp)->uc_mcontext.rip)
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext.rip = (Addr_t) addr; }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->uc_mcontext.r14 = 0; }

     typedef void SigReturn_t;

#    error Cygwin/AMD64 not supported yet

#  elif defined(OPSYS_DARWIN)
    /** amd64, Darwin **/
#    define SIG_OVERFLOW		SIGFPE

    /* see /usr/include/mach/i386/thread_status.h */
#    define SIG_GetCode(info,scp)	((info)->si_code)
#    define SIG_GetPC(scp)		((scp)->uc_mcontext->__ss.__rip)
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext->__ss.__rip = (Addr_t) addr; }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->uc_mcontext->__ss.__r14 = 0; }

#  elif defined(OPSYS_FREEBSD)
    /** amd64, FreeBSD **/
#    define SIG_OVERFLOW		SIGFPE

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->uc_mcontext.mc_rip)
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext.mc_rip = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->uc_mcontext.mc_r14 = 0; }

     typedef void SigReturn_t;

#  elif defined(OPSYS_LINUX)
    /** amd64, LINUX **/
/* on linux, overflow can occur in two ways:
 *  (1) "int 4" instruction, which is invoked for addition and multiplication
 *      overflow, causes a SIGSEGV.
 *  (2) Division of the most negative number by -1 causes a SIGFPE.
 */

#    define SIG_OVERFLOW		SIGSEGV
#    define SIG_OVERFLOW2		SIGFPE

#    define SIG_GetCode(info,scp)	((scp)->uc_mcontext.gregs[REG_RIP])
/* for linux, SIG_GetCode simply returns the address of the fault */
#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[REG_RIP])
#    define SIG_SetPC(scp,addr)		{ (scp)->uc_mcontext.gregs[REG_RIP] = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->uc_mcontext.gregs[REG_R14] = 0; }

/* macro to check if SIGSEGV was caused by `int 4` instruction */
#    define SIG_IS_OVERFLOW_TRAP(sig,pc)					\
	(((sig) == SIG_OVERFLOW2) ||						\
	    ((((Byte_t*)pc)[-2] == 0xcd) && (((Byte_t*)pc)[-1] == 0x04)))

#  elif defined(OPSYS_NETBSD)
    /** amd64, NetBSD (version 3.x) **/
#    define SIG_OVERFLOW		SIGFPE

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((uc)->uc_mcontext.__gregs[_REG_RIP])
#    define SIG_SetPC(scp, addr)	{ (uc)->uc_mcontext.__gregs[_REG_RIP] = (Addr_t)(addr); }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->uc_mcontext.__gregs[_REG_R14] = 0; }

#    error NetBSD/AMD64 not supported yet

#  elif defined(OPSYS_OPENBSD)
    /** amd64, OpenBSD **/
#    define SIG_OVERFLOW		SIGFPE

#    define SIG_GetCode(info, scp)	(info)
#    define SIG_GetPC(scp)		((scp)->sc_rip)
#    define SIG_SetPC(scp, addr)	{ (scp)->sc_rip = (Addr_t)(addr); }
#    define SIG_SIG_ZeroLimitPtr(scp)	{ (scp)->sc_r14 = 0; }

     typedef void SigReturn_t;

#    error OpenBSD/AMD64 not supported yet

#  elif defined(OPSYS_SOLARIS)
     /** amd64, Solaris */

#    define SIG_GetPC(scp)		((scp)->uc_mcontext.gregs[EIP])
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext.gregs[EIP] = (Addr_t)(addr); }
/*#    define SIG_ZeroLimitPtr(scp)  { ML_X86Frame[LIMITPTR_X86OFFSET] = 0; }*/

#    error Solaris/AMD64 not supported yet

#  else
#    error "unknown OPSYS for amd64"
#  endif

#elif defined(ARCH_ARM64)

#  if defined(OPSYS_DARWIN)
    /** arm64, Darwin **/
      /* we do not define SIG_OVERFLOW, because we do not use hardware traps to
       * implement overflow.
       */
#    define SIG_GetPC(scp)		((scp)->uc_mcontext->__ss.__pc)
#    define SIG_SetPC(scp, addr)	{ (scp)->uc_mcontext->__ss.__pc = (Addr_t) addr; }
#    define SIG_ZeroLimitPtr(scp)	{ (scp)->uc_mcontext->__ss.__x[25] = 0; }

#  else
#    error "unknown OPSYS for arm64"
#  endif

#endif

#ifndef SIG_InitFPE
#define SIG_InitFPE()		/* nop */
#endif

#ifndef SIG_ResetFPE
#define SIG_ResetFPE(SCP)	/* nop */
#endif

#endif /* !_SIGNAL_SYSDEP_ */

