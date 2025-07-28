/*! \file fp-dep.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * System dependencies for floating-point interface.  This header file defines
 * a subset of the X3J11 specification for Floating-point C extensions (March
 * 29, 1995 version).  Eventually, we expect that most C compilers will support
 * this, and this header file won't be necessary.
 *
 * The following things are defined:
 *
 *				Rounding modes:
 *	FE_TONEAREST		  to nearest
 *	FE_UPWARD		  to +Inf
 *	FE_DOWNWARD		  to -Inf
 *	FE_TOWARDZERO		  to 0.0
 *
 *	fe_rnd_mode_t		The representation type of rounding modes.
 *
 *	fegetround()		Get rounding mode
 *	fesetround(rm)		Set rounding mode (return old mode)
 *
 *	RMODE_EQ(rm1, rm2)	Compare two rounding modes for equality.
 *
 *	RMODE_C_EQ_ML		Set, if the rounding mode values for C are
 *				the same as for SML (ignoring representation).
 */

#ifndef _FP_DEP_H_
#define _FP_DEP_H_

#include "ml-osdep.h"

#if defined(HAS_ANSI_C_FP_EXT)
#  include <fenv.h>
/* some compilers may ignore the fesetround() function if this is not ON */
#pragma STDC FENV_ACCESS ON

typedef int fe_rnd_mode_t;

#elif defined(OPSYS_AIX)
#  include <float.h>
#  ifndef FP_RND_RN
   /** Some gcc installations screw up the header files, so that <float.h>
    ** doesn't contain these definitions.
    **/
#    define FP_RND_RZ		0
#    define FP_RND_RN		1
#    define FP_RND_RP		2
#    define FP_RND_RM		3
#  endif
#  define FE_TONEAREST		FP_RND_RN
#  define FE_TOWARDZERO		FP_RND_RZ
#  define FE_UPWARD		FP_RND_RP
#  define FE_DOWNWARD		FP_RND_RM
typedef int fe_rnd_mode_t;
#  define fegetround()		fp_read_rnd()
#  define fesetround(RM)	fp_swap_rnd(RM)

#elif defined(OPSYS_FREEBSD)
#  include <floatingpoint.h>
#  define FE_TONEAREST		FP_RN
#  define FE_TOWARDZERO		FP_RZ
#  define FE_UPWARD		FP_RP
#  define FE_DOWNWARD		FP_RM
typedef int fe_rnd_mode_t;
#  define fegetround()		fpgetround()
#  define fesetround(RM)	fpsetround(RM)

#elif (defined(OPSYS_NETBSD) || defined(OPSYS_OPENBSD))
#  include <ieeefp.h>
#  define FE_TONEAREST		FP_RN
#  define FE_TOWARDZERO		FP_RZ
#  define FE_UPWARD		FP_RP
#  define FE_DOWNWARD		FP_RM
typedef int fe_rnd_mode_t;
#  define fegetround()		fpgetround()
#  define fesetround(RM)	fpsetround(RM)

#elif (defined(OPSYS_WIN32) || defined(OPSYS_CYGWIN))
/**
 ** Win32 can set (some) alternate math paramters, but then only by re-linking
 ** with different objects.  Best to do it by hand here as well.
 **/
#  define FE_TONEAREST		0
#  define FE_TOWARDZERO		3
#  define FE_UPWARD		2
#  define FE_DOWNWARD		1
typedef int fe_rnd_mode_t;
extern int fegetround (void);
extern int fesetround (int);

#elif defined(OPSYS_MKLINUX)
/* we will probably have to write some assembler to support this. */
#  define NO_ROUNDING_MODE_CTL

#elif defined(OPSYS_OSF1)
/* because of bugs in Digital's OS (versions earlier than V4.0), rounding
 * mode control cannot be supported.  Note that later versions of Digital
 * Unix (V4.0+) are called OPSYS_DUNIX.
 */
#  define NO_ROUNDING_MODE_CTL

#elif defined(OPSYS_SOLARIS)
#  include <ieeefp.h>
#  define FE_TONEAREST		FP_RN
#  define FE_TOWARDZERO		FP_RZ
#  define FE_UPWARD		FP_RP
#  define FE_DOWNWARD		FP_RM
typedef int fe_rnd_mode_t;
#  if defined(ARCH_X86)
  /* There is a bug in the Solaris X86 implementation of
   * fpgetround() and fpsetround(); we use the assembler code instead.
   */
extern int fegetround (void);
extern int fesetround (int);

#  else
#    define fegetround()          fpgetround()
#    define fesetround(RM)        fpsetround(RM)
#  endif

#elif defined(OPSYS_DARWIN) && defined(ARCH_PPC)
#   include <architecture/ppc/fp_regs.h>
#   define FE_TONEAREST RN_NEAREST
#   define FE_UPWARD RN_TOWARD_PLUS
#   define FE_DOWNWARD RN_TOWARD_MINUS
#   define FE_TOWARDZERO RN_TOWARD_ZERO
    typedef ppc_fp_rn_t  fe_rnd_mode_t;
    PVT fe_rnd_mode_t fegetround() { return(get_fp_scr()).rn; }
    PVT fe_rnd_mode_t fesetround(fe_rnd_mode_t rm) {
	ppc_fp_scr_t fpstate = get_fp_scr();
	fe_rnd_mode_t old = fpstate.rn;
	fpstate.rn = rm;
	set_fp_scr(fpstate);
	return (old);
    }
#else
#  error do not know about FP dependencies
#endif


#ifndef RMODE_EQ
#  define RMODE_EQ(RM1, RM2)	((RM1) == (RM2))
#endif


#ifndef RMODE_C_NEQ_ML
#  if ((FE_TONEAREST == 0) && (FE_TOWARDZERO == 1) && (FE_UPWARD == 2) && (FE_DOWNWARD == 3))
#    define RMODE_C_EQ_ML
#  endif
#endif

#endif /* !_FP_DEP_H_ */



