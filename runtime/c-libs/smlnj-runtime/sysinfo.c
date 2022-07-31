/*! \file sysinfo.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * General interface to query system properties.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "machine-id.h"

#if defined(OPSYS_UNIX)
#  include "ml-unixdep.h"  /* for OS_NAME */
#elif defined(OPSYS_WIN32)
#  define OS_NAME "Win32"
#endif

#define STREQ(s1, s2)	(strcmp((s1), (s2)) == 0)


#define FALSE_VALUE	"NO"
#define TRUE_VALUE	"YES"


/* _ml_RunT_sysinfo : string -> string option
 *
 * Current queries:
 *   "OS_NAME"
 *   "OS_VERSION"	(not supported)
 *   "ARCH"
 *   "ARCH_ARCH"	(deprecated; use "ARCH")
 *   "TARGET_ARCH"	(deprecated; use "ARCH")
 *   "HAS_SOFT_POLL"
 *   "HAS_MP"
 *   "HEAP_SUFFIX"
 */
ml_val_t _ml_RunT_sysinfo (ml_state_t *msp, ml_val_t arg)
{
    char	*name = STR_MLtoC(arg);
    ml_val_t	res;

    if (STREQ("OS_NAME", name))
	res = ML_CString(msp, OS_NAME);
    else if (STREQ("OS_VERSION", name))
	res = ML_CString(msp, "<unknown>");
    else if (STREQ("HEAP_SUFFIX", name))
        res = ML_CString(msp, MACHINE_ID "-" OPSYS_ID);
    else if (STREQ("ARCH_NAME", name))
#if   defined(ARCH_ARM64)
        res = ML_CString(msp, "ARM64");
#elif   defined(ARCH_AMD64)
	res = ML_CString(msp, "AMD64");
#else
	res = ML_CString(msp, "<unknown>");
#endif
    else
	return OPTION_NONE;

    OPTION_SOME(msp, res, res);

    return res;

} /* end of _ml_RunT_sysinfo */
