/* tmpname.c
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

#if defined(HAS_MKSTEMP) && defined(P_tmpdir)
#  define TEMPLATE	P_tmpdir "/SMLNJ-XXXXXX"
#endif

/* _ml_OS_tmpname:
 */
ml_val_t _ml_OS_tmpname (ml_state_t *msp, ml_val_t arg)
{
#if defined(HAS_MKSTEMP) && defined(P_tmpdir)

  /* mkstemp was added to the IEEE Std 1003.1 in 2004, so most systems should support it */
    char	template[sizeof(TEMPLATE)];
    int		sts;

    strcpy (template, TEMPLATE);
    sts = mkstemp (template);

    if (sts < 0) {
	return RAISE_SYSERR(msp, sts);
    }
    else {
	close (sts);  /* close the file descriptor */
	return ML_CString (msp, template);
    }

#else /* for old systems */
    char	buf[L_tmpnam];

    tmpnam (buf);

    return ML_CString (msp, buf);

#endif

} /* end of _ml_OS_tmpname */

