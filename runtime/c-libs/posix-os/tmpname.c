/* tmpname.c
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include <limits.h>
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

#ifndef PATH_MAX
#define PATH_MAX 256
#endif

/* _ml_OS_tmpname:
 */
ml_val_t _ml_OS_tmpname (ml_state_t *msp, ml_val_t arg)
{
#if defined(HAS_MKSTEMP) && defined(P_tmpdir)

  /* mkstemp was added to the IEEE Std 1003.1 in 2004, so most systems should support it */
    char template[PATH_MAX];
    int sts;
    const char *tmpdir = getenv ("TMPDIR");

    if (tmpdir != NIL(char *)) {
        int r = snprintf (template, PATH_MAX, "%s/SMLNJ-XXXXXX", tmpdir);
        if (r < sizeof(template)) {
            sts = mkstemp (template);
            if (sts >= 0) {
                close (sts);
                return ML_CString(msp, template);
            }
        }
    }

    strcpy (template, P_tmpdir "/SMLNJ-XXXXXX");
    sts = mkstemp (template);
    if (sts < 0) {
        return RAISE_SYSERR(msp, sts);
    } else {
        close (sts);
        return ML_CString (msp, template);
    }

#else /* for old systems */
    char	buf[L_tmpnam];

    tmpnam (buf);

    return ML_CString (msp, buf);

#endif

} /* end of _ml_OS_tmpname */

