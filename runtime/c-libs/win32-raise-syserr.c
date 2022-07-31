/* win32-raise-syserr.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "ml-c.h"

/* RaiseSysError:
 *
 * Raise the ML exception SysErr, which has the spec:
 *
 *    exception SysErr of (string * syserror option)
 *
 * We use the last win32-api error value as the syserror; eventually that
 * will be represented by an (int * string) pair.  If alt_msg is non-zero,
 * then use it as the error string and use NONE for the syserror.
 */
ml_val_t RaiseSysError (ml_state_t *msp, const char *altMsg, const char *at)
{
    ml_val_t	    s, syserror, arg, exn, atStk;
    const char	    *msg;
    char	    buf[32];
    int             ml_errno = -1;

    if (altMsg != NIL(char *)) {
	msg = altMsg;
	syserror = OPTION_NONE;
    }
    else {
        ml_errno = (int) GetLastError();
	sprintf(buf, "<win32 error code %d>", ml_errno);
	msg = buf;
	OPTION_SOME(msp, syserror, INT_CtoML(ml_errno));
    }

    s = ML_CString (msp, msg);
    if (at != NIL(char *)) {
	ml_val_t atMsg = ML_CString (msp, at);
	LIST_cons(msp, atStk, atMsg, LIST_nil);
    }
    else
	atStk = LIST_nil;
    REC_ALLOC2 (msp, arg, s, syserror);
    EXN_ALLOC (msp, exn, PTR_CtoML(SysErrId), arg, atStk);

    RaiseMLExn (msp, exn);

    return exn;

} /* end of RaiseSysError */
