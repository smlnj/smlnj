/* getnetbyname.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

#if defined(__CYGWIN32__)
#undef getnetbyname
#define getnetbyname(x) NULL
#endif

/* _ml_NetDB_getnetbyname : string -> (string * string list * addr_family * sysword) option
 */
ml_val_t _ml_NetDB_getnetbyname (ml_state_t *msp, ml_val_t arg)
{
#if defined(OPSYS_WIN32)
  /* FIXME:  getnetbyname() does not seem to exist under Windows.  What is
     the equivalent? */
  return RAISE_ERROR(msp, "<getnetbyname not implemented>");
#else
    return _util_NetDB_mknetent (msp, getnetbyname (STR_MLtoC(arg)));
#endif
} /* end of _ml_NetDB_getnetbyname */
