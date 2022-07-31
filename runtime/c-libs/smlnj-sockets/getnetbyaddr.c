/* getnetbyaddr.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

#if defined(__CYGWIN32__)
#undef getnetbyaddr
#define getnetbyaddr(x,y) NULL
#endif

/* _ml_NetDB_getnetbyaddr
 *     : (sysword * addr_family) -> (string * string list * addr_family * sysword) option
 */
ml_val_t _ml_NetDB_getnetbyaddr (ml_state_t *msp, ml_val_t arg)
{
#if defined(OPSYS_WIN32)
  /* FIXME:  getnetbyaddr() does not seem to exist under Windows.  What is
     the equivalent? */
  return RAISE_ERROR(msp, "<getnetbyaddr not implemented>");
#else
    unsigned long   net = REC_SELWORD(arg, 0);
    int		    type = REC_SELINT(arg, 1);

    return _util_NetDB_mknetent (msp, getnetbyaddr(net, type));
#endif
} /* end of _ml_NetDB_getnetbyaddr */
