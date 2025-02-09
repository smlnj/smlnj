/* gethostname.c
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

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/* _ml_NetDB_gethostname : unit -> string
 */
ml_val_t _ml_NetDB_gethostname (ml_state_t *msp, ml_val_t arg)
{
    UNUSED_UNIT_PARAM(arg);

    char hostname[MAXHOSTNAMELEN];

    if (gethostname (hostname, MAXHOSTNAMELEN) == -1)
        return RAISE_SYSERR(msp, sts);
    else
        return ML_CString(msp, hostname);

} /* end of _ml_NetDB_gethostname */
