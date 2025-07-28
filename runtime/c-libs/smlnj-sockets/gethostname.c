/*! \file gethostname.c
 *
 * \author John Reppy
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

#ifndef MAXHOSTNAMELEN
#  ifdef _POSIX_HOST_NAME_MAX
#    define MAXHOSTNAMELEN _POSIX_HOST_NAME_MAX
#  else
#    define MAXHOSTNAMELEN 256
#  endif
#endif

/* _ml_NetDB_gethostname : unit -> string
 */
ml_val_t _ml_NetDB_gethostname (ml_state_t *msp, ml_val_t arg)
{
    char	hostname[MAXHOSTNAMELEN];

/* NOTE: we could use `sysconf(_SC_HOST_NAME_MAX)` to determine the maximum
 * hostname length.
 */
    if (gethostname (hostname, MAXHOSTNAMELEN) == -1) {
	return RAISE_SYSERR(msp, sts);
    } else {
	return ML_CString(msp, hostname);
    }

} /* end of _ml_NetDB_gethostname */
