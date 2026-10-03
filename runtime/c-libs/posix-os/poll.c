/*! \file poll.c
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The run-time code for OS.IO.poll.  This function has the signature
 *
 *      (int * word ref) list * time option -> bool
 *
 * where the first argument is a list of pairs of file descriptors and descriptor
 * refs.  The file descriptors are guaranteed to be unique.  The function returns
 * `true` if a queried descriptor is matched and `false` if it times out.
 */

#include "ml-unixdep.h"
#if defined(HAS_SELECT)
#  include INCLUDE_TYPES_H
#  include INCLUDE_TIME_H
#elif defined(HAS_POLL)
#  include <stropts.h>
#  include <poll.h>
#else
#  error no support for I/O polling
#endif
#include INCLUDE_TIME_H
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* bit masks for polling descriptors (see src/sml-nj/boot/Unix/os-io.sml) */
#define RD_BIT          0x1
#define WR_BIT          0x2
#define ERR_BIT         0x4

PVT ml_val_t ML_Poll (ml_state_t *msp, ml_val_t pollList, struct timeval *timeout);


/* _ml_OS_poll : ((int * word ref) list * (Int32.int * int) option) -> bool
 */
ml_val_t _ml_OS_poll (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t        pollList = REC_SEL(arg, 0);
    ml_val_t        timeout  = REC_SEL(arg, 1);
    struct timeval  tv, *tvp;

    if (timeout == OPTION_NONE) {
        tvp = NIL(struct timeval *);
    } else {
        timeout         = OPTION_get(timeout);
        tv.tv_sec       = REC_SELINT32(timeout, 0);
        tv.tv_usec      = REC_SELINT(timeout, 1);
        tvp = &tv;
    }

    return ML_Poll (msp, pollList, tvp);

} /* end of _ml_OS_poll */


#ifdef HAS_POLL

#ifdef POLLMSG
#define POLL_ERROR      (POLLRDBAND|POLLPRI|POLLHUP|POLLMSG)
#else
#define POLL_ERROR      (POLLRDBAND|POLLPRI|POLLHUP)
#endif

/* ML_Poll:
 *
 * The version of the polling operation for systems that provide SVR4 polling.
 */
PVT ml_val_t ML_Poll (ml_state_t *msp, ml_val_t pollList, struct timeval *timeout)
{
    int             tout, sts;
    struct pollfd   *fds, *fdp;
    int             nfds, flag;
    ml_val_t        l, item, flagRef;

    if (timeout == NIL(struct timeval *)) {
        tout = -1;
    } else {
      /* convert to miliseconds */
        tout = (timeout->tv_sec * 1000) + (timeout->tv_usec / 1000);
    }

    /* count the number of polling items */
    for (l = pollList, nfds = 0;  l != LIST_nil;  l = LIST_tl(l)) {
        nfds++;
    }

    /* allocate the fds vector */
    fds = NEW_VEC(struct pollfd, nfds);
    CLEAR_MEM (fds, sizeof(struct pollfd)*nfds);

    /* initialize the polling descriptors */
    for (l = pollList, fdp = fds;  l != LIST_nil;  l = LIST_tl(l), fdp++) {
        item = LIST_hd(l);
        fdp->fd = REC_SELINT(item, 0);
        flag = INT_MLtoC(DEREF(REC_SEL(item, 1)));
        if ((flag & RD_BIT) != 0)
            fdp->events |= POLLIN;
        if ((flag & WR_BIT) != 0)
            fdp->events |= POLLOUT;
        if ((flag & ERR_BIT) != 0)
            fdp->events |= POLL_ERROR;
    }

    sts = poll (fds, nfds, tout);

    if (sts < 0) {
        FREE(fds);
        return RAISE_SYSERR(msp, sts);
    } else if (sts == 0) {
        /* timeout, so return `false` */
        return ML_false;
    } else {
        for (l = pollList, fdp = fds;  l != LIST_nil;  l = LIST_tl(l), fdp++) {
            item = LIST_hd(l);
            fdp->fd = REC_SELINT(item, 0);
            flagRef = REC_SEL(item, 1);
            flag = 0;
            if (fdp->revents != 0) {
                if ((fdp->revents & POLLIN) != 0) {
                    flag |= RD_BIT;
                }
                if ((fdp->revents & POLLOUT) != 0) {
                    flag |= WR_BIT;
                }
                if ((fdp->revents & POLL_ERROR) != 0) {
                    flag |= ERR_BIT;
                }
            }
            /* update the flag reference with the resulting bits */
            ASSIGN(flagRef, INT_CtoML(resFlag));
        }
        FREE(fds);
        return ML_true;
    }

} /* end of ML_Poll */

#else /* HAS_SELECT */

/* ML_Poll:
 *
 * The version of the polling operation for systems that provide BSD select.
 */
PVT ml_val_t ML_Poll (ml_state_t *msp, ml_val_t pollList, struct timeval *timeout)
{
    fd_set      rset, wset, eset;
    fd_set      *rfds, *wfds, *efds;
    int         maxFD, sts, fd, flag;
    ml_val_t    l, item, flagRef;

    rfds = wfds = efds = NIL(fd_set *);
    maxFD = 0;
    for (l = pollList;  l != LIST_nil;  l = LIST_tl(l)) {
        item = LIST_hd(l);
        fd = REC_SELINT(item, 0);
        flag = INT_MLtoC(DEREF(REC_SEL(item, 1)));
SayDebug("# poll: fd = %d, flg = %#0x\n", fd, flag);
        if ((flag & RD_BIT) != 0) {
            if (rfds == NIL(fd_set *)) {
                rfds = &rset;
                FD_ZERO(rfds);
            }
            FD_SET (fd, rfds);
        }
        if ((flag & WR_BIT) != 0) {
            if (wfds == NIL(fd_set *)) {
                wfds = &wset;
                FD_ZERO(wfds);
            }
            FD_SET (fd, wfds);
        }
        if ((flag & ERR_BIT) != 0) {
            if (efds == NIL(fd_set *)) {
                efds = &eset;
                FD_ZERO(efds);
            }
            FD_SET (fd, efds);
        }
        if (fd > maxFD) maxFD = fd;
    }

    sts = select (maxFD+1, rfds, wfds, efds, timeout);

    if (sts < 0) {
        return RAISE_SYSERR(msp, sts);
    } else if (sts == 0) {
        /* timeout, so return `false` */
        return ML_false;
    } else {
        for (l = pollList;  l != LIST_nil;  l = LIST_tl(l)) {
            item = LIST_hd(l);
            fd = REC_SELINT(item, 0);
            flagRef = REC_SEL(item, 1);
            flag = 0;
            if ((rfds != NIL(fd_set *)) && FD_ISSET(fd, rfds)) {
                flag |= RD_BIT;
            }
            if ((wfds != NIL(fd_set *)) && FD_ISSET(fd, wfds)) {
                flag |= WR_BIT;
            }
            if ((efds != NIL(fd_set *)) && FD_ISSET(fd, efds)) {
                flag |= ERR_BIT;
            }
            /* update the flag reference with the resulting bits */
            ASSIGN(flagRef, INT_CtoML(flag));
        }

        return ML_true;
    }

} /* end of ML_Poll */

#endif

