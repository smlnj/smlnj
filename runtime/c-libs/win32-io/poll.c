/*! \file poll.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * crude implementation of a polling function on Windows
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#include "win32-fault.h"

/* bit masks for polling descriptors (see src/sml-nj/boot/Unix/os-io.sml) */
#define RD_BIT		0x1
#define WR_BIT		0x2
#define ERR_BIT		0x4

/* _ml_win32_OS_poll : (handle * word) list * (int * word) list * Word32.int option
 *                     -> (handle * word) list * (int * word) list
 */
ml_val_t _ml_win32_OS_poll (ml_state_t *msp, ml_val_t arg)
{
    DWORD dwMilliseconds;
    ml_val_t pollList = REC_SEL(arg,0);
    ml_val_t pollSockList = REC_SEL(arg,1);
    ml_val_t timeout = REC_SEL(arg,2);
    int sec,usec;
    ml_val_t l,item;
    ml_val_t hList, sList, resTuple;
    HANDLE handle, *hArray;
    fd_set read, write, err;
    int fd, flag;
    struct timeval tv, *tvp;
    int result;

    int count, index;

  /* first, convert timeout to milliseconds */
    if (timeout == OPTION_NONE) {
	dwMilliseconds = INFINITE;
    }
    else {
	timeout = OPTION_get(timeout);
	dwMilliseconds = WORD32_MLtoC(timeout);
    }

  /* count number of handles */
    for (l = pollList, count = 0; l != LIST_nil; l = LIST_tl(l)) {
	count++;
    }

  /* allocate array of handles */
    hArray = NEW_VEC (HANDLE,count);

  /* initialize the array */
    for (l = pollList, index = 0; l != LIST_nil; l = LIST_tl(l)) {
	item = LIST_hd (l);
	handle = HANDLE_MLtoC(REC_SEL(item, 0));
	hArray[index++] = handle;
    }

  /* generalized poll to see if anything is available */
    result = WaitForMultipleObjects (count, hArray, FALSE,dwMilliseconds);
    hList = LIST_nil;
    if (!((result==WAIT_FAILED) || (result==WAIT_TIMEOUT))) {
      /* at least one handle was ready. Find all that are */
	for (l=pollList; l!=LIST_nil; l=LIST_tl(l)) {
	    item = LIST_hd (l);
	    handle = HANDLE_MLtoC(REC_SEL(item, 0));
	    result = WaitForSingleObject (handle, 0);
	    if ((result == WAIT_FAILED) || (result == WAIT_TIMEOUT)) continue;
	    LIST_cons (msp, hList, item, hList);
	}
    }

    FREE(hArray);

  /* SOCKETS */
  /* count number of handles and init the fdsets */
    FD_ZERO(&read);
    FD_ZERO(&write);
    FD_ZERO(&err);
    for (l=pollSockList,count=0; l!=LIST_nil; l=LIST_tl(l)) {
	count++;
	item = LIST_hd (l);
	fd = REC_SELINT(item, 0);
	flag = REC_SELINT(item, 1);
	if ((flag & RD_BIT) != 0) {
	    FD_SET(fd, &read);
	}
	if ((flag & WR_BIT) != 0) {
	    FD_SET(fd, &write);
	}
	if ((flag & ERR_BIT) != 0) {
	    FD_SET(fd, &err);
	}
    }

    if (timeout == OPTION_NONE) {
	tvp = NIL(struct timeval *);
    } else {
	tv.tv_sec	= dwMilliseconds / 1000;
	tv.tv_usec	= (dwMilliseconds % 1000) * 1000;
	tvp = &tv;
    }

    sList = LIST_nil;

    if (count > 0) {
	result = select (count, &read, &write, &err, tvp);
	if (result < 0) {
	    return RAISE_SYSERR(msp, sts);
	}
	else if (result > 0) {
	    ml_val_t	*resVec = NEW_VEC(ml_val_t, result);
	    int		i, resFlag;

	    for (i = 0, l = pollSockList;  l != LIST_nil;  l = LIST_tl(l)) {
		item    = LIST_hd(l);
		fd      = REC_SELINT(item, 0);
		flag    = REC_SELINT(item, 1);
		resFlag	= 0;
		if (((flag & RD_BIT) != 0) && FD_ISSET(fd, &read)) resFlag |= RD_BIT;
		if (((flag & WR_BIT) != 0) && FD_ISSET(fd, &write)) resFlag |= WR_BIT;
		if (((flag & ERR_BIT) != 0) && FD_ISSET(fd, &err)) resFlag |= ERR_BIT;
		if (resFlag != 0) {
		    REC_ALLOC2 (msp, item, INT_CtoML(fd), INT_CtoML(resFlag));
		    resVec[i++] = item;
		}
	    }

	    ASSERT(i == result);

	    for (i = result-1, sList = LIST_nil;  i >= 0;  i--) {
		item = resVec[i];
		LIST_cons (msp, sList, item, sList);
	    }

	    FREE(resVec);
	}
    }


    REC_ALLOC2(msp, resTuple, hList, sList)
    return resTuple;
}

/* end of poll.c */
