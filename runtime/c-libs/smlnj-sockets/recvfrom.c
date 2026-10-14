/* recvbuffrom.c
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_recvfrom : (sock * int * bool * bool) -> (Word8Vector.vector * addr)
 *
 * The arguments are: socket, number of bytes, peek flag, and OOB flag.  The
 * result is the vector of bytes read and the source address.
 */
ml_val_t _ml_Sock_recvfrom (ml_state_t *msp, ml_val_t arg)
{
    char	addrBuf[MAX_SOCK_ADDR_SZB];
    socklen_t	addrLen = MAX_SOCK_ADDR_SZB;
    int		sock = REC_SELINT(arg, 0);
    int		nbytes = REC_SELINT(arg, 1);
    int		flag = 0;
    ml_val_t	vec;
    int		n;

    if (REC_SEL(arg, 2) == ML_true) flag |= MSG_PEEK;
    if (REC_SEL(arg, 3) == ML_true) flag |= MSG_OOB;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw (msp, BYTES_TO_WORDS(nbytes));

    n = recvfrom (
	sock, PTR_MLtoC(char, vec), nbytes, flag,
	(struct sockaddr *)addrBuf, &addrLen);

    if (n < 0) {
	return RAISE_SYSERR(msp, sts);
    } else if (n == 0) {
	return ML_string0;
    } else {
	ml_val_t addr, res;

        if (n < nbytes) {
            /* we need to shrink the vector */
            ML_ShrinkRaw (msp, vec, BYTES_TO_WORDS(n));
        }

        /* the header for the data vector */
	SEQHDR_ALLOC (msp, res, DESC_string, vec, n);

        /* allocate the address in the heap */
	ml_val_t data = ML_CData (msp, addrBuf, addrLen);
	SEQHDR_ALLOC (msp, addr, DESC_word8vec, data, addrLen);

	REC_ALLOC2(msp, res, res, addr);

	return res;
    }

} /* end of _ml_Sock_recvfrom */

