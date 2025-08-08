/*! \file copy-loop.h
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _COPY_LOOP_
#define _COPY_LOOP_

/* a simple loop, since the optimizer will unroll it */
STATIC_INLINE void Copy (ml_val_t *src, ml_val_t *dst, int len)
{
    for (int i = 0;  i < len;  ++i) {
	dst[i] = src[i];
    }
}

#define COPYLOOP(SRC,DST,LEN)	Copy(SRC,DST,LEN)

#endif /* !_COPY_LOOP_ */
