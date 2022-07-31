/*! \file swap-bytes.c
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"

#ifdef BYTE_ORDER_LITTLE

/* SwapBytes32:
 */
Unsigned32_t SwapBytes32 (Unsigned32_t x)
{
    unsigned int	b0 = x & 0x000000FF;
    unsigned int	b1 = x & 0x0000FF00;
    unsigned int	b2 = x & 0x00FF0000;
    unsigned int	b3 = x & 0xFF000000;

    return ((b0 << 24) | (b1 << 8) | (b2 >> 8) | (b3 >> 24));

} /* end of SwapBytes */

#endif /* BYTE_ORDER_LITTLE */
