/*! \file alloc-code.c
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 */

#include "cache-flush.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_RunT_alloc_code : int -> Word8Array.array
 *
 * Allocate a code object of the given size.
 *
 * Note: Generating the name string within the code object is now
 *       part of the code generator's responsibility.
 */
ml_val_t _ml_RunT_alloc_code (ml_state_t *msp, ml_val_t arg)
{
    Die("unexpected call to _ml_RunT_alloc_code\n");

} /* end of _ml_RunT_alloc_code */
