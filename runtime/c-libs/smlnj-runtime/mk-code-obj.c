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

/* _ml_RunT_mk_code_obj : Word8Vector.vector -> Word8Vector.vector
 *
 * Allocate an initialize an executable code object.
 */
ml_val_t _ml_RunT_mk_code_obj (ml_state_t *msp, ml_val_t arg)
{
    int nbytes = GET_SEQ_LEN(arg);
    ml_val_t	code, res;

    ENABLE_CODE_WRITE
        code = ML_AllocCode (msp,GET_SEQ_DATAPTR(void, arg),  nbytes);
    DISABLE_CODE_WRITE

    SEQHDR_ALLOC(msp, res, DESC_word8vec, code, nbytes);

    return res;

} /* end of _ml_RunT_alloc_code */
