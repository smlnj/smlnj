/*! \file generate.c
 *
 * \author John Reppy
 *
 * SML callable wrapper for the LLVM code generator.  This code is C++, but the
 * exported functions are marked as "C" functions to avoid name mangling.
 */

/*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-state.h"
#include "cfun-proto-list.h"
#include "codegen.h"

/* _ml_CodeGen_generate : string * Word8Vector.vector * bool -> Word8Vector.vector * int
 *
 * Given the source-file name and ASDL pickle of the CFG IR, generate
 * native machine code and return the corresponding code object and
 * entry-point offset.
 */
ml_val_t _ml_CodeGen_generate (ml_state_t *msp, ml_val_t arg)
{
  /* get the source name as a C string */
    ml_val_t mlSrc = REC_SEL(arg, 0);
    char *src = GET_SEQ_DATAPTR(char, mlSrc);

  /* get the pickle data and size */
    ml_val_t mlPkl = REC_SEL(arg, 1);
    char *pkl = GET_SEQ_DATAPTR(char, mlPkl);
    size_t pklSzb = GET_SEQ_LEN(mlPkl);

/* TODO: get the verifyLLVM flag */

    return llvm_codegen (msp, src, pkl, pklSzb);

} /* end of _ml_CodeGen_generate */
