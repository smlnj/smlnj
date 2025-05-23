/*! \file set-target.c
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-state.h"
#include "cfun-proto-list.h"
#include "codegen.h"

/* _ml_CodeGen_setTarget : string option -> bool
 *
 * Sets the code generator target; use `NONE` to specify the host target.  Returns
 * `true` if there was an error in setting the target.
 */
ml_val_t _ml_CodeGen_setTarget (ml_state_t *msp, ml_val_t arg)
{
    if (arg == OPTION_NONE) {
        return llvm_setTarget(NIL(const char *));
    }
    else {
        return llvm_setTarget (STR_MLtoC(OPTION_get(arg)));
    }

} /* end of _ml_CodeGen_setTarget */
