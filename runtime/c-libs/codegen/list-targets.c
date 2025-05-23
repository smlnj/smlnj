/*! \file list-targets.c
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

/* _ml_CodeGen_listTargets : unit -> string list
 */
ml_val_t _ml_CodeGen_listTargets (ml_state_t *msp, ml_val_t arg)
{
    return llvm_listTargets (msp);

} /* end of _ml_CodeGen_listTargets */
