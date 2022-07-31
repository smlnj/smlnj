/*! \file codegen.h
 *
 * \author John Reppy
 *
 * C interface to LLVM code generator.
 */

/*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _CODEGEN_H_
#define _CODEGEN_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "ml-objects.h"

/* return an SML `string list` of the names of the supported target architectures
 */
ml_val_t llvm_listTargets (ml_state_t *msp);

/*  set the target architecture.  This call returns `true` when there
 * is an error and `false` otherwise.
 */
ml_val_t llvm_setTarget (const char *target);

/* llvm_codegen:
 *
 * Given the source-file name and ASDL pickle of the CFG IR, generate
 * native machine code and return the corresponding ML code object and
 * entry-point offset as a heap-allocated pair.
 */
ml_val_t llvm_codegen (ml_state_t *msp, const char *src, const char *pkl, size_t szb);

#ifdef __cplusplus
}
#endif

#endif // !_CODEGEN_H_
