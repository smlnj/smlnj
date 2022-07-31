/* ml-fp.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: changes to this file must be tracked in ml-fp.c.
 */


#ifdef ARCH_X86

extern void Save_C_FPState();
extern void Restore_C_FPState();

extern void Save_ML_FPState();
extern void Restore_ML_FPState();

#else

#define Save_C_FPState()
#define Restore_C_FPState()
#define Save_ML_FPState()
#define Restore_ML_FPState()

#endif

/* end of ml-fp.h */

