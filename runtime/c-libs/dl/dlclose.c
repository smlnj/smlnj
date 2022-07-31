/*! \file dlclose.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifdef OPSYS_WIN32
# include <windows.h>
#else
# include "ml-unixdep.h"
# include <dlfcn.h>
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* 64BIT: use c_pointer type for handles */
/* _ml_P_Dynload_dlclose : Word32.word -> unit
 *
 * Close dynamically loaded library.
 */
ml_val_t _ml_U_Dynload_dlclose (ml_state_t *msp, ml_val_t ml_handle)
{
    void *handle = (void *) (WORD_MLtoC (ml_handle));

#ifdef OPSYS_WIN32
    (void) FreeLibrary (handle);
#else
    (void) dlclose (handle);
#endif

    return ML_unit;
}
