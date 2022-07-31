/* dlsym.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifdef OPSYS_WIN32
# include <windows.h>
extern void dlerror_set (const char *fmt, const char *s);
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
/* _ml_P_Dynload_dlsym : Word32.word * string -> Word32.word
 *
 * Extract symbol from dynamically loaded library.
 */
ml_val_t _ml_U_Dynload_dlsym (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t ml_handle = REC_SEL (arg, 0);
    char *symname = STR_MLtoC (REC_SEL (arg, 1));
    void *handle = (void *) (WORD_MLtoC (ml_handle));
    void *addr;
    ml_val_t res;

#ifdef OPSYS_WIN32
    addr = GetProcAddress (handle, symname);
    if (addr == NULL && symname != NULL) {
        dlerror_set ("Symbol `%s' not found", symname);
    }
#else
    addr = dlsym (handle, symname);
#endif

    WORD_ALLOC (msp, res, (Word_t) addr);
    return res;
}
