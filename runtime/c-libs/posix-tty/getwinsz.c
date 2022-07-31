/*! \file getwinsz.c
 *
 * \author John Reppy
 *
 * Runtime support for Basis Library proposal 2021-001 (Add `getWindowSz`
 * function to `Posix.TTY` structure).
 */

/*
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <sys/ioctl.h>
#include <termios.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "ml-values.h"

/* _ml_P_TTY_getwinsz : int -> (int * int) option
 */
ml_val_t _ml_P_TTY_getwinsz (ml_state_t *msp, ml_val_t arg)
{
#ifdef TIOCGWINSZ
    int fd, sts;
    struct winsize wsz;

    fd = INT_MLtoC(arg);
    sts = ioctl (fd, TIOCGWINSZ, &wsz);
    if (sts == 0) {
        ml_val_t result;
        OPTION_SOME(
            msp,
            result,
            ML_Alloc2(msp, INT_CtoML(wsz.ws_row), INT_CtoML(wsz.ws_col)));
        return result;
    }
    else {
        return OPTION_NONE;
    }
#else
    return OPTION_NONE;
#endif

} /* end of _ml_P_TTY_getwinsz */
