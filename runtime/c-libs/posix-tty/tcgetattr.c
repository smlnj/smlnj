/* tcgetattr.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include INCLUDE_TIME_H
#include <termios.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_TTY_tcgetattr : int -> termio_rep
 *    termio_rep = (SysWord.word * SysWord.word * SysWord.word * SysWord.word * string * SysWord.word * SysWord.word)
 *
 * Get parameters associated with tty.
 *
 * NOTE: the calls to cfget[io]speed by making the code more OS-dependent
 * and using the structure of struct termios.
 */
ml_val_t _ml_P_TTY_tcgetattr (ml_state_t *msp, ml_val_t arg)
{
    int			sts, fd = INT_MLtoC(arg);
    ml_val_t		iflag, oflag, cflag, lflag;
    ml_val_t		cc, ispeed, ospeed, obj;
    struct termios	data;

    sts = tcgetattr(fd, &data);

    if (sts < 0) {
	return RAISE_SYSERR(msp, sts);
    }

  /* allocate the vector; note that this might cause a GC */
    cc = ML_AllocString (msp, NCCS);
    memcpy (GET_SEQ_DATAPTR(void, cc), data.c_cc, NCCS);

    SYSWORD_ALLOC (msp, iflag, data.c_iflag);
    SYSWORD_ALLOC (msp, oflag, data.c_oflag);
    SYSWORD_ALLOC (msp, cflag, data.c_cflag);
    SYSWORD_ALLOC (msp, lflag, data.c_lflag);
    SYSWORD_ALLOC (msp, ispeed, cfgetispeed (&data));
    SYSWORD_ALLOC (msp, ospeed, cfgetospeed (&data));

    ML_AllocWrite (msp, 0, MAKE_DESC(DTAG_record, 7));
    ML_AllocWrite (msp, 1, iflag);
    ML_AllocWrite (msp, 2, oflag);
    ML_AllocWrite (msp, 3, cflag);
    ML_AllocWrite (msp, 4, lflag);
    ML_AllocWrite (msp, 5, cc);
    ML_AllocWrite (msp, 6, ispeed);
    ML_AllocWrite (msp, 7, ospeed);
    obj = ML_Alloc(msp, 7);

    return obj;

} /* end of _ml_P_TTY_tcgetattr */
