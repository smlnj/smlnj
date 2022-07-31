/* tcsetattr.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <termios.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_TTY_tcsetattr : int * int * termio_rep -> unit
 *    termio_rep = (SysWord.word * SysWord.word * SysWord.word * SysWord.word * string * SysWord.word * SysWord.word)
 *
 * Set parameters associated with tty.
 *
 * NOTE: the calls to cfset[io]speed by making the code more OS-dependent
 * and using the structure of struct termios.
 */
ml_val_t _ml_P_TTY_tcsetattr (ml_state_t *msp, ml_val_t arg)
{
    int			sts, fd = REC_SELINT(arg, 0);
    int			action = REC_SELINT(arg, 1);
    ml_val_t		termio_rep = REC_SEL(arg, 2);
    struct termios	data;
    ml_val_t		tmp;

    tmp = REC_SEL(termio_rep, 0);
    data.c_iflag = SYSWORD_MLtoC(tmp);
    tmp = REC_SEL(termio_rep, 1);
    data.c_oflag = SYSWORD_MLtoC(tmp);
    tmp = REC_SEL(termio_rep, 2);
    data.c_cflag = SYSWORD_MLtoC(tmp);
    tmp = REC_SEL(termio_rep, 3);
    data.c_lflag = SYSWORD_MLtoC(tmp);

    memcpy (data.c_cc, GET_SEQ_DATAPTR(void, REC_SEL(termio_rep, 4)), NCCS);

    tmp = REC_SEL(termio_rep, 5);
    sts = cfsetispeed (&data, SYSWORD_MLtoC(tmp));
    if (sts < 0) {
	return RAISE_SYSERR(msp, sts);
    }

    tmp = REC_SEL(termio_rep, 6);
    sts = cfsetospeed (&data, SYSWORD_MLtoC(tmp));
    if (sts < 0) {
	return RAISE_SYSERR(msp, sts);
    }

    sts = tcsetattr(fd, action, &data);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_TTY_tcsetattr */
