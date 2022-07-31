/* openf.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_openf : (string * SysWord.word * SysWord.word) -> int
 *                        name     flags          mode
 *
 * Open a file and return the file descriptor.
 */
ml_val_t _ml_P_FileSys_openf (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	path = REC_SEL(arg, 0);
    ml_val_t	ml_flags = REC_SEL(arg, 1);
    int		flags = SYSWORD_MLtoC(ml_flags);
    ml_val_t	ml_mode = REC_SEL(arg, 2);
    int		mode = SYSWORD_MLtoC(ml_mode);
    int		fd;

    fd = open (STR_MLtoC(path), flags, mode);

    CHK_RETURN(msp, fd)

} /* end of _ml_P_FileSys_openf */
