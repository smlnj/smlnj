/* fchown.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <unistd.h>

/* _ml_P_FileSys_fchown : (int * SysWord.word * SysWord.word) -> unit
 *                         fd    uid            gid
 *
 * Change owner and group of file given a file descriptor for it.
 */
ml_val_t _ml_P_FileSys_fchown (ml_state_t *msp, ml_val_t arg)
{
    int		fd =  REC_SELINT(arg, 0);
    ml_val_t	ml_uid = REC_SEL(arg, 1);
    uid_t	uid = SYSWORD_MLtoC(ml_uid);
    ml_val_t	ml_gid = REC_SEL(arg, 2);
    gid_t	gid = SYSWORD_MLtoC(ml_gid);
    int		sts;

    sts = fchown (fd, uid, gid);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_fchown */
