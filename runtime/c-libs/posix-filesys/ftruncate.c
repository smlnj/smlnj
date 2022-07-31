/* ftruncate.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <sys/types.h>
#include <sys/stat.h>

/* _ml_P_FileSys_ftruncate_64 : (int * Position.int) -> unit
 *
 * Truncate or extend a file to a specified length
 */
ml_val_t _ml_P_FileSys_ftruncate (ml_state_t *msp, ml_val_t arg)
{
    int		fd = REC_SELINT(arg, 0);
    ml_val_t	boxed_len = REC_SEL(arg, 1);
    off_t	len = (off_t)INT64_MLtoC(boxed_len);
    int		sts;

    sts = ftruncate (fd, len);

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_ftruncate */
