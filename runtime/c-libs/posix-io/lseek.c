/* lseek.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-unixdep.h"
#include INCLUDE_TYPES_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_lseek : int * Position.int * int -> Position.int
 *
 * Move read/write file pointer.
 */
ml_val_t _ml_P_IO_lseek (ml_state_t *msp, ml_val_t arg)
{
    Int_t       fd = REC_SELINT(arg, 0);
    ml_val_t	box_offset = REC_SEL(arg, 1);
    off_t	offset = (off_t)INT64_MLtoC(box_offset);
    off_t       pos;
    Int_t       whence = REC_SELINT(arg, 2);
    ml_val_t    box_pos;

    pos = lseek(fd, offset, whence);

    if (pos < 0) {
	RAISE_SYSERR (msp, (int)pos);
    }

    INT64_ALLOC(msp, box_pos, pos);

    return box_pos;

} /* end of _ml_P_IO_lseek */
