/* rewinddir.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <dirent.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_rewinddir : object -> unit
 *
 * Rewind a directory stream.
 */
ml_val_t _ml_P_FileSys_rewinddir (ml_state_t *msp, ml_val_t arg)
{
    UNUSED(msp);
    rewinddir(PTR_MLtoC(DIR, arg));

    return ML_unit;

} /* end of _ml_P_FileSys_rewinddir */
