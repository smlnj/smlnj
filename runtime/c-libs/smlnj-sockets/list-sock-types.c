/* list-sock-types.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "sock-util.h"
#include "cfun-proto-list.h"
#include "ml-c.h"

/* _ml_Sock_listsocktypes : unit -> CInterface.system_const list
 *
 * Return a list of the known socket types (this may contain unsupported
 * families).
 */
ml_val_t _ml_Sock_listsocktypes (ml_state_t *msp, ml_val_t arg)
{
    UNUSED_UNIT_PARAM(arg);
    return ML_SysConstList (msp, &_Sock_Type);

} /* end of _ml_Sock_listsocktypes */
