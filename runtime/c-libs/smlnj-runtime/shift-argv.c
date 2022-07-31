/* shift-argv.c
 *
 * COPYRIGHT (c) 2007 by The Fellowship of SML/NJ
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Proc_shift_argv:
 */
ml_val_t _ml_Proc_shift_argv (ml_state_t *msp, ml_val_t arg)
{
  if (*CmdLineArgs != NIL(char *))
    ++CmdLineArgs;

  return ML_unit;

} /* end of _ml_Proc_shift_argv */
