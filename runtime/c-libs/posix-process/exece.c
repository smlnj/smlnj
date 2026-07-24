/* exece.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_exece : string * string list * string list -> 'a
 *
 * Overlay a new process image, using specified environment.
 */
ml_val_t _ml_P_Process_exece (ml_state_t *msp, ml_val_t arg)
{
    int             sts;
    ml_val_t        path = REC_SEL(arg, 0);
    ml_val_t        arglst = REC_SEL(arg, 1);
    ml_val_t        envlst = REC_SEL(arg, 2);
    char            **argv, **envp;
    ml_val_t        p;
    char            **cp;
    int             nArgs;

    for (nArgs = 0, p = arglst; p != LIST_nil; p = LIST_tl(p)) {
        nArgs++;
    }

    cp = PTR_MLtoC(char *, ML_AllocRaw(msp, BYTES_TO_WORDS((nArgs + 1) * sizeof(char *))));
    argv = cp;
    for (p = arglst;  p != LIST_nil;  p = LIST_tl(p)) {
        *cp++ = STR_MLtoC(LIST_hd(p));
    }
    *cp++ = 0;  /* terminate the argv[] */

    envp = cp;
    for (p = envlst;  p != LIST_nil;  p = LIST_tl(p))
        *cp++ = STR_MLtoC(LIST_hd(p));
    *cp++ = 0;  /* terminate the envp[] */

    sts = execve(STR_MLtoC(path), argv, envp);

    CHK_RETURN (msp, sts)

} /* end of _ml_P_Process_exece */
