/*! \file alarm.c
 *
 * \brief Implementation of OS.Process.alarm function.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_alarm : word64 -> word64
 *
 * Set a process alarm clock
 */
ml_val_t _ml_P_Process_alarm (ml_state_t *msp, ml_val_t arg)
{
/* TODO: use setitimer to get finer-grain periods */
    unsigned int t = (unsigned int)(WORD64_MLtoC(arg) / NS_PER_SEC);

    t = alarm(t);

    return ML_AllocWord64(msp, NS_PER_SEC * (Unsigned64_t)t);

} /* end of _ml_P_Process_alarm */
