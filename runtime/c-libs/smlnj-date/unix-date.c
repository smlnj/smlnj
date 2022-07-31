/*! \file unix-date.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "unix-date.h"

/* allocate a 9-tuple for a `struct tm` value */
ml_val_t _ml_alloc_tm (ml_state_t *msp, const struct tm *tm)
{

    ML_AllocWrite(msp, 0, MAKE_DESC(DTAG_record, 9));
    ML_AllocWrite(msp, 1, INT_CtoML(tm->tm_sec));
    ML_AllocWrite(msp, 2, INT_CtoML(tm->tm_min));
    ML_AllocWrite(msp, 3, INT_CtoML(tm->tm_hour));
    ML_AllocWrite(msp, 4, INT_CtoML(tm->tm_mday));
    ML_AllocWrite(msp, 5, INT_CtoML(tm->tm_mon));
    ML_AllocWrite(msp, 6, INT_CtoML(tm->tm_year + 1900));
    ML_AllocWrite(msp, 7, INT_CtoML(tm->tm_wday));
    ML_AllocWrite(msp, 8, INT_CtoML(tm->tm_yday));
    ML_AllocWrite(msp, 9, INT_CtoML(tm->tm_isdst));

    return ML_Alloc(msp, 9);

}
