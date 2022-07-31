/*! \file win32-dde.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * interface to win32 dynamic data exchange.  Note that HCONV is just an alias
 * for HANDLE.
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

HDDEDATA CALLBACK DdeCallback (UINT uType, UINT uFmt, HCONV hconv, HSZ hsz1,
                               HSZ hsz2, HDDEDATA hdata, ULONG_PTR dwData1,
                               ULONG_PTR dwData2)
{
  /* As a DDE client, we will only get notification messages that
     ignore the return value anyway */
    return (HDDEDATA)0;
}

static DWORD InstanceId = 0;

void InitializeIfNeeded ()
{
    if (InstanceId == 0) {
        DdeInitialize(&InstanceId, DdeCallback, APPCMD_CLIENTONLY, 0);
    }
}

ml_val_t _ml_win32_DDE_start_dialog (ml_state_t *msp, ml_val_t arg)
{
    Byte_t* service = STR_MLtoC(REC_SEL(arg,0));
    Byte_t* topic = STR_MLtoC(REC_SEL(arg,1));
    HCONV conversation = NULL;
    HSZ hszService, hszTopic;
    ml_val_t res;

    InitializeIfNeeded();

    hszService = DdeCreateStringHandle(InstanceId, service, CP_WINANSI);
    hszTopic = DdeCreateStringHandle(InstanceId, topic, CP_WINANSI);

    conversation = DdeConnect(InstanceId, hszService, hszTopic, NULL);

    DdeFreeStringHandle(InstanceId, hszService);
    DdeFreeStringHandle(InstanceId, hszTopic);

    return HANDLE_CtoML(msp, conversation);
}

ml_val_t _ml_win32_DDE_execute_string (ml_state_t *msp, ml_val_t arg)
{
    HCONV conversation = (HCONV)HANDLE_MLtoC(REC_SEL(arg, 0));
    Byte_t* command = STR_MLtoC(REC_SEL(arg,1));
    Word_t retry = INT_MLtoC(REC_SEL(arg,2));
    Word_t delay = INT_MLtoC(REC_SEL(arg,3));
    DWORD dwResult = 0;
    HDDEDATA retval = 0;

    do {
	retval = DdeClientTransaction(
		command, strlen(command)+1, conversation, 0, 0,
		XTYP_EXECUTE, delay, &dwResult);
	retry--;
    } while (retval == 0 && DdeGetLastError(InstanceId) == DMLERR_BUSY && retry >= 0);

    if (!retval) {
	return RAISE_SYSERR(msp,-1);
    }

    return ML_unit;
}

ml_val_t _ml_win32_DDE_stop_dialog (ml_state_t *msp, ml_val_t arg)
{
    HCONV conversation = (HCONV)HANDLE_MLtoC(REC_SEL(arg, 0));

    DdeDisconnect(conversation);

    return ML_unit;
}
