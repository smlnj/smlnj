/* win32-reg.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * interface to win32 registry functions; note that HKEY is just an alias
 * for HANDLE
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

ml_val_t _ml_win32_REG_open_key_ex(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *subKey = STR_MLtoC(REC_SEL(arg,1));
    Word_t flags = REC_SELWORD(arg,2);
    HKEY target = NULL;

    LONG result = RegOpenKeyEx(key, subKey, 0, flags, &target);

    if (result == ERROR_SUCCESS) {
	ml_val_t res;
	return HANDLE_CtoML(msp, target);
    }

    return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_REG_create_key_ex(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *subKey = STR_MLtoC(REC_SEL(arg,1));
    Word_t flags = REC_SELWORD(arg,2);
    HKEY target = NULL;
    DWORD dwDisposition = 0;
    ml_val_t res;

    LONG result = RegCreateKeyEx(key, subKey, 0, NULL, 0, flags, NULL, &target, &dwDisposition);

    if (result == ERROR_SUCCESS) {
	RegCloseKey(target);
	target = NULL;

	/* Safe, as can only ever be 1 or 2 */
	return WORD32_CtoML(msp, dwDisposition);
    }

    return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_REG_close_key_ex(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(arg);
    LONG result = RegCloseKey(key);

    if (result == ERROR_SUCCESS) {
	return ML_unit;
    }

    return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_REG_delete_key(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *subKey = STR_MLtoC(REC_SEL(arg,1));
    LONG result = RegDeleteKey(key, subKey);

    if (result == ERROR_SUCCESS) {
	return ML_unit;
    }

    return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_REG_delete_value(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *subKey = STR_MLtoC(REC_SEL(arg,1));
    LONG result = RegDeleteValue(key, subKey);

    if (result == ERROR_SUCCESS) {
	return ML_unit;
    }

    return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_REG_enum_key_ex(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Word_t index = INT_MLtoC(REC_SEL(arg,1));
    ml_val_t vec;
    HKEY target = NULL;
    LONG result = 0;
    LONG characters = 256;   /* 255 is the max key name size */

    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (characters));
    result = RegEnumKeyEx(key, index, PTR_MLtoC(void, vec), &characters, 0, NULL, NULL, NULL);

    /* return string option */
    if (result == ERROR_SUCCESS) {
	ml_val_t obj, res;

	/* allocate string header */
	SEQHDR_ALLOC (msp, obj, DESC_string, vec, characters);
	/* put together the option string */
	OPTION_SOME(msp, res, obj);
	return res;
    }

    /* return NONE */
    if (result == ERROR_NO_MORE_ITEMS) {
	return OPTION_NONE;
    }

    return RAISE_SYSERR(msp,-1);
}

ml_val_t _ml_win32_REG_enum_value_ex(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Word_t index = INT_MLtoC(REC_SEL(arg,1));
    ml_val_t vec;
    HKEY target = NULL;
    LONG result = 0;
    DWORD nameLen = 0;

    result = RegQueryInfoKey(key, NULL, NULL, 0, NULL, NULL, NULL, NULL, &nameLen, NULL, NULL, NULL);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    nameLen += 1;
    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (nameLen));
    result = RegEnumValue(key, index, PTR_MLtoC(void, vec), &nameLen, 0, NULL, NULL, NULL);

    /* return string option */
    if (result == ERROR_SUCCESS) {
	ml_val_t obj, res;

	/* allocate string header */
	SEQHDR_ALLOC (msp, obj, DESC_string, vec, nameLen);
	/* put together the option string */
	OPTION_SOME(msp, res, obj);
	return res;
    }

    /* return NONE */
    if (result == ERROR_NO_MORE_ITEMS) {
	return OPTION_NONE;
    }

    return RAISE_SYSERR(msp,-1);
}

/*
 * This is a helper method to get the underlying type stored in the
 * registry value, so that the calling ML code can type select to the
 * appropriately-typed getter. Note that we have to go to these
 * lengths because it's not currently straightforward to store
 * values into datatypes.
 */
ml_val_t _ml_win32_REG_query_value_type(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    LONG result = 0;
    DWORD dwType = 0;
    ml_val_t res;

    result = RegQueryValueEx(key, valueName, 0, &dwType, NULL, NULL);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return WORD32_CtoML(msp, dwType);
}

ml_val_t _ml_win32_REG_QueryString(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    LONG result = 0;
    DWORD dwSize = 0;
    ml_val_t res, vec;

    result = RegQueryValueEx(key, valueName, 0, NULL, NULL, &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (dwSize));
    result = RegQueryValueEx(key, valueName, 0, NULL, PTR_MLtoC(void, vec), &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    /* allocate string header */
    /* note that we subtract one, as it comes back with a trailing null included in the count */
    SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize-1);
    return res;
}

ml_val_t _ml_win32_REG_query_value_string(ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_REG_QueryString(msp, arg);
}

ml_val_t _ml_win32_REG_query_value_multi_string(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    LONG result = 0;
    DWORD dwSize = 0;
    ml_val_t res, vec, str, tail;
    char *concatenated = NULL;
    char *ptr = NULL;
    int nextToCopy = 0;

    result = RegQueryValueEx(key, valueName, 0, NULL, NULL, &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    concatenated = (char *)MALLOC(dwSize);
    result = RegQueryValueEx(key, valueName, 0, NULL, concatenated , &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }


    res = LIST_nil;
    ptr = concatenated;
    while (dwSize > 0) {
	nextToCopy = strlen(ptr);
	vec = ML_AllocRaw (msp, BYTES_TO_WORDS (nextToCopy+1));
	strcpy_s((PTR_MLtoC(char, vec)), nextToCopy+1, ptr);
	SEQHDR_ALLOC (msp, str, DESC_string, vec, nextToCopy);
	ptr += strlen(ptr)+1;

	tail = res;
	LIST_cons(msp, res, str, tail);
	dwSize -= (nextToCopy + 1);
    }

    FREE(concatenated);
    return res;
}

ml_val_t _ml_win32_REG_query_value_expand_string(ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_REG_QueryString(msp, arg);
}

ml_val_t _ml_win32_REG_query_value_dword(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    LONG result = 0;
    DWORD dwValue = 0;
    DWORD dwSize = sizeof(DWORD);
    ml_val_t res, vec;

    result = RegQueryValueEx(key, valueName, 0, NULL, (LPBYTE)&dwValue, &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return WORD32_CtoML(msp, dwValue);
}

ml_val_t _ml_win32_REG_query_value_binary(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    void *pData = NULL;
    LONG result = 0;
    DWORD dwSize = 0;
    ml_val_t res, vec, zero;

    result = RegQueryValueEx(key, valueName, 0, NULL, NULL, &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    vec = ML_AllocBytearray (msp, dwSize);
    pData = GET_SEQ_DATAPTR(void, vec);
    result = RegQueryValueEx(key, valueName, 0, NULL, pData, &dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return vec;
}

ml_val_t _ml_win32_REG_set_value_dword(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    DWORD dwValue = REC_SELWORD(arg,2);
    LONG result = 0;
    DWORD dwSize = sizeof(DWORD);

    result = RegSetValueEx(key, valueName, 0, REG_DWORD, (const BYTE *)&dwValue, dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return ML_unit;
}

ml_val_t _ml_win32_REG_SetStringValue(ml_state_t *msp, ml_val_t arg, DWORD dwStringType)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    Byte_t *value = STR_MLtoC(REC_SEL(arg,2));
    LONG result = 0;
    DWORD dwSize = strlen(value)+1;

    result = RegSetValueEx(key, valueName, 0, dwStringType, (const BYTE *)value, dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return ML_unit;
}

ml_val_t _ml_win32_REG_set_value_string(ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_REG_SetStringValue(msp, arg, REG_SZ);
}

ml_val_t _ml_win32_REG_set_value_expand_string(ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_REG_SetStringValue(msp, arg, REG_EXPAND_SZ);
}

ml_val_t _ml_win32_REG_set_value_multi_string(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    ml_val_t stringList = REC_SEL(arg,2);
    LONG result = 0;
    DWORD dwSize = 0;
    ml_val_t iter = stringList;
    char *concatenated = NULL;
    char *ptr = NULL;

    while (iter != LIST_nil) {
      /* need to add one to each for room for extra NULLs */
	dwSize += strlen(STR_MLtoC(LIST_hd(iter))) + 1;
	iter = LIST_tl(iter);
    }

    /* extra second NULL terimator at end */
    concatenated = (char *)MALLOC((dwSize+1) * sizeof(char));
    ptr = concatenated;
    iter = stringList;
    while (iter != LIST_nil) {
	strcpy_s(ptr, dwSize+1, STR_MLtoC(LIST_hd(iter)));
	ptr += strlen(ptr)+1;
	iter = LIST_tl(iter);
    }
    (*ptr) = '\0';

    result = RegSetValueEx(key, valueName, 0, REG_MULTI_SZ, (const BYTE *)concatenated, dwSize);
    FREE(concatenated);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return ML_unit;
}

ml_val_t _ml_win32_REG_set_value_binary(ml_state_t *msp, ml_val_t arg)
{
    HKEY key = (HKEY)HANDLE_MLtoC(REC_SEL(arg,0));
    Byte_t *valueName = STR_MLtoC(REC_SEL(arg,1));
    Byte_t *dwValue = GET_SEQ_DATAPTR(Byte_t, REC_SEL(arg,2));
    DWORD dwSize = GET_SEQ_LEN(REC_SEL(arg,2));

    LONG result = 0;

    result = RegSetValueEx(key, valueName, 0, REG_BINARY, (const BYTE *)dwValue, dwSize);
    if (result != ERROR_SUCCESS) {
	return RAISE_SYSERR(msp,-1);
    }

    return ML_unit;
}
