/*! \file win32-config.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * interface to win32 system configuration information
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

ml_val_t _ml_win32_CONFIG_get_version_ex (ml_state_t *msp, ml_val_t arg)
{
    OSVERSIONINFOEX versionInfo;
    long result = 0;
    int length = 0;
    ml_val_t res, major, minor, build, platform, csd, vec;

    ZeroMemory(&versionInfo, sizeof(OSVERSIONINFOEX));
    versionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

    result = GetVersionEx((OSVERSIONINFO *)&versionInfo);
    if (result == 0) {
	return RAISE_SYSERR(msp,-1);
    }

    major = WORD32_CtoML(msp, versionInfo.dwMajorVersion);
    minor = WORD32_CtoML(msp, versionInfo.dwMinorVersion);
    build = WORD32_CtoML(msp, versionInfo.dwBuildNumber);
    platform = WORD32_CtoML(msp, versionInfo.dwPlatformId);

    length = strlen(versionInfo.szCSDVersion);
    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (length + 1));
    strcpy_s(PTR_MLtoC(void, vec), length+1, versionInfo.szCSDVersion);
    SEQHDR_ALLOC (msp, csd, DESC_string, vec, length);

    REC_ALLOC5(msp, res, major, minor, build, platform, csd);

    return res;
}
/*
val getVolumeInformation : string -> {
                               volumeName : string,
                               systemName : string,
                               serialNumber : SysWord.word,
                               maximumComponentLength : int
                             }
*/
ml_val_t _ml_win32_CONFIG_get_volume_information (ml_state_t *msp, ml_val_t arg)
{
    TCHAR szVolumeName[MAX_PATH+1];
    DWORD serialNumber;
    DWORD maxComponentLength;
    DWORD fileSystemFlags;
    TCHAR szFilesystemName[MAX_PATH+1];
    Byte_t *subKey = STR_MLtoC(arg);
    int length;
    ml_val_t res, volume, system, serial, maxcomponent, vec1, vec2;

    if (!GetVolumeInformation(
	subKey, szVolumeName, MAX_PATH+1, &serialNumber, &maxComponentLength,
	&fileSystemFlags, szFilesystemName, MAX_PATH+1))
    {
	return RAISE_SYSERR(msp, -1);
    }

    SYSWORD_ALLOC(msp, serial, serialNumber);
    maxcomponent = INT_CtoML(maxComponentLength);

    length = strlen(szVolumeName);
    vec1 = ML_AllocRaw (msp, BYTES_TO_WORDS (length + 1));
    strcpy_s(PTR_MLtoC(void, vec1), length+1, szVolumeName);
    SEQHDR_ALLOC (msp, volume, DESC_string, vec1, length);

    length = strlen(szFilesystemName);
    vec2 = ML_AllocRaw (msp, BYTES_TO_WORDS (length + 1));
    strcpy_s(PTR_MLtoC(void, vec2), length+1, szFilesystemName);
    SEQHDR_ALLOC (msp, system, DESC_string, vec2, length);

    REC_ALLOC4(msp, res, volume, system, serial, maxcomponent);
    return res;
}


ml_val_t _ml_win32_CONFIG_get_windows_directory (ml_state_t *msp, ml_val_t arg)
{
    TCHAR directory[MAX_PATH+1];
    DWORD dwSize = MAX_PATH+1;
    ml_val_t res, vec;

    if ((dwSize = GetWindowsDirectory(directory, dwSize)) == 0) {
	return RAISE_SYSERR(msp,-1);
    }

    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (dwSize+1));
    strcpy_s(PTR_MLtoC(void, vec), dwSize+1, directory);
    SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize);

    return res;
}

ml_val_t _ml_win32_CONFIG_get_system_directory (ml_state_t *msp, ml_val_t arg)
{
    TCHAR directory[MAX_PATH+1];
    DWORD dwSize = MAX_PATH+1;
    ml_val_t res, vec;

    if ((dwSize = GetSystemDirectory(directory, dwSize)) == 0) {
	return RAISE_SYSERR(msp,-1);
    }

    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (dwSize+1));
    strcpy_s(PTR_MLtoC(void, vec), dwSize+1, directory);
    SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize);

    return res;
}

ml_val_t _ml_win32_CONFIG_get_computer_name(ml_state_t *msp, ml_val_t arg)
{
    TCHAR name[MAX_PATH+1];
    DWORD dwSize = MAX_PATH+1;
    ml_val_t res, vec;

    if (!GetComputerName(name, &dwSize)) {
	return RAISE_SYSERR(msp,-1);
    }

    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (dwSize+1));
    strcpy_s(PTR_MLtoC(void, vec), dwSize+1, name);
    SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize);

    return res;
}

ml_val_t _ml_win32_CONFIG_get_user_name(ml_state_t *msp, ml_val_t arg)
{
    TCHAR name[MAX_PATH+1];
    DWORD dwSize = MAX_PATH+1;
    ml_val_t res, vec;

    if (!GetUserName(name, &dwSize)) {
	return RAISE_SYSERR(msp,-1);
    }

    vec = ML_AllocRaw (msp, BYTES_TO_WORDS (dwSize));
    strcpy_s(PTR_MLtoC(void, vec), dwSize, name);
    SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize-1);

    return res;
}
