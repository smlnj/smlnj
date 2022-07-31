/* cfun-list.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Date"
#define CLIB_VERSION	"1.3"
#define CLIB_DATE	"June 9, 2019"
#endif

CFUNC("localOffset",		_ml_Date_localOffset,		"")
CFUNC("localOffsetForTime",	_ml_Date_localOffsetForTime,	"")
CFUNC("localTime",		_ml_Date_localtime,		"")
CFUNC("gmTime",			_ml_Date_gmtime,		"")
CFUNC("mkTime",			_ml_Date_mktime,		"")
CFUNC("strfTime",		_ml_Date_strftime,		"")
