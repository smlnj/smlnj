/*! \file cfun-list.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Time"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"June 9, 2019"
#endif

CFUNC("gettime",	_ml_Time_gettime,		"")
CFUNC("timeofday",	_ml_Time_timeofday,		"")
