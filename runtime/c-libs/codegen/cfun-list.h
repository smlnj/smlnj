/*! \file cfun-list.h
 *
 * \author John Reppy
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

/*
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"CodeGen"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"June 14, 2024"
#endif

CFUNC("generate",		_ml_CodeGen_generate,		"")
CFUNC("listTargets",		_ml_CodeGen_listTargets,	"")
CFUNC("setTarget",		_ml_CodeGen_setTarget,		"")
