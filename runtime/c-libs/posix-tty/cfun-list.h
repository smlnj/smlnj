/*! \file cfun-list.h
 *
 * This file lists the directory library of C functions that are callable by ML.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"POSIX-TTY"
#define CLIB_VERSION	"1.1"
#define CLIB_DATE	"July 25, 2022"
#endif

CFUNC("osval",	     _ml_P_TTY_osval,          "string -> int")
CFUNC("tcgetattr",   _ml_P_TTY_tcgetattr,      "int -> termio_rep")
CFUNC("tcsetattr",   _ml_P_TTY_tcsetattr,      "int * int * termio_rep -> unit")
CFUNC("tcsendbreak", _ml_P_TTY_tcsendbreak,    "int * int -> unit")
CFUNC("tcdrain",     _ml_P_TTY_tcdrain,        "int -> unit")
CFUNC("tcflush",     _ml_P_TTY_tcflush,        "int * int -> unit")
CFUNC("tcflow",      _ml_P_TTY_tcflow,         "int * int -> unit")
CFUNC("tcgetpgrp",   _ml_P_TTY_tcgetpgrp,      "int -> int")
CFUNC("tcsetpgrp",   _ml_P_TTY_tcsetpgrp,      "int * int -> unit")
/* Basis Library proposal 2021-001 */
CFUNC("getwinsz",    _ml_P_TTY_getwinsz,       "int -> (int * int) option")
