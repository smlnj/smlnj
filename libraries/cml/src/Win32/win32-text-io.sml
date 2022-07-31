(* win32-text-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The implementation of the TextIO stack on Win32 systems.
 *)

structure TextIO = TextIOFn (structure OSPrimIO = Win32TextPrimIO);
