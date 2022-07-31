(* text-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the TextIO stack on Posix systems.
 *)

structure TextIO = TextIOFn (structure OSPrimIO = PosixTextPrimIO);
