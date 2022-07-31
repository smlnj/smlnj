(* bin-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the BinIO stack on Posix systems.
 *
 *)

structure BinIO = BinIOFn (structure OSPrimIO = PosixBinPrimIO);
