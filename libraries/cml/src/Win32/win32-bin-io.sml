(* win32-bin-io.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure BinIO = BinIOFn (structure OSPrimIO = Win32BinPrimIO);
