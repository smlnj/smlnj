(* win32-bin-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Labs.
 *
 * The implementation of the BinIO stack on Win32 systems.
 *
 *)

structure BinIO :> BIN_IO
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
(**    where type StreamIO.pos = BinPrimIO.pos  **)
  = BinIOFn (structure OSPrimIO = Win32BinPrimIO);


