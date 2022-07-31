(* bin-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the BinIO stack on Posix systems.
 *
 *)

structure BinIO :> BIN_IO
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
(*    where type StreamIO.pos = BinPrimIO.pos  - redundant *)
  = BinIOFn (structure OSPrimIO = PosixBinPrimIO);

