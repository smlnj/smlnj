(* win32-text-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Labs.
 *
 * The implementation of the TextIO stack on Win32 systems.
 *
 *)

structure TextIO :> TEXT_IO
    where type StreamIO.reader = TextPrimIO.reader
    where type StreamIO.writer = TextPrimIO.writer
    where type StreamIO.pos = TextPrimIO.pos
  = TextIOFn (structure OSPrimIO = Win32TextPrimIO);

