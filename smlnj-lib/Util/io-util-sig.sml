(* io-util-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for redirecting stdIn/stdOut.
 *)

signature IO_UTIL =
  sig

  (* rebind stdIn *)
    val withInputFile : string * ('a -> 'b) -> 'a -> 'b
    val withInstream : TextIO.instream * ('a -> 'b) -> 'a -> 'b

  (* rebind stdOut *)
    val withOutputFile : string * ('a -> 'b) -> 'a -> 'b
    val withOutstream : TextIO.outstream * ('a -> 'b) -> 'a -> 'b

  end
