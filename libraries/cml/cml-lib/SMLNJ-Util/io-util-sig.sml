(* io-util-sig.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)

signature IO_UTIL =
  sig
    type instream
    type outstream

    val withInputFile : string * ('a -> 'b) -> 'a -> 'b
    val withInstream : instream * ('a -> 'b) -> 'a -> 'b
    val withOutputFile : string * ('a -> 'b) -> 'a -> 'b
    val withOutstream : outstream * ('a -> 'b) -> 'a -> 'b
  end
