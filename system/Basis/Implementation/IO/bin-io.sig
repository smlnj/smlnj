(* bin-io.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature BIN_IO =
  sig
    include IMPERATIVE_IO

    val openIn     : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream
  end
  where type vector = Word8Vector.vector
  and type StreamIO.vector = Word8Vector.vector
  and type StreamIO.elem = Word8.word
  and type StreamIO.pos = Position.int;
