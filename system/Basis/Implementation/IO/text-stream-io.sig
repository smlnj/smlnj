(* text-stream-io.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TEXT_STREAM_IO =
  sig
    include STREAM_IO
    val inputLine    : instream -> (string * instream) option
    val outputSubstr : (outstream * substring) -> unit
  end
