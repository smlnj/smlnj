(* text-io.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature TEXT_IO =
  sig

  (* include IMPERATIVE_IO *)
    type vector = string
    type elem = char

    type instream
    type outstream

    val input    : instream -> vector
    val input1   : instream -> elem option
    val inputN   : (instream * int) -> vector
    val inputAll : instream -> vector
    val canInput : (instream * int) -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool

    val output   : (outstream * vector) -> unit
    val output1  : (outstream * elem) -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit

    structure StreamIO : TEXT_STREAM_IO
      where type vector = string
      and type elem = char

    val mkInstream  : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : (instream * StreamIO.instream) -> unit

    val getPosOut    : outstream -> StreamIO.out_pos
    val setPosOut    : (outstream * StreamIO.out_pos) -> unit
    val mkOutstream  : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : (outstream * StreamIO.outstream) -> unit

    val inputLine    : instream -> string option
    val outputSubstr : (outstream * substring) -> unit

    val openIn     : string -> instream
    val openString : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream

    val stdIn  : instream
    val stdOut : outstream
    val stdErr : outstream

    val print : string -> unit

    val scanStream :
	  ((elem, StreamIO.instream) StringCvt.reader
	    -> ('a, StreamIO.instream) StringCvt.reader
	  ) -> instream -> 'a option

  end;
