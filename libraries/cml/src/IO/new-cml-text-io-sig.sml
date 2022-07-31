(* cml-text-io-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This extends the SMLBL TEXT_IO interface with event-valued operations.
 *)

signature CML_TEXT_IO =
  sig
  (* include TEXT_IO *)
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

    structure StreamIO : CML_TEXT_STREAM_IO
      where type reader = TextPrimIO.reader
      where type writer = TextPrimIO.writer
      where type pos = TextPrimIO.pos
      where type vector = string
      where type elem = char

(*
    val getPosIn    : instream -> StreamIO.in_pos
    val setPosIn    : (instream * StreamIO.in_pos) -> unit
*)
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

    val input1Evt   : instream -> elem option CML.event
    val inputNEvt   : (instream * int) -> vector CML.event
    val inputEvt    : instream -> vector CML.event
    val inputAllEvt : instream -> vector CML.event

    val openChanIn  : string CML.chan -> instream
    val openChanOut : string CML.chan -> outstream

    val print : string -> unit

    val scanStream :
	  ((elem, StreamIO.instream) StringCvt.reader
	    -> ('a, StreamIO.instream) StringCvt.reader
	  ) -> instream -> 'a option

  end;
