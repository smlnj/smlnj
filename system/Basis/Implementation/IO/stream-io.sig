(* stream-io.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature STREAM_IO =
  sig
    type vector
    type elem
    type reader
    type writer

    type instream
    type outstream

    type pos
    type out_pos

    val input       : instream -> (vector * instream)
    val input1      : instream -> (elem * instream) option
    val inputN      : (instream * int) -> (vector * instream)
    val inputAll    : instream -> (vector * instream)
    val canInput    : (instream * int) -> int option
    val closeIn     : instream -> unit
    val endOfStream : instream -> bool
    val mkInstream  : (reader * vector) -> instream
    val getReader   : instream -> (reader * vector)
    val filePosIn   : instream -> pos

    val output        : (outstream * vector) -> unit
    val output1       : (outstream * elem) -> unit
    val flushOut      : outstream -> unit
    val closeOut      : outstream -> unit
    val setBufferMode : (outstream * IO.buffer_mode) -> unit
    val getBufferMode : outstream -> IO.buffer_mode
    val mkOutstream   : (writer * IO.buffer_mode) -> outstream
    val getWriter     : outstream -> (writer * IO.buffer_mode)
    val getPosOut     : outstream -> out_pos
    val setPosOut     : out_pos -> unit
    val filePosOut    : out_pos -> pos

  end
