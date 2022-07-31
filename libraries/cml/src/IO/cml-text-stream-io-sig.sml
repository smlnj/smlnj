(* cml-text-stream-io-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This extends the SMLBL TEXT_STREAM_IO interface with event-valued operations.
 *)

signature CML_TEXT_STREAM_IO =
  sig
    include TEXT_STREAM_IO

    val input1Evt    : instream -> (elem * instream) option CML.event
    val inputNEvt    : (instream * int) -> (vector * instream) CML.event
    val inputEvt     : instream -> (vector * instream) CML.event
    val inputAllEvt  : instream -> (vector * instream) CML.event
    val inputLineEvt : instream -> (vector * instream) option CML.event

  end;
