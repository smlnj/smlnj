(* asdl-read-pickle.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common interface for basic unpickling operations.
 *)

signature ASDL_READ_PICKLE =
  sig

    type instream

    val readBool : instream -> ASDL.bool

    val readBoolOption  : instream -> ASDL.bool option

    val readInt : instream -> ASDL.int

    val readUInt : instream -> ASDL.uint

    val readInteger : instream -> ASDL.integer

    val readString : instream -> ASDL.string

    val readIdentifier : instream -> ASDL.identifier

  (* utility functions for sum-type tags *)
    val readTag8 : instream -> word

    val readTag16 : instream -> word

  end
