(* asdl-write-pickle.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common interface for basic pickling operations.
 *)

signature ASDL_WRITE_PICKLE =
  sig

    type outstream

    val writeBool : outstream * ASDL.bool -> unit

    val writeBoolOption : outstream * ASDL.bool option -> unit

    val writeInt : outstream * ASDL.int -> unit

    val writeUInt : outstream * ASDL.uint -> unit

    val writeInteger : outstream * ASDL.integer -> unit

    val writeString : outstream * ASDL.string -> unit

    val writeIdentifier : outstream * ASDL.identifier -> unit

  (* utility functions for sum-type tags *)
    val writeTag8 : outstream * word -> unit

    val writeTag16 : outstream * word -> unit

  end
