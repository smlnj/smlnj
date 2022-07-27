(* asdl-file-pickle.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Combined file pickle/unpickle operations for basic types.
 *)

structure ASDLFilePickle : sig

    include ASDL_PICKLE

  (* byte-level I/O to support primitive picklers *)
    val input1 : instream -> Word8.word
    val output1 : outstream * Word8.word -> unit

  (* pickle to/from files *)
    val toFile : (outstream * 'a -> unit) -> (string * 'a) -> unit
    val fromFile : (instream -> 'a) -> string -> 'a

  end where type instream = BinIO.instream
      where type outstream = BinIO.outstream
  = struct

    open ASDLFileReadPickle
    open ASDLFileWritePickle

  end
