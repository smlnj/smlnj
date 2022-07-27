(* asdl-memory-pickle.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Combined memory pickle/unpickle operations for basic types.
 *)

structure ASDLMemoryPickle : sig

    include ASDL_PICKLE

  (* byte-level I/O to support primitive picklers *)
    val input1 : instream -> Word8.word
    val output1 : outstream * Word8.word -> unit

  (* open a vector as an input stream *)
    val openVector : Word8Vector.vector -> instream

  (* has all the input been consumed? *)
    val endOfStream : instream -> bool

  (* pickle to/from a vector *)
    val toVector : (outstream * 'a -> unit) -> 'a -> Word8Vector.vector
    val fromVector : (instream -> 'a)
	  -> Word8Vector.vector
	  -> 'a

  end where type outstream = Word8Buffer.buf
  = struct

    open ASDLMemoryReadPickle
    open ASDLMemoryWritePickle

  end
