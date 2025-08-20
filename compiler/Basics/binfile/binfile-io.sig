(* binfile-io.sig
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

signature BINFILE_IO =
  sig

    structure SectId : sig

        eqtype t

        val fromString : string -> t
        val toString : t -> string

        (* standard section IDs *)
        val binfile : t
        val info : t
        val import : t
        val export : t
        val pids : t
        val guid : t
        val literals : t
        val code : t
        val cfkPickle : t
        val staticEnv : t
        val libStamp : t
        val depGraphPickle : t
        val padding : t

      end

    structure Hdr : sig

        datatype kind = BinFile | StableArchive

        type sect_desc = {
            kind : SectId.t,
            flags : word,
            offset : Position.int,
            size : int
          }

        type t = {
            kind : kind,
            version : word,
            smlnjVersion : {id : int list, suffix : string},
            sects : sect_desc vector
          }

        (* the current version *)
        val version : word

      end

    val size : sect -> int

    (** Section output *)
    val outInt32 : sect * Int32.int -> unit
    val outWord32 : sect * Word32.int -> unit
    val outString : sect * string -> unit
    val outBytes : sect * Word8Vector.vector -> unit
    val outBytes' : sect * Word8VectorSlice.slice -> unit
    val finish : sect -> unit

  end
