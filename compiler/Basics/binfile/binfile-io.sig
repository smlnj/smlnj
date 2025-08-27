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

        type smlnj_version = {id : int list, suffix : string}

        type sect_desc = {
            kind : SectId.t,            (* section ID *)
            flags : word,               (* per-section flag bits; reserved for future *)
            offset : Position.int,      (* offset from beginning of containing binfile *)
            size : int                  (* size in bytes of section *)
          }

        type t = {
            kind : kind,
            version : word,
            smlnjVersion : smlnj_version,
            sects : sect_desc vector
          }

        (* is the binfile an archive? *)
        val isArchive : hdr -> bool

        (* the current version *)
        val version : word

        (* `sizeOfHdr nSects` returns the size of a header that has `nSects`
         * sections.
         *)
        val sizeOfHdr : int -> int

        (* return the size (in bytes) of the binfile described by the header *)
        val sizeOfBinfile : t -> int

      end

    (***** Section input *****)
    structure In : sig
        type t
        type sect

        val openFile : string -> t
        val openStream : BinIO.instream -> t

        val header : t -> Hdr.t

        (* `section (bf, id, inFn)` looks up the section with `id` in the binfile
         * `bf` and then uses `inFn` to read its contents.  Returns `NONE` when
         * the section is missing and `SOME contents` when the section is present
         * and `inFn` returns `contents`.
         *)
        val section : t * SectId.t * (sect * int -> 'a) -> 'a option

        val bytes : sect * int -> Word8Vector.vector
        val string : sect * int -> string
        val int32 : sect -> Int32.int
        val word32 : sect -> Word32.int
        val pid : sect -> PersStamps.persstamp
        val codeobj : sect * int -> CodeObj.code_object

      end

    (***** Section output *****)
    structure Out : sig
        type t
        type sect

        val openFile : string * Hdr.kind * smlnj_version -> t
        val openStream : BinIO.outstream * Hdr.kind * smlnj_version -> t

        val finish : t -> unit

        (* `section (bf, id, sz, outFn)` adds a section with the given `id`
         * and size `sz` in bytes.  The `outFn` is used to output the contents
         * of the section using the functions below.
         *)
        val section : t * SectId.t * int * (sect -> unit) -> unit

        val bytes : sect * Word8Vector.vector -> unit
        val bytes' : sect * Word8VectorSlice.slice -> unit
        val int32 : sect * Int32.int -> unit
        val word32 : sect * Word32.int -> unit
        val string : sect * string -> unit
        val pid : sect * PersStamps.persstamp -> unit
        val codeobj : sect * CodeObj.code_object -> unit

      end

    (* error messages for Binfile I/O *)
    val error : string -> 'a

  end
