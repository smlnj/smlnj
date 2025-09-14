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

        (* the fixed-size part of the header *)
        type t

        (* the SML/NJ version (see the `SMLNJVersion` structure) *)
        type smlnj_version = {version_id : int list, suffix : string}

        val isArchive : t -> bool
        val smlnjVersion : t -> smlnj_version

      end

    (***** Section input *****)
    structure In : sig
        type t
        type sect

        val openFile : string -> t
        val openStream : BinIO.instream -> t

        (* the header info for the binfile *)
        val header : t -> Hdr.t

        (* `section (bf, id, inFn)` looks up the section with `id` in the binfile
         * `bf` and then uses `inFn` to read its contents.  Returns `NONE` when
         * the section is missing and `SOME contents` when the section is present
         * and `inFn` returns `contents`.
         *)
        val section : t * SectId.t * (sect * int -> 'a) -> 'a option

        (* read the specified number of bytes from the section *)
        val bytes : sect * int -> Word8Vector.vector
        (* read a string of the specified length from the section *)
        val string : sect * int -> string
        (* read a packed integer from the section using the LEB128 encoding *)
        val packedInt : sect -> int
        (* read a 32-bit signed integer from the section *)
        val int32 : sect -> int
        (* read a 32-bit unsigned integer from the section *)
        val word32 : sect -> word
        (* read a PID from the section *)
        val pid : sect -> PersStamps.persstamp
        (* read a code object from the section *)
        val codeObject : sect * int -> CodeObj.t

      end

    (***** Section output *****)
    structure Out : sig
        type t
        type sect

        (* `openFile (file, isArchive, version)` *)
        val openFile : string * bool * Hdr.smlnj_version -> t
        (* `openStream (outS, isArchive, version)` *)
        val openStream : BinIO.outstream * bool * Hdr.smlnj_version -> t

        val finish : t -> unit

        (* `section (bf, id, sz, outFn)` adds a section with the given `id`
         * and size `sz` in bytes.  The `outFn` is used to output the contents
         * of the section using the functions below.
         *)
        val section : t * SectId.t * int * (sect -> unit) -> unit

        (* write a vector of bytes to the section *)
        val bytes : sect * Word8Vector.vector -> unit
        val string : sect * string -> unit
        (* write a packed integer to the section using the LEB128 encoding *)
        val packedInt : sect * int -> unit
        val int32 : sect * int -> unit
        val word32 : sect * word -> unit
        val pid : sect * PersStamps.persstamp -> unit
        val codeObject : sect * CodeObj.t -> unit

      end

    (* given the number of bytes in a section, return it padded size.  Currently
     * sections are padded to a multiple of 8 bytes.
     *)
    val padSize : int -> int

    (* error messages for Binfile I/O *)
    val error : string -> 'a

  end
