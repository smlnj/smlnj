(* binfile-io.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Basic I/O support for containerized binfiles (see
 * https://github.com/smlnj/.github/wiki/New-Binfile-Format)
 * This file must be kept in sync with runtime/kernel/boot.c.
 *)

structure BinfileIO :> BINFILE_IO =
  struct

    structure W = Word

    fun error msg = (
	  Control_Print.say (concat ["binfile format error: ", msg, "\n"]);
	  raise FormatError)

    (* section IDs are represented as words internally, and as four-character
     * little-endian codes externally.
     *)
    structure SectId =
      struct

        eqtype t= Word.word

        val fromString : string -> t

        fun fromString s = (case explode s
               of [c1, c2, c3, c4] =>
                    W.<<(W.fromInt(ord c4), 0w24) +
                    W.<<(W.fromInt(ord c3), 0w16) +
                    W.<<(W.fromInt(ord c2), 0w8) +
                    W.fromInt(ord c1)
                | _ => raise Size
              (* end case *))

        fun toString (id : t) = ??

        val binfile : section_id = sectionID "BINF"
        val info : section_id = sectionID "INFO"
        val import : section_id = sectionID "IMPT"
        val export : section_id = sectionID "EXPT"
        val pids : section_id = sectionID "PIDS"
        val guid : section_id = sectionID "GUID"
        val literals : section_id = sectionID "LITS"
        val code : section_id = sectionID "CODE"
        val cfkPickle : section_id = sectionID "CFGP"
        val staticEnv : section_id = sectionID "SENV"
        val libStamp : section_id = sectionID "STMP"
        val depGraphPickle : section_id = sectionID "PDGR"
        val padding : section_id = sectionID "PAD "

      end

    structure Hdr =
      struct

        datatype kind = BinFile | StableArchive

        type sect_desc = {
            kind : SectId.t,
            flags : word,
            offset : Position.int,
            size : int
          }

        val version = 0wx20250801

        type t = {
            kind : kind,
            version : word,
            smlnjVersion : {id : int list, suffix : string},
            sects : sect_desc vector
          }

        (* is the binfile an archive? *)
        fun isArchive ({kind = StableArchive, ...} : t) = true
          | isArchive _ = false

        fun sizeOfHdr (hdr : t) = 32 + 16 * Vector.length(#sects hdr)

        fun sizeOfFile (hdr : t) =
              Vector.foldl
                (fn ({size, ...}, acc) => acc + 8 * size)
                  (sizeOfHdr hdr)
                    (#sects hdr)

        fun findSection (hdr : t, id : SectId.t) =
              Vector.find (fn {kind, ...} => id = kind) (#sects hdr)

      end

    structure In =
      struct
      end

    structure Out =
      struct

    fun mkSMLNJVersion (hdr : Hdr.t) = let
          val vn = String.concatWithMap "." Int.toString (#id (#smlnjVersion hdr))
          val v = if (#suffix(#smlnjVersion hdr) = "")
                then vn
                else concat[vn, "-", suffix]
          in
            (* the SML/NJ version string in the old binfile format is 16 bytes *)
            if (size v > 16)
              then substring (v, 0, 16)
            else if (size v < 16)
              then StringCvt.padRight #" " 16 v
              else v
          end

    fun pickleInt32 i = let
	  val w = fromInt i
	  fun out w = toByte w
	  in
	    W8V.fromList [
		toByte (w >> 0w24), toByte (w >> 0w16),
		toByte (w >> 0w8), toByte w
	      ]
	  end
    fun writeInt32 s i = BinIO.output (s, pickleInt32 i)

    fun picklePackedInt32 i = let
	  val n = LargeWord.fromInt i
	  val // = LargeWord.div
	  val %% = LargeWord.mod
	  val !! = LargeWord.orb
	  infix // %% !!
	  val toW8 = Word8.fromLargeWord
	  fun r (0w0, l) = W8V.fromList l
	    | r (n, l) = r (n // 0w128, toW8 ((n %% 0w128) !! 0w128) :: l)
	  in
	    r (n // 0w128, [toW8 (n %% 0w128)])
	  end

    fun writePid (s, pid) = BinIO.output (s, Pid.toBytes pid)
    fun writePidList (s, l) = app (fn p => writePid (s, p)) l

      end

  end
