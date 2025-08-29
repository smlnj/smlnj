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

    (* section IDs are represented as words internally and as four-character
     * little-endian codes externally.
     *)
    structure SectId =
      struct

        type t = Word.word

        (* ID from its word representation; we mask out the high bits *)
        fun fromWord (w) : t = Word.andb(w, 0wxffffffff)

        fun fromString s = (case explode s
               of [c1, c2, c3, c4] =>
                    W.<<(W.fromInt(ord c4), 0w24) +
                    W.<<(W.fromInt(ord c3), 0w16) +
                    W.<<(W.fromInt(ord c2), 0w8) +
                    W.fromInt(ord c1)
                | _ => raise Size
              (* end case *))

        fun toString (id : t) = let
              fun toChr w = chr(Word.toInt(Word.andb(w, 0wxff)))
              in
                CharVector.fromList [
                    toChr id,
                    toChr(Word.>>(id, 0w8),
                    toChr(Word.>>(id, 0w16),
                    toChr(Word.>>(id, 0w24)
                  ]
              end

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

        type smlnj_version = {version_id : int list, suffix : string}

        type sect_desc = {
            kind : SectId.t,
            flags : word,
            offset : Position.int,
            size : int
          }

        val version = 0wx20250801
        (* size of header exclusive of the section table *)
        val fixedSize = 32
        (* size of an entry in the section table *)
        val sectDescSize = 16
        (* maximum number of sections in a binfile *)
        val maxNSects = 32 * 1024

        type t = {
            kind : kind,
            version : word,
            smlnjVersion : smlnj_version,
            sects : sect_desc vector
          }

        (* is the binfile an archive? *)
        fun isArchive ({kind = StableArchive, ...} : t) = true
          | isArchive _ = false

        fun sizeOfHdr (hdr : t) = fixedSize + sectDescSize * Vector.length(#sects hdr)

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
        datatype t = IN of {
            hdr : Hdr.t,
            file : string option,
            inS : BinIO.instream,
            base : Position.int
          }

        datatype sect = SECT of {
            bf : t,
            desc : Hdr.sect_desc
          }

        fun getChar (bv, i) = chr (Word8.toInt (W8V.sub(bv, i)))

        fun getString (bv, base, n) = CharVector.tabulate(n, fn i => getChar (bv, base+i))

        (* get a little-endian 32-bit signed integer from the byte vector `bv`
         * at offset `i`.  We assume that the offset is 4-byte aligned.
         *)
        fun getInt32 (bv, i) = LargeWord.toIntX(PackWord32Little.subVecX(bv, i div 4))

        (* get a little-endian 32-bit unsigned integer from the byte vector `bv`
         * at offset `i`.  We assume that the offset is 4-byte aligned.
         *)
        fun getUInt32 (bv, i) = Word.fromLarge(PackWord32Little.subVec(bv, i div 4))

        (* read and decode the header from a binary input stream *)
        fun header inS = let
              val hdrData = BinIO.inputN(inS, Hdr.fixedSize)
              val () = if (W8V.length hdrData <> Hdr.fixedSize)
                    then (* error *)
                    else ()
              (* decode the header *)
              val kind = (case getString(hdrData, 0, 8)
                     of "BinFile " => Hdr.BinFile
                      | "StabArch" => Hdr.StableArchive
                      | s => (* error *)
                    (* end case *))
              val vers = getUInt32(hdrData, 8)
              val () = if vers <> Hdr.version
                    then (* error *)
                    else ()
              val smlnjVers = (case SMLNJVersion.fromString (getString(hdrData, 12, 16))
                     of SOME v => v
                      | NONE => (* error *)
                    (* end case *))
(* TODO: decode SML/NJ version *)
              val nSects = getInt32(hdrData, 28)
              val () = if (nSects < 0) orelse (Hdr.maxNSects < nSects)
                    then (* error *)
                    else ()
              (* read a section descriptor *)
              fun getSectDesc () = let
                    val descData = BinIO.inputN(inS, Hdr.sectDescSize)
                    val () = if (W8V.length descData <> Hdr.sectDescSize)
                          then (* error *)
                          else ()
                    val kind = SectId.fromWord(getUInt32(descData, 0))
                    val flags = getUInt32(descData, 4)
                    val offset = Position.fromLarge(
                          Word.toLargeInt(getUInt32(descData, 8)))
                    val sz = Word.toIntX(getUInt32(descData, 12))
                    in {
                      kind = kind,
                      flags = flags,
                      offset = offset,
                      size = sz
                    } end
              val sectTbl = Vector.tabulate (nSects, fn _ => getSectDesc())
(* TODO: validate the table *)
              in {
                kind = kind,
                version = vers,
                smlnjVersion = smlnjVers,
                sects = sectTbl
              } end

        fun create (name, inS, base) =
              IN{hdr = header inS, file = name, inS = inS, base = base}

        fun openFile file = let
              val inS = BinIO.openIn file
                    handle ex => (* error opening file *)
              in
                create (file, inS, 0)
              end

        fun openStream inS = create ("<stream>", inS, 0)

        (* is the binfile an archive? *)
        fun isArchive (IN{hdr={kind = StableArchive, ...}, ...}) = true
          | isArchive _ = false

        fun findSection (hdr : Hdr.t, id : SectId.t) =
              Vector.find (fn {kind, ...} => id = kind) (#sects hdr)

        (* `section (bf, id, inFn)` looks up the section with `id` in the binfile
         * `bf` and then uses `inFn` to read its contents.  Returns `NONE` when
         * the section is missing and `SOME contents` when the section is present
         * and `inFn` returns `contents`.
         *)
        fun section (IN{hdr, ...}, sectId, inFn) = (case findSection (hdr, sectId)
               of SOME{offset, size, ...} =>
(* TODO: seek to the `offset` file position; then read `size` bytes *)
                | NONE => (* error *)
              (* end case *))

        val bytes : sect * int -> Word8Vector.vector
        val string : sect * int -> string
        val int32 : sect -> Int32.int
        val word32 : sect -> Word32.int
        val pid : sect -> PersStamps.persstamp
        val codeobj : sect * int -> CodeObj.code_object

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
