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
    structure W8V = Word8Vector
    structure W8B = Word8Buffer
    structure Pid = PersStamps

    fun error msg = (
	  Control_Print.say (concat ["binfile format error: ", msg, "\n"]);
	  raise FormatError)

    (* section IDs are represented as words internally and as four-character
     * little-endian codes externally.
     *)
    structure SectId =
      struct

        type t = W.word

        (* ID from its word representation; we mask out the high bits *)
        fun fromWord (w) : t = W.andb(w, 0wxffffffff)

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
                    toChr(W.>>(id, 0w8),
                    toChr(W.>>(id, 0w16),
                    toChr(W.>>(id, 0w24)
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

        type smlnj_version = {version_id : int list, suffix : string}

        val version = 0wx20250801
        (* size of header exclusive of the section table *)
        val fixedSize = 32
        (* size of an entry in the section table *)
        val sectDescSize = 16
        (* maximum number of sections in a binfile *)
        val maxNSects = 32 * 1024

        (* the fixed part of the section header *)
        type t = {
            isArchive : bool,
            version : word,
            smlnjVersion : smlnj_version
          }

        (* initialize a header *)
        fun mkHeader (isArchive, smlnjVers) =
              { isArchive = isArchive, version = version, smlnjVersion = smlnjVers }

        (* is the binfile an archive? *)
        fun isArchive ({isArchive, ...} : t) = isArchive

        fun smlnjVersion (hdr : t) = #smlnjVersion hdr

        fun sizeOfHdr nSects = fixedSize + sectDescSize * nSects

        type sect_desc = {
            kind : SectId.t,
            flags : word,
            offset : Position.int,
            size : int
          }

        type sect_tbl = sect_desc Vector.vector

        fun sizeOfFile (hdr : t, sects : sect_tbl) =
              Vector.foldl
                (fn ({size, ...}, acc) => acc + 8 * size)
                  (sizeOfHdr hdr)
                    sects

        fun findSection (sects : sect_tbl, id : SectId.t) =
              Vector.find (fn {kind, ...} => id = kind) sects hdr

      end

    structure In =
      struct
        datatype t = IN of {
            hdr : Hdr.t,
            file : string option,
            inS : BinIO.instream,       (* the original input stream for the binfile *)
            base : Position.int,
            sects : Hdr.sect_tbl
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
              val isArchive = (case getString(hdrData, 0, 8)
                     of "BinFile " => false
                      | "StabArch" => true
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
              (* get the number of sections *)
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
                create (SOME file, inS, 0)
              end

        fun openStream inS = create (NONE, inS, 0)

        fun header (IN{hdr, ...}) = hdr

        (* is the binfile an archive? *)
        fun isArchive (IN{hdr={kind = StableArchive, ...}, ...}) = true
          | isArchive _ = false

        fun findSection (hdr : Hdr.t, id : SectId.t) =
              Vector.find (fn {kind, ...} => id = kind) (#sects hdr)

        (* seek to the specified file position, which is relative to the start of
         * the binfile
         *)
        fun seek (IN{inS, base, ...}, pos) = let
              val newPos = base + pos
              val inS' = BinIO.getInstream inS
              in
                if BinIO.StreamIO.filePosIn inS' = newPos
                  then () (* the input stream is at the correct position *)
                  else (case (BinIO.StreamIO.getReader inS')
                     of rd as BinPrimIO.RD{setPos=SOME setPos, ...} => (
                          (* seek to the new position and then reset the input stream *)
                          setPos newPos;
                          BinIO.setInstream (
                            inS,
                            BinIO.StreamIO.mkInstream (rd, W8V.fromList[])))
                      | _ => (* error: inS does not support random access! *)
                    (* end case *))
              end

        (* `section (bf, id, inFn)` looks up the section with `id` in the binfile
         * `bf` and then uses `inFn` to read its contents.  Returns `NONE` when
         * the section is missing and `SOME contents` when the section is present
         * and `inFn` returns `contents`.
         *)
        fun section (bf, sectId, inFn) = (
            case findSection (header bf, sectId)
               of SOME desc => let
                    val () = seek (bf, base + #offset desc)
                    val sect = SECT{bf = bf, desc = desc}
                    in
                      SOME(inFn (sect, #size desc))
                    end
                | NONE => NONE
              (* end case *))

        fun bytesIn (_, 0) = Byte.stringToBytes ""
          | bytesIn (SECT{bf=IN{inS, ...}, ...}, n) = let
              val bv = BinIO.inputN (s, n)
              in
                if n = W8V.length bv
                  then bv
                  else error (concat[
                      "expected ", Int.toString n, " bytes, but found ",
                      Int.toString(W8V.length bv)
                    ])
              end

        fun bytes (sect, n) = bytesIn (inStrm sect, n)

        fun string (sect, n) = Byte.bytesToString (bytesIn (sect, n))

        fun int32 s = getInt32 (bytesIn(s, 4), 0)
        fun word32 s = getWord32 (bytesIn(s, 4), 0)

        fun packedInt (SECT{bf=IN{inS, ...}, ...}) = let
              fun loop n = (case BinIO.input1 inS
                     of NONE => error "unable to read a packed int32"
                      | SOME w8 => let
                          val n' = n * 0w128 + Word8.toLargeWord (Word8.andb (w8, 0w127))
                          in
                            if Word8.andb (w8, 0w128) = 0w0 then n' else loop n'
                          end
                    (* end case *))
              in
                LargeWord.toIntX (loop 0w0)
              end

        fun pid s = Pid.fromBytes (bytesIn (s, bytesPerPid))

(* NOTE: we are assuming an entry-point of 0, since that is always the value *)
        fun codeobj (sect, sz) = CodeObj.input(strm, codeSz, 0)

      end

    structure Out =
      struct

        datatype t = OUT of {
            hdr : Hdr.t,
            file : string option,
            outS : BinIO.outstream,
            sects : sect_data list ref
          }

        and sect_data = SD of {
            kind : SectId.t,                    (* the section kind *)
            szW : word,                         (* size of the section in 8-byte words *)
            outFn : BinIO.outstream -> unit     (* output function *)
          }

        (* for the section output functions, we pass in the output stream for
         * the binfile.
         *)
        datatype sect =  SECT of BinIO.outstream

        fun create (isArchive, smlnjVers, name, outS) = OUT{
                hdr = Hdr.mkHeader (isArchive, smlnjVers),
                file = name,
                outS = outS,
                sects = ref []
              }

        fun openFile (file, isArchive, smlnjVers) = let
              val outS = BinIO.openOut file
                    handle ex => (* error opening file *)
              in
                create (isArchive, smlnjVers, SOME file, outS)
              end

        fun openStream (outS, isArchive, smlnjVers) =
              create (isArchive, smlnjVers, NONE, outS)

        fun finish (OUT{hdr, outS, sects, ...}) = let
              (* reverse the list of sections and count them *)
              val (nSects, sections) = List.foldl
                    (fn (sect, (n, sects)) => (n+1, sect::sects))
                      (0, [])
                        (!sects)
              (* size of header including the section table in 8-byte words *)
              val hdrSzW = W.>>(W.fromInt(Hdr.sizeOfHdr nSects), 0w3)
              (* output a section descriptor *)
              fun outputSectDesc (SD{kind, szW, ...}, offset) = (
                    outputW32 (outS, kind);
                    outputW32 (outS, 0w0); (* reserved for future use *)
                    outputW32 (outS, offset);
                    outputW32 (outS, szW);
                    offset + szW)
              (* output the contents of a section *)
              fun outSect (SD{outFn, ...}) = let
                    val () = outFn outS;
                    val pos = W.fromLargeInt(Position.toLargeInt(BinIO.getPosOut outS))
                    val excess = W.andb(pos, 0w7)
                    in
                      (* add padding (if necessary) to ensure 8-byte alignment *)
                      if excess <> 0w0
                        then lp (0w8 - excess)
                        else ()
                    end
              in
                (* output the fixed part of the header *)
                if #isArchive hdr
                  then outputString(outS, "StabArch")
                  else outputString(outS, "BinFile ");
                outputW32 (outS, #version hdr);
                outputString (outS, #smlnjVersion hdr);
                outputI32 (outS, nSects);
                (* output the section table *)
                ignore (List.foldl outputSectDesc hdrSzW sections;
                (* output the sections *)
                List.app outSect sections;
                (* discard the sections *)
                sects := []
              end

        (* `section (bf, id, szb, outFn)` adds a section with the given `id`
         * and size `szb` in bytes.  The `outFn` is used to output the contents
         * of the section using the functions below.
         *)
        fun section (OUT{sects, ...}, sectId, szb, outFn) = let
              (* round size up and convert to number of 8-byte words *)
              val szW = W.>>(W.fromInt szb + 0w7, 0w3)
              in
                sects := SD{kind = sectId, szW = szW, outFn = outFn} :: !sects
              end

(* do we need this function? *)
        fun mkSMLNJVersion () = let
              val v = SMLNJVersion.toString (#smlnjVersion hdr)
              in
                (* the SML/NJ version string is limited to 16 bytes *)
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

        fun pid (SECT outS, pid) = BinIO.output (outS, Pid.toBytes pid)

      end

  end
