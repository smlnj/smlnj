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
    structure Pid = PersStamps
    structure BIO = BinIO
    structure SIO = BIO.StreamIO

    fun error msg = (
	  Control_Print.say (concat ["binfile format error: ", msg, "\n"]);
	  raise CodeObj.FormatError)

    (* convert a 8-byte-word offset to a byte file position *)
    fun toAlignedPos i = 8 * Position.fromInt i

    (* convert a 8-byte-word size to a byte size *)
    fun toPaddedSzb i = 8 * i

    (* convert a size in bytes to a padded size in 8-byte words *)
    fun padSize' szb = W.>>(W.fromInt szb + 0w7, 0w3)
    fun padSize szb = W.toInt(padSize' szb)

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
              fun toChr w = chr(W.toInt(W.andb(w, 0wxff)))
              in
                CharVector.fromList [
                    toChr id,
                    toChr(W.>>(id, 0w8)),
                    toChr(W.>>(id, 0w16)),
                    toChr(W.>>(id, 0w24))
                  ]
              end

        val binfile : t = fromString "BINF"
        val info : t = fromString "INFO"
        val import : t = fromString "IMPT"
        val export : t = fromString "EXPT"
        val pids : t = fromString "PIDS"
        val guid : t = fromString "GUID"
        val literals : t = fromString "LITS"
        val code : t = fromString "CODE"
        val cfkPickle : t = fromString "CFGP"
        val staticEnv : t = fromString "SENV"
        val libStamp : t = fromString "STMP"
        val depGraphPickle : t = fromString "PDGR"
        val padding : t = fromString "PAD "

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

        fun bfVersion (h : t) = #version h

        (* is the binfile an archive? *)
        fun isArchive ({isArchive, ...} : t) = isArchive

        fun smlnjVersion (hdr : t) = #smlnjVersion hdr

        fun sizeOfHdr nSects = fixedSize + sectDescSize * nSects

        type sect_desc = {
            kind : SectId.t,            (* 4-byte section ID *)
            flags : word,               (* flags (for future use) *)
            offsetW : int,              (* 8-byte word offset from start of binfile *)
            szW : int                   (* size in 8-byte words *)
          }

        type sect_tbl = sect_desc Vector.vector

        fun findSection (sects : sect_tbl, id : SectId.t) =
              Vector.find (fn {kind, ...} => id = kind) sects

      end

    structure In =
      struct
        datatype t = IN of {
            hdr : Hdr.t,
            file : string option,
            inS : BIO.instream,       (* the original input stream for the binfile *)
            base : Position.int,
            sects : Hdr.sect_tbl
          }

        datatype sect = SECT of {
            bf : t,
            desc : Hdr.sect_desc
          }

        fun getString (bv, base, n) =
              Byte.unpackStringVec (Word8VectorSlice.slice(bv, base, SOME n))

        (* get a little-endian 32-bit signed integer from the byte vector `bv`
         * at byte offset `i`.  We assume that the offset is 4-byte aligned.
         *)
        fun getInt32 (bv, i) = LargeWord.toIntX(PackWord32Little.subVecX(bv, i div 4))

        (* get a little-endian 32-bit unsigned integer from the byte vector `bv`
         * at byte offset `i`.  We assume that the offset is 4-byte aligned.
         *)
        fun getUInt32 (bv, i) = Word.fromLarge(PackWord32Little.subVec(bv, i div 4))

        fun create (name, inS, base) = let
              (* get the header *)
              val hdrData = BIO.inputN(inS, Hdr.fixedSize)
              val () = if (W8V.length hdrData <> Hdr.fixedSize)
                    then error "incomplete binfile header"
                    else ()
              (* decode the header *)
              val isArchive = (case getString(hdrData, 0, 8)
                     of "BinFile " => false
                      | "StabArch" => true
                      | s => error(concat[
                            "unknown binfile file kind '", String.toString s, "'"
                          ])
                    (* end case *))
              val vers = getUInt32(hdrData, 8)
              val () = if vers <> Hdr.version
                    then error(concat[
                        "invalid version number 0x", Word.toString vers
                      ])
                    else ()
              val smlnjVers = (case SMLNJVersion.fromString (getString(hdrData, 12, 16))
                     of SOME v => v
                      | NONE => error "malformed SML/NJ version in header"
                    (* end case *))
              val hdr = {
                      isArchive = isArchive,
                      version = vers,
                      smlnjVersion = smlnjVers
                    }
              (* get the number of sections *)
              val nSects = getInt32(hdrData, 28)
              val () = if (nSects < 0) orelse (Hdr.maxNSects < nSects)
                    then error(concat [
                        "invalid number of sections (", Int.toString nSects, ")"
                      ])
                    else ()
              (* read a section descriptor *)
              fun getSectDesc () = let
                    val descData = BIO.inputN(inS, Hdr.sectDescSize)
                    val () = if (W8V.length descData <> Hdr.sectDescSize)
                          then error "incomplete section description"
                          else ()
                    val kind = SectId.fromWord(getUInt32(descData, 0))
                    val flags = getUInt32(descData, 4)
                    val offset = Word.toInt(getUInt32(descData, 8))
                    val sz = Word.toInt(getUInt32(descData, 12))
                    in {
                      kind = kind,
                      flags = flags,
                      offsetW = offset,
                      szW = sz
                    } end
(* TODO: validate the table *)
              val sects = Vector.tabulate (nSects, fn _ => getSectDesc())
              in
                IN{hdr = hdr, file = name, inS = inS, base = base, sects = sects}
              end

(* TODO: check that the file exists *)
        fun openFile file = let
              val inS = BIO.openIn file
              in
                create (SOME file, inS, 0)
              end

        fun openStream inS = create (NONE, inS, 0)

        fun header (IN{hdr, ...}) = hdr

        (* is the binfile an archive? *)
        fun isArchive (IN{hdr={isArchive, ...}, ...}) = isArchive

        fun findSection (IN{sects, ...}, id : SectId.t) =
              Vector.find (fn {kind, ...} => id = kind) sects

        (* seek to the specified file position, which is relative to the start of
         * the binfile
         *)
        fun seek (IN{inS, base, ...}, pos) = let
              val newPos = base + pos
              val inS' = BIO.getInstream inS
              in
                if SIO.filePosIn inS' = newPos
                  then () (* the input stream is at the correct position *)
                  else (case (SIO.getReader inS')
                     of (rd as BinPrimIO.RD{setPos=SOME setPos, ...}, bv) => (
                          (* seek to the new position and then reset the input stream *)
                          setPos newPos;
                          BIO.setInstream (
                            inS,
                            SIO.mkInstream (rd, W8V.fromList[])))
                      | _ => error "binfile does not support random access"
                    (* end case *))
              end

        (* `section (bf, id, inFn)` looks up the section with `id` in the binfile
         * `bf` and then uses `inFn` to read its contents.  Returns `NONE` when
         * the section is missing and `SOME contents` when the section is present
         * and `inFn` returns `contents`.
         *)
        fun section (bf, sectId, inFn) = (
            case findSection (bf, sectId)
               of SOME desc => let
                    val () = seek (bf, toAlignedPos(#offsetW desc))
                    val sect = SECT{bf = bf, desc = desc}
                    in
                      SOME(inFn (sect, toPaddedSzb(#szW desc)))
handle ex => (
Control_Print.say(concat["** Exception: ", General.exnMessage ex, "\n"]);
Control_Print.say(concat[
"  in `section (-, '", String.toString(SectId.toString sectId), "', -)`; pos = 0x",
Position.fmt StringCvt.HEX (toAlignedPos(#offsetW desc)), "; szb = ",
Int.toString(toPaddedSzb(#szW desc)), "\n"
]);
raise ex)
                    end
                | NONE => NONE
              (* end case *))

        fun inStrm (SECT{bf=IN{inS, ...}, ...}) = inS

        fun bytes (_, 0) = Byte.stringToBytes ""
          | bytes (sect, n) = let
              val bv = BIO.inputN (inStrm sect, n)
              in
                if n = W8V.length bv
                  then bv
                  else error (concat[
                      "expected ", Int.toString n, " bytes, but found ",
                      Int.toString(W8V.length bv)
                    ])
              end

        fun string (sect, n) = Byte.bytesToString (bytes (sect, n))

        fun int32 sect = getInt32 (bytes(sect, 4), 0)
        fun word32 sect = getUInt32 (bytes(sect, 4), 0)

        val decodePackedInt = LEB128.decodeInt SIO.input1

        fun packedInt sect = let
              val inS = inStrm sect
              in
                case decodePackedInt (BIO.getInstream inS)
                 of SOME(n, inS') => (
                      BIO.setInstream (inS, inS');
                      n)
                  | NONE => error "unable to read a packed int"
                (* end case *)
              end

        fun pid sect = Pid.fromBytes (bytes (sect, Pid.persStampSize))

(* NOTE: we are assuming an entry-point of 0, since that is always the value *)
        fun codeObject (sect, sz) = CodeObj.input(inStrm sect, sz, 0)

      end

    structure Out =
      struct

        datatype t = OUT of {
            hdr : Hdr.t,
            file : string option,
            outS : BIO.outstream,
            sects : sect_data list ref
          }

        and sect_data = SD of {
            kind : SectId.t,            (* the section kind *)
            szW : word,                 (* size of the section in 8-byte words *)
            outFn : sect -> unit        (* output function *)
          }

        (* for the section output functions, we pass in the output stream for
         * the binfile.
         *)
        and sect =  SECT of BIO.outstream

        fun outputW32 (outS, w) = let
              fun outB w = BIO.output1 (outS, Word8.fromLarge(Word.toLarge w))
              in
                outB w;
                outB (Word.>>(w, 0w8));
                outB (Word.>>(w, 0w16));
                outB (Word.>>(w, 0w24))
              end

        fun outputI32 (outS, n) = outputW32 (outS, Word.fromInt n)

        fun outputPid (outS, pid) = BIO.output (outS, Pid.toBytes pid)

        fun outputString (outS, s) = BIO.output (outS, Byte.stringToBytes s)

        fun create (isArchive, smlnjVers, name, outS) = OUT{
                hdr = Hdr.mkHeader (isArchive, smlnjVers),
                file = name,
                outS = outS,
                sects = ref []
              }

        fun openFile (file, isArchive, smlnjVers) = let
              val outS = BIO.openOut file
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
              val hdrSzW = padSize'(Hdr.sizeOfHdr nSects)
              (* output a section descriptor *)
              fun outputSectDesc (SD{kind, szW, ...}, offsetW) = (
                    outputW32 (outS, kind);
                    outputW32 (outS, 0w0); (* reserved for future use *)
                    outputW32 (outS, offsetW);
                    outputW32 (outS, szW);
                    offsetW + szW)
              (* output the contents of a section *)
              fun outSect (sect as SD{outFn, ...}) = let
                    val () = outFn (SECT outS);
                    val curPos = SIO.filePosOut(BIO.getPosOut outS)
                    val pos = W.fromLargeInt(Position.toLarge curPos)
                    val excess = W.andb(pos, 0w7)
                    fun pad 0w0 = ()
                      | pad p = (BIO.output1 (outS, 0w0); pad (p - 0w1))
                    in
                      (* add padding (if necessary) to ensure 8-byte alignment *)
                      if excess <> 0w0
                        then pad (0w8 - excess)
                        else ()
                    end
              (* the SML/NJ version field is trimmed/padded to 16 characters *)
              val smlnjVersion = let
                    val s = SMLNJVersion.toString(#smlnjVersion hdr)
                    in
                      if (size s < 16) then StringCvt.padRight #" " 16 s
                      else if (size s > 16) then substring (s, 0, 16)
                      else s
                    end
              in
                (* output the fixed part of the header *)
                if #isArchive hdr
                  then outputString(outS, "StabArch")
                  else outputString(outS, "BinFile ");
                outputW32 (outS, #version hdr);
                outputString (outS, smlnjVersion);
                outputI32 (outS, nSects);
                (* output the section table *)
                ignore (List.foldl outputSectDesc hdrSzW sections);
                (* output the sections *)
                List.app outSect sections;
                (* discard the sections *)
                sects := []
              end

        (* `section (bf, id, szb, outFn)` adds a section with the given `id`
         * and size `szb` in bytes.  The `outFn` is used to output the contents
         * of the section using the functions below.
         *)
        fun section (OUT{sects, ...}, sectId, szb, outFn) =
              sects := SD{kind = sectId, szW = padSize' szb, outFn = outFn} :: !sects

(* do we need this function?
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
*)

        fun bytes (SECT outS, v) = BIO.output (outS, v)

        fun string (SECT outS, s) = outputString (outS, s)

        fun packedInt (SECT outS, n) = BIO.output (outS, LEB128.intToBytes n)

        fun word32 (SECT outS, w) = outputW32 (outS, w)

        fun int32 (SECT outS, n) = outputI32 (outS, n)

        fun pid (SECT outS, pid) = outputPid (outS, pid)

        fun codeObject (SECT outS, code) = CodeObj.output (outS, code)

      end

  end
