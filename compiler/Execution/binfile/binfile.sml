(* binfile.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * author: Matthias Blume and John Reppy
 *
 * This file is an implementation of the old `BINFILE` signature using the new
 * binfile format, which is described in
 *
 *      https://github.com/smlnj/.github/wiki/New-Binfile-Format
 *)

structure Binfile :> BINFILE =
  struct

    structure BFIO = BinfileIO
    structure Pid = PersStamps
    structure W8V = Word8Vector
    structure SS = Substring

    exception FormatError = CodeObj.FormatError

    type pid = Pid.persstamp

    type csegments = CodeObj.csegments

    type executable = CodeObj.executable

    type stats = { env: int, data: int, code: int }

    type pickle = { pid: pid, pickle: W8V.vector }

    type version_info = {
        bfVersion : word,       (* will be 0w0 for old versions *)
        arch : string,
        smlnjVersion : string
      }

    datatype bfContents = BF of {
        version : version_info,
	imports: ImportTree.import list,
	exportPid: pid option,
	cmData: pid list,
	senv: pickle,
	guid: string,
	csegments: csegments,
	executable: executable option ref
      }

    fun unBF (BF x) = x

    val version = #version o unBF
    val staticPidOf = #pid o senvPickleOf
    val exportPidOf = #exportPid o unBF
    val cmDataOf = #cmData o unBF
    val senvPickleOf = #senv o unBF
    val guidOf = #guid o unBF

    fun create { version, imports, exportPid, cmData, senv, csegments, guid } = BF{
            version = version,
	    imports = imports,
	    exportPid = exportPid,
	    cmData = cmData,
	    senv = senv,
	    guid = guid,
	    csegments = csegments,
	    executable = ref NONE
	  }

    val bytesPerPid = Pid.persStampSize

(* mapping from old binfile format to sections:
        header info                             ==> "INFO"
        imports                                 ==> "IMPT"
        export pid list                         ==> "EXPT"
        static pid + environment pids           ==> "PIDS" (static pid is in "SENV")
        guid                                    ==> "GUID"
        literal data segment                    ==> "LITS"
        code segments                           ==> "CODE"
        static environment (conditional)        ==> "SENV" (includes pid)
 *)

    (***** SECTIONS *****)

    type info_sect = {
        arch : string,
        opsys : string
      }

    (***** INPUT OPERATIONS *****)

    fun readGUid s = let
          val bf = BFIO.openInstream stream
          val hdr = BFIO.getHeader bf
          in
          end

(* FIXME: instead of passing the `version_info`, we should just pass the expected
 * SML/NJ version and architecture.
 *)
    fun read { version : version_info, stream } = let
          val bf = BFIO.In.openStream stream
          val hdr = BFIO.getHeader bf
          (* check that we have a Binfile and not an archive *)
          val _ = if (BFIO.Hdr.isArchive hdr)
                then BFIO.error "unexpected stable archive"
                else ()
          (* helper function to get a section *)
          fun readSection (id, inFn) = (case BFIO.In.section(bf, id, inFn)
                 of SOME contents => contents
                  | NONE => BFIO.error(concat[
                        "missing '", BFIO.SectId.toString sectId, "' section"
                      ])
                (* end case *))
          (* get the `info` section *)
          val {arch, opsys} = let
                fun inFn (sect, 16) = let
                      val arch = BFIO.In.string(sect, 8)
                      val opsys = BFIO.In.string(sect, 8)
                      in
                        {arch = arch, opsys = opsys}
                      end
                  | inFn (_, sz) = BFIO.error(concat[
                        "invalid size (", Int.toString sz, " bytes) for info section"
                      ])
                in
                  readSection (BFIO.SectId.info, inFn)
                end
          (* compute the old-style version info for the binfile *)
          val version' = let
                val smlnjVers = let
                      val vn = String.concatWithMap "." Int.toString
                            (#id (#smlnjVersion hdr))
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
                in
                  { bfVersion = #version hdr, arch = arch, smlnjVersion = smlnjVers }
                end
(* TODO: check version *)
(* TODO: read contents from sections *)
	  in {
	    contents = create {
                version = version',
		imports = imports,
		exportPid = exportPid,
		cmData = cmData,
		senv = { pid = staticPid, pickle = penv },
		guid = guid,
		csegments = {data = data, code = code}
	      },
	    stats = {
		env = W8V.length penv,
                code = W8V.length code,
		data = W8V.length data
	      }
	  } end

    (***** OUTPUT OPERATIONS *****)

    (* pad/trim a string to fixed length *)
    fun padString (s, n) = if (size s > n)
            then substring (s, 0, n)
          else if (size s < n)
            then StringCvt.padLeft #" " n s
            else s

    (** info ('INFO') section **)
    val infoSize = 16
    fun addInfoSection (bf, arch, opsys) = let
          fun outFn sect = (
                BFIO.string(sect, padString(arch, 8));
                BFIO.string(sect, padString(opsys, 8)))
          in
            BFIO.Out.section (bf, BFIO.SectId.info, infoSize, outFn)
          end

    (** imports ('IMPT') section **)
    local
      structure B = Word8Buffer
      structure IT = ImportTree
      (* encode a 32-bit integer as a little-endian sequence of 7-bit digits
       * with an extension bit
       *)
      fun emitPackedInt32 (buf, 0) = B.add1(bud, 0w0)
        | emitPackedInt32 (buf, i) = let
            val // = LargeWord.div
            val %% = LargeWord.mod
            val !! = LargeWord.orb
            infix // %% !!
            val toW8 = Word8.fromLargeWord
            fun emit 0w0 = ()
              | emit n = (
                  B.add1 (buf, toW8(n %% 0w128) !! 0w128);
                  emit (n // 0w128))
            in
              emit (LargeWord.fromInt i)
            end
      fun emitImportSpec ((selector, tree), (n, p)) = (
            emitPackedInt32 selector;
            emitImportTree tree)
      and emitImportTree (IT.ITNODE [], (n, p)) = emitPackedInt32 0
	| emitImportTree (IT.ITNODE l, (n, p)) = (
            emitPackedInt32 (List.length l);
            List.app emitImportSpec l)
      fun emitImport (pid, tree) = (
            B.addVec (buf, Pid.toBytes pid);
            emitImportTree tree)
    in
    (* compute the size of the tree and the number of leaves *)
    fun importTreeSize trees = let
          (* packed ints use one byte per seven bits of source data *)
          fun packedIntSz (n, sz) = let
                val n = LargeWord.fromInt i
                in
                  if (n < 0wx80) then sz + 1
                  else if (n < 0wx4000) then sz + 2
                  else if (n < 0wx200000) then sz + 3
                  else if (n < 0wx10000000) then sz + 4
                  else sz + 5
                end
          fun itreeSz (IT.ITNODE[], (nl, sz)) =
                (nl + 1, sz + 1)
            | itreeSz (IT.ITNODE l, (nl, sz))) =
                List.foldl specSz (nl, packedIntSz (length l, sz)) l
          and specSz ((selector, tree), (nl, sz)) =
                itreeSz (tree, (nl, packedIntSz (selector, sz)))
          and importSz ((pid, tree), (nl, sz)) = itreeSz (tree, (nl, sz + bytesPerPid))
          val (nLeaves, sz) = List.foldl importSz (0, 0) trees
          in
            (nLeaves, packedIntSz(nLeaves, sz))
          end
    (* add an import tree section to the binfile *)
    fun addImportTreeSection (bf, l) = let
          val (nLeaves, sz) = importTreeSize l
          fun outFn sect = let
                val buf = B.new sz
                val () = (
                      emitPackedInt32 nLeaves;
                      List.app emitImport l)
                val res = B.contents buf
                in
                  if (W8V.length res <> sz)
                    then raise Fail "Internal error: incorrect size for imports"
                    else ();
                  BFIO.Out.bytes (sect, res)
                end
          in
            BFIO.Out.section (bf, BFIO.SectId.import, sz, outFn)
          end
    end (* local *)

    (** export ('EXPT') section **)
    fun exportSize NONE = 0
      | exportSize (SOME _) = bytesPerPid
    fun addExportSection (bf, optExportPid) = let
          val (sz, outFn) = (case optExportPid
                 of NONE => (0, fn _ => ())
                  | SOME pid => (bytesPerPid, fn sect => BFIO.pid(sect, pid))
                (* end case *))
          in
            BFIO.Out.section (bf, BFIO.SectId.export, sz, outFn)
          end

    (** pids ('PIDS') section *)
    fun pidsSize pids = bytesPerPid * List.length pids
    fun addPidsSection (bf, pids) = let
          val sz = pidsSize pids
          fun outFn sect = List.app (fn pid => BFIO.pid(sect, pid)) pids
          in
            BFIO.Out.section (bf, BFIO.SectId.pids, sz, outFn)
          end

    (** Guid ('GUID') section *)
    fun guidSz guid = String.size guid
    fun addGuidSection (bf, guid) = let
          val sz = guidSz guid
          fun outFn sect = BFIO.string(sect, guid)
          in
            BFIO.Out.section (bf, BFIO.SectId.guid, sz, outFn)
          end

    (** literal ('LITS') section **)
    fun literalSize data = W8V.length data
    fun addLiteralSection (bf, data) = let
          val sz = literalSize data
          fun outFn sect = BFIO.Out.bytes(sect, data)
          in
            BFIO.Out.section (bf, BFIO.SectId.literals, sz, outFn)
          end

    (** code ('CODE') section **)
    fun codeSize code = CodeObject.size code
    fun addCodeSection (bf, code) = let
          val sz = codeSize code
          fun outFn sect = (
                BFIO.Out.int32(sect, CodeObj.entrypoint code);
                BFIO.Out.codeobj(sect, code))
          in
            BFIO.Out.section (bf, BFIO.SectId.code, sz, outFn)
          end

    (** static environment ('SENV') section **)
    fun staticEnvSize (_, _, false) = bytesPerPid
      | staticEnvSize (_, pkl, true) = bytesPerPid + W8V.length pkl
    fun addStaticEnvSection (bf, pid, pkl, nopickle) = let
          val sz = staticEnvSize (bf, pid, pkl, nopickle)
          fun outFn sect = (
                BFIO.Out.pid (sect, pid);
                if nopickle
                  then ()
                  else BFIO.Out.bytes (bf, pkl))
          in
            BFIO.Out.section (bf, BFIO.SectId.staticEnv, sz, outFn)
          end

    fun size { contents, nopickle } = let
	  val BF{ version, imports, exportPid, cmData, senv, csegments, guid, ... } =
                contents
          val { bfVersion, arch, smlnjVersion } = version
	  val { pickle = senvPkl, pid = staticPid } = senv
          val { code, data } = csegments
          (* pad section sizes to a multiple of 8 bytes *)
          fun padSize sz = Word.toIntX(Word.andb(Word.fromInt sz + 0w7, Word.notb 0w7))
          in
            BFIO.Hdr.sizeOfHdr 8
              + infoSize
              + padSize (importTreeSize imports)
              + padSize (exportSize exportPid)
              + padSize (pidsSize cmData)
              + padSize (guidSize guid)
              + padSize (literalSize data)
              + padSize (codeSize code)
              + padSize (staticEnvSize (staticPid, senvPkl, nopickle))
          end

    fun write { stream, contents, nopickle } = let
	  val BF{ version, imports, exportPid, cmData, senv, csegments, guid, ... } =
                contents
          val { bfVersion, arch, smlnjVersion } = version
	  val { pickle = senvPkl, pid = staticPid } = senv
          val { code, data } = csegments
          val bf = BFIO.Out.openStream (stream, BFIO.Hdr.BinFile, smlnjVersion)
          val stats = {
                    env = if nopickle then 0 else W8V.length senvPkl,
                    data = W8V.length data,
                    code = CodeObject.size code
                  }
          in
(* FIXME: eventually, we should include useful OS info in the info section *)
            addInfoSection (bf, arch, "");
            addImportTreeSection (bf, imports);
            addExportSection (bf, exportPid);
            addPidsSection (bf, cmData);
            addGuidSection (bf, guid);
            addLiteralSection (bf, data);
            addCodeSection (bf, code);
            addStaticEnvSection (bf, staticPid, senvPkl, nopickle);
            BFIO.Out.finish bf;
	    stats
          end

    fun exec (bf, dynEnv, exnWrapper) = let
          val BF{ imports, exportPid, executable, csegments, ... } = bf
	  val executable = (case !executable
		 of SOME e => e
		  | NONE => let
		      val e = Isolate.isolate (
			        Execute.mkExec { cs = csegments, exnWrapper = exnWrapper })
		      in
			executable := SOME e; e
		      end
		(* end case *))
	  in
	    Execute.execute {
		executable = executable,
		imports = imports,
		exportPid = exportPid,
		dynEnv = dynEnv
	      }
	  end








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

    fun version (BF{hdr, ...}) = let
          val {arch, opsys} = (* get info section *)
          in {
            bfVersion = #version hdr,
            arch = arch,
            smlnjVersion = mkSMLNJVersion hdr
          } end

    val bytesPerPid = Pid.persStampSize

    val staticPidOf = #pid o senvPickleOf
    val exportPidOf = #exportPid o unBF
    val cmDataOf = #cmData o unBF
    val senvPickleOf = #senv o unBF
    val guidOf = #guid o unBF

    fun error msg = (
	  Control_Print.say (concat ["binfile format error: ", msg, "\n"]);
	  raise FormatError)

    val fromInt = Word32.fromInt
    val fromByte = Word32.fromLargeWord o Word8.toLargeWord
    val toByte = Word8.fromLargeWord o Word32.toLargeWord
    val >> = Word32.>>
    infix >>

    fun bytesIn (s, 0) = Byte.stringToBytes ""
      | bytesIn (s, n) = let
	  val bv = BinIO.inputN (s, n)
	  in
	    if n = W8V.length bv
	      then bv
	      else error (concat[
		  "expected ", Int.toString n, " bytes, but found ",
		  Int.toString(W8V.length bv)
		])
	  end

    fun readInt32 s = LargeWord.toIntX(PackWord32Big.subVec(bytesIn(s, 4), 0))

    fun readPackedInt32 s = let
	  fun loop n = (case BinIO.input1 s
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

    fun readPid s = Pid.fromBytes (bytesIn (s, bytesPerPid))
    fun readPidList (s, n) = List.tabulate (n , fn _ => readPid s)

    fun readImportTree s = (case readPackedInt32 s
	   of 0 => (ImportTree.ITNODE [], 1)
	    | cnt => let
		fun readImportList 0 = ([], 0)
		  | readImportList cnt = let
		      val selector = readPackedInt32 s
		      val (tree, n) = readImportTree s
		      val (rest, n') = readImportList (cnt - 1)
		      in
			((selector, tree) :: rest, n + n')
		      end
		val (l, n) = readImportList cnt
		in
		  (ImportTree.ITNODE l, n)
		end
	  (* end case *))

    fun readImports (s, n) = if n <= 0
	  then []
	  else let
	    val pid = readPid s
	    val (tree, n') = readImportTree s
	    val rest = readImports (s, n - n')
	    in
	      (pid, tree) :: rest
	    end


    fun mkVersionInfo {bfVersion=0w0, arch, smlnjVersion} = let
        (* old-style binfile.  The version info is a 16-byte string formed from the
         * architecture and SML/NJ version number. The basic format is "<arch>-<version>",
         * with the architecture limited to 7 characters and the verion limited to 8
         * (one character for the "-").  It is padded with spaces as necessary to
         * fill out 16 bytes.
         *)
	  val vbytes = 8			        (* version part; allow for xxxx.y.z *)
	  val abytes = oldVersInfoSzb - vbytes - 1      (* arch part *)
	  fun trim (i, s) = if (size s > i) then substring (s, 0, i) else s
          val v = trim (vbytes, smlnjVersion)
	  val a = trim (abytes, arch)
	  in
            StringCvt.padRight #" " oldVersInfoSzb (concat[a, "-", v])
	    (* assert (W8V.length (MAGIC <arch>) = magicBytes *)
	  end
      | mkVersionInfo {bfVersion, arch, smlnjVersion} = let
          fun byte shft = chr(Word.toIntX(Word.andb(Word.>>(bfVersion, shft), 0wxff)))
          val vers = String.implode [
                  byte 0w24, byte 0w16, byte 0w8, byte 0w0
                ]
	  fun fixWid (w, s) = if (size s > w)
                  then substring (s, 0, w)
                else if (size s < w)
                  then StringCvt.padRight #" " w s
                  else s
          in
            String.concat[binFileKind, vers, fixWid(12, arch), fixWid(16, smlnjVersion)]
          end

    fun versionInfoSzb ({bfVersion=0w0, ...} : version_info) = oldVersInfoSzb
      | versionInfoSzb _ = newVersInfoSzb

  (* calculate size of code objects (including lengths and entrypoints) *)
    fun codeSize (csegs: csegments) =
	  CodeObj.size(#code csegs) + W8V.length(#data csegs) + 16

  (* This function must be kept in sync with the "write" function below.
   * It calculates the number of bytes written by a corresponding
   * call to "write".
   *)
    fun size { contents, nopickle } = let
	  val { version, imports, exportPid, senv, cmData, csegments, guid, ... } =
		unBF contents
          val oldFormat = (#bfVersion version = 0w0)
	  val (_, picki) = pickleImports imports
	  val hasExports = isSome exportPid
	  fun pickleSize { pid, pickle } = if nopickle then 0 else W8V.length pickle
        (* the number of length fields depends on the file-format version *)
          val fieldsSz = if oldFormat then 9 * 4 else 8 * 4
        (* the number of environment PIDs depends on the file-format version *)
          val nei = length cmData + (if oldFormat then 2 else 1)
	  in
	    versionInfoSzb version +
	    fieldsSz +
	    W8V.length picki +
	    (if hasExports then bytesPerPid else 0) +
	    bytesPerPid * nei +
	    String.size guid +
	    codeSize csegments +
	    pickleSize senv
	  end

    fun create { version, imports, exportPid, cmData, senv, csegments, guid } = BF{
            version = version,
	    imports = imports,
	    exportPid = exportPid,
	    cmData = cmData,
	    senv = senv,
	    guid = guid,
	    csegments = csegments,
	    executable = ref NONE
	  }

  (* read the version info; we first read oldVersInfoSzb bytes and check for a 2021.1+
   * header format.  If so, we read the additional bytes.
   *)
    fun readVersionInfo s = let
          val blk = bytesIn (s, oldVersInfoSzb)
          fun trimWS (s, i, n) = SS.string(SS.dropr Char.isSpace (SS.extract(s, i, SOME n)))
          in
            (* the first 8 bytes of the 2021.1+ binfile format is the file kind,
             * so we check for that first.
             *)
            if Byte.unpackStringVec(Word8VectorSlice.slice(blk, 0, SOME 8)) = binFileKind
              then let (* new format *)
                fun byte i = Word.fromLargeWord(Word8.toLargeWord(W8V.sub(blk, i)))
                val bfV = Word.<<(byte 8, 0w24) + Word.<<(byte 9, 0w16)
                      + Word.<<(byte 10, 0w8) + byte 11
                val hdr = Byte.bytesToString(W8V.concat[
                        blk, bytesIn (s, newVersInfoSzb - oldVersInfoSzb)
                      ])
                val a = trimWS(hdr, 12, 12)
                val v = trimWS(hdr, 24, 16)
                in
(*
Control_Print.say (concat [
"readVersionInfo NEW [", String.toString hdr ^ "]\n"
]);
Control_Print.say (concat["  a = \"", String.toString a, "\"\n"]);
Control_Print.say (concat["  v = \"", String.toString v, "\"\n"]);
*)
                  {bfVersion = bfV, arch = a, smlnjVersion = v}
                end
              else let (* old format *)
                val magic = SS.dropr Char.isSpace (SS.full (Byte.bytesToString blk))
                val (a, v) = SS.splitl (fn #"-" => false | _ => true) magic
                in
(*
Control_Print.say (concat [
"readVersionInfo OLD [", String.toString(SS.string magic) ^ "]\n"
]);
*)
                  {bfVersion = 0w0, arch = SS.string a, smlnjVersion = SS.string(SS.triml 1 v)}
                end
          end

  (* must be called with second arg >= 0 *)
    fun readCSegs (strm, nbytes) = let
	  val dataSz = readInt32 strm
	  val _ = readInt32 strm (* ignore entry point field for data segment *)
	  val avail = nbytes - dataSz - 8
	  val data = if avail < 0 then error "data size" else bytesIn (strm, dataSz)
	  val codeSz = readInt32 strm
	  val ep = readInt32 strm
	  val avail = avail - codeSz - 8
	  val code = if avail < 0
                then error "code size"
                else CodeObj.input(strm, codeSz, ep)
	  in
	    { code = code, data = data }
	  end

    fun readGUid s = let
	  val _ = readVersionInfo s
	  val _ = readInt32 s
	  val ne = readInt32 s
	  val importSzB = readInt32 s
	  val cmInfoSzB = readInt32 s
	  val nei = cmInfoSzB div bytesPerPid
	  val lambdaSz = readInt32 s
	  val g = readInt32 s
	  val _ = bytesIn (s, importSzB + 3 * 4)
	  val _ = bytesIn (s, ne * bytesPerPid)
	  val _ = readPidList (s, nei)
	  val _ = bytesIn (s, lambdaSz)
	  in
	    Byte.bytesToString (bytesIn (s, g))
	  end

    fun read { version : version_info, stream = s } = let
	  val version' = readVersionInfo s
          val oldFormat = (#bfVersion version = 0w0) orelse (#bfVersion version' = 0w0)
        (* trim the SML/NJ version string to the maximum length supported by the
         * binfile header format.
         *)
          fun smlnjVersion ({smlnjVersion=v, ...} : version_info) = let
                val len = if oldFormat then 8 else 16
                in
                  if String.size v > len
                    then String.substring(v, 0, len)
                    else v
                end
          val _ = if #arch version <> #arch version'
		then error (concat[
		    "incorrect architecture \"", String.toString(#arch version'),
		    "\", expected \"", #arch version, "\""
		  ])
              else if smlnjVersion version <> smlnjVersion version'
		then error (concat[
		    "incorrect compiler version \"", String.toString(#smlnjVersion version'),
		    "\", expected \"", #smlnjVersion version, "\""
		  ])
                else ()
	  val leni = readInt32 s
	  val ne = readInt32 s
	  val importSzB = readInt32 s
	  val cmInfoSzB = readInt32 s
	  val nei = cmInfoSzB div bytesPerPid
        (* read unused lambda size (old format only) *)
	  val _ = if #bfVersion version' = 0w0 then readInt32 s else 0
	  val g = readInt32 s
	  val pad = readInt32 s
	  val cs = readInt32 s
	  val es = readInt32 s
	  val imports = readImports (s, leni)
	  val exportPid = (case ne
		 of 0 => NONE
		  | 1 => SOME(readPid s)
		  | _ => error "too many export PIDs"
		(* end case *))
	  val envPids = readPidList (s, nei)
	  val (staticPid, cmData) = (case (envPids, #bfVersion version')
		 of (st :: _ :: cmData, 0w0) =>
                    (* NOTE: the second item is the lambdaPID, which is not used *)
                      (st, cmData)
                  | (st :: cmData, _) => (st, cmData) (* new format does not have lambdaPID *)
		  | _ => error "env PID list"
		(* end case *))
	  val guid = Byte.bytesToString (bytesIn (s, g))
	(* skip padding *)
	  val _ = if pad <> 0 then ignore (bytesIn (s, pad)) else ()
	(* now get the code *)
	  val code = readCSegs (s, cs)
	  val penv = bytesIn (s, es)
	  in {
	    contents = create {
                version = version',
		imports = imports,
		exportPid = exportPid,
		cmData = cmData,
		senv = { pid = staticPid, pickle = penv },
		guid = guid,
		csegments = code
	      },
	    stats = {
		env = es, code = cs,
		data = W8V.length (#data code)
	      }
	  } end


  end (* structure Binfile *)
