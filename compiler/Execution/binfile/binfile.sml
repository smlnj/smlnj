(* binfile.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * author: Matthias Blume
 *
 * See dev-notes/binfile.adoc for a description of the binfile format.
 * This file must be kept in sync with runtime/kernel/boot.c.
 *)

structure Binfile :> BINFILE =
  struct

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

    val bfVersion = 0wx20211123         (* Bin File version 2021-11-23 *)

    fun mkVersion {arch, smlnjVersion} : version_info = {
            bfVersion = 0w0,        (* old binfile format for now *)
            arch = arch, smlnjVersion = smlnjVersion
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

    val bytesPerPid = Pid.persStampSize
    val oldVersInfoSzb = 16
    val newVersInfoSzb = 40     (* post 2021.1 *)

    val binFileKind = "BinFile "

    val exportPidOf = #exportPid o unBF
    val cmDataOf = #cmData o unBF
    val senvPickleOf = #senv o unBF
    val staticPidOf = #pid o senvPickleOf

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
    fun readPidList (s, n) = List.tabulate (n, fn _ => readPid s)

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

    local
      fun pickleImportSpec ((selector, tree), (n, p)) = let
	    val sp = picklePackedInt32 selector
	    val (n', p') = pickleImportTree (tree, (n, p))
	    in
	      (n', sp :: p')
	    end
      and pickleImportTree (ImportTree.ITNODE [], (n, p)) = (n + 1, picklePackedInt32 0 :: p)
	| pickleImportTree (ImportTree.ITNODE l, (n, p)) = let
	    val (n', p') = foldr pickleImportSpec (n, p) l
	    in
	      (n', picklePackedInt32 (length l) :: p')
	    end

      fun pickleImport ((pid, tree), (n, p)) = let
	    val (n', p') = pickleImportTree (tree, (n, p))
	    in
	      (n', Pid.toBytes pid :: p')
	    end
    in
    fun pickleImports l = let
	  val (n, p) = foldr pickleImport (0, []) l
	  in
	    (n, W8V.concat p)
	  end
    end (* local *)

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
	  val code = if avail < 0 then error "code size" else CodeObj.input(strm, codeSz)
	  in
	    CodeObj.set_entrypoint (code, ep);
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

    fun writeCSegs (s, {code, data}) = (
	  writeInt32 s (W8V.length data);
	  writeInt32 s 0;  (* dummy entry point for data segment *)
	  BinIO.output(s, data);
	  writeInt32 s (CodeObj.size code);
	  writeInt32 s (CodeObj.entrypoint code);
	  CodeObj.output (s, code))

    fun write { stream = s, contents, nopickle } = let
	(* Keep this in sync with "size" (see above). *)
	  val { version, imports, exportPid, cmData, senv, csegments, guid, ... } = unBF contents
          val oldFormat = (#bfVersion version = 0w0)
	  val { pickle = senvP, pid = staticPid } = senv
          val envPids = if oldFormat
                then staticPid :: staticPid :: cmData (* second PID is unused lambdaPid *)
                else staticPid :: cmData
	  val (leni, picki) = pickleImports imports
	  val importSzB = W8V.length picki
	  val (ne, epl) = (case exportPid
		 of NONE => (0, [])
	          | SOME p => (1, [p])
		(* end case *))
	  val nei = length envPids
	  val cmInfoSzB = nei * bytesPerPid
	  fun pickleSize { pid, pickle } = if nopickle then 0 else W8V.length pickle
	  val g = String.size guid
	  val pad = 0			(* currently no padding *)
	  val cs = codeSize csegments
	  val es = pickleSize senv
	  val writeEnv = if nopickle
		then fn () => ()
		else fn () => BinIO.output (s, senvP)
	  val datasz = W8V.length (#data csegments)
        (* the various length fields; the `lambdaSz` field is omitted in newer versions *)
          val fields = let
                val flds = [g, pad, cs, es]
                val flds = if oldFormat
                      then 0 :: flds    (* include 0 for unused lambda size *)
                      else flds
                in
                  leni :: ne :: importSzB :: cmInfoSzB :: flds
                end
	  in
	    BinIO.output (s, Byte.stringToBytes (mkVersionInfo version));
	    app (writeInt32 s) fields;
	    BinIO.output (s, picki);
	    writePidList (s, epl);
	    (* arena1 *)
	    writePidList (s, envPids);
	    (* GUID area *)
	    BinIO.output (s, Byte.stringToBytes guid);
	    (* padding area is currently empty *)
	    writeCSegs (s, csegments);
	    writeEnv ();
	    { env = es, data = datasz, code = cs }
	  end

    fun exec (BF {imports, exportPid, executable, csegments, ... }, dynEnv, exnWrapper) = let
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

  end (* structure Binfile *)
