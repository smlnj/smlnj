(* old-binfile.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * author: Matthias Blume
 *
 * This is a temporary file that supports reading the old binfile
 * format as we transition to the new format.
 *
 * See dev-notes/binfile.adoc for a description of the binfile format.
 * This file must be kept in sync with runtime/kernel/boot.c.
 *)

structure OldBinfile :> sig

    type version_info = {
        bfVersion : word,       (* will be 0w0 for old versions *)
        arch : string,
        smlnjVersion : string
      }

    type pid = PersStamps.persstamp

    type csegments = {
	code : CodeObj.t,
	data : Word8Vector.vector       (* literals *)
      }

    type executable = CodeObj.executable

    type stats = { env: int, data: int, code: int }

    type pickle = { pid: pid, pickle: Word8Vector.vector }

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

  (* read just the guid *)
    val readGUid : BinIO.instream -> string

  (* read binfile contents from an IO stream *)
    val read : {
            version : version_info,                     (* expected binfile version *)
	    stream: BinIO.instream
	  } -> { contents: bfContents, stats: stats }

  end = struct

    structure Pid = PersStamps
    structure W8V = Word8Vector
    structure SS = Substring

    exception FormatError = CodeObj.FormatError

    type pid = Pid.persstamp

    type csegments = {
	code : CodeObj.t,
	data : Word8Vector.vector       (* literals *)
      }

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

  end (* structure OldBinfile *)
