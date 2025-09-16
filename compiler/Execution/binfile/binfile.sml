(* binfile.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
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
    structure W8VS = Word8VectorSlice

    exception FormatError = CodeObj.FormatError

    type pid = Pid.persstamp

    type executable = CodeObj.executable

    type stats = { env: int, data: int, code: int }

    type pickle = { pid: pid, pickle: W8V.vector }

    type version_info = {
        bfVersion : word,       (* will be 0w0 for old versions *)
        arch : string,
        smlnjVersion : string
      }

    datatype bfContents = datatype OldBinfile.bfContents
(*
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
*)

    fun mkVersion {arch, smlnjVersion} = {
            bfVersion = BinfileIO.Hdr.version,
            arch = arch,
            smlnjVersion = smlnjVersion
          }

    fun unBF (BF x) = x

    val version = #version o unBF
    val senvPickleOf = #senv o unBF
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

    (* read the import tree. *)
    fun getImportSection (sect, _) = let
          (* first we get the number of leaves (imports) in all of the trees *)
          val nLeaves = BFIO.In.packedInt sect
          (* read a tree from the section *)
          fun getTree () = (case BFIO.In.packedInt sect
                 of 0 => (ImportTree.ITNODE [], 1)
                  | cnt => let
                      fun readImportList (0, kids, n) = (List.rev kids, n)
                        | readImportList (cnt, kids, n) = let
                            val selector = BFIO.In.packedInt sect
                            val (tree, n') = getTree ()
                            in
                              readImportList (cnt-1, (selector, tree)::kids, n+n')
                            end
                      val (l, n) = readImportList (cnt, [], 0)
                      in
                        (ImportTree.ITNODE l, n)
                      end
                (* end case *))
          fun getImports n = if (n <= 0)
                then []
                else let
                  val pid = BFIO.In.pid sect
                  val (tree, n') = getTree ()
                  val rest = getImports (n - n')
                  in
                    (pid, tree) :: rest
                  end
          in
            getImports nLeaves
          end

    fun getPidSection (sect, sz) = BFIO.In.pid sect

    fun getPidsSection (sect, sz) = let
          val nPids = sz div bytesPerPid
          in
            List.tabulate (nPids, fn _ => BFIO.In.pid sect)
          end

    fun getGuidSection (sect, sz) = BFIO.In.string (sect, sz)

    fun readGUid s = let
          val bf = BFIO.In.openStream s
          in
            case BFIO.In.section (bf, BFIO.SectId.guid, getGuidSection)
             of SOME guid => guid
              | NONE => BFIO.error "missing 'GUID' section"
            (* end case *)
          end

    fun getLiteralsSection (sect, sz) = BFIO.In.bytes (sect, sz)

    fun getCodeSection (sect, sz) = let
          val entryPt = BFIO.In.packedInt sect
          in
            BFIO.In.codeObject (sect, entryPt)
          end

    fun getStaticEnvSection (sect, sz) = let
          val staticPid = BFIO.In.pid sect
          val pklSz = if (sz > bytesPerPid)
                then BFIO.In.packedInt sect
                else 0
          val pkl = BFIO.In.bytes (sect, pklSz)
          in
            (* read and discard the padding *)
            ignore (BFIO.In.bytes (sect, sz - pklSz - bytesPerPid));
            { pid = staticPid, pickle = pkl }
          end

(* FIXME: instead of passing the `version_info`, we should just pass the expected
 * SML/NJ version and architecture.
 *)
    fun read { version : version_info, stream } = let
(* DEBUG *)val () = Control_Print.say "**** read new-format binfile\n"
          val bf = BFIO.In.openStream stream
          val hdr = BFIO.In.header bf
          (* check that we have a Binfile and not an archive *)
          val _ = if (BFIO.Hdr.isArchive hdr)
                then BFIO.error "unexpected stable archive"
                else ()
          (* helper function to get a section *)
          fun readSection (id, inFn) = (case BFIO.In.section(bf, id, inFn)
                 of SOME contents => contents
                  | NONE => BFIO.error(concat[
                        "missing '", BFIO.SectId.toString id, "' section"
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
                      val {version_id, suffix} = BFIO.Hdr.smlnjVersion hdr
                      val vn = String.concatWithMap "." Int.toString version_id
                      val v = if (suffix = "") then vn else concat[vn, "-", suffix]
                      in
                        (* the SML/NJ version string in the old binfile format is 16 bytes *)
                        if (size v > 16)
                          then substring (v, 0, 16)
                        else if (size v < 16)
                          then StringCvt.padRight #" " 16 v
                          else v
                      end
                in {
                    bfVersion = BFIO.Hdr.bfVersion hdr,
                    arch = arch,
                    smlnjVersion = smlnjVers
                } end
(* TODO: check version *)
          val imports = readSection (BFIO.SectId.import, getImportSection)
          (* get the optional export Pid *)
          val exportPid = BFIO.In.section (bf, BFIO.SectId.export, getPidSection)
          val cmData = readSection (BFIO.SectId.pids, getPidsSection)
          val guid = readSection (BFIO.SectId.guid, getGuidSection)
          val literals = readSection (BFIO.SectId.literals, getLiteralsSection)
          val code = readSection (BFIO.SectId.code, getCodeSection)
          val senv = readSection (BFIO.SectId.staticEnv, getStaticEnvSection)
	  in {
	    contents = create {
                version = version',
		imports = imports,
		exportPid = exportPid,
		cmData = cmData,
		senv = senv,
		guid = guid,
		csegments = {lits = literals, code = code}
	      },
	    stats = {
		env = W8V.length(#pickle senv),
                code = CodeObj.size code,
		data = W8V.length literals
	      }
	  } end

    (* determine if a binfile has the old format. *)
    fun isNewFormat (inS : BinIO.StreamIO.instream) = let
          val (bv, _) = BinIO.StreamIO.inputN (inS, 12)
          in
            (W8V.length bv = 12) andalso let
              val data = W8VS.full bv
              val kind = Byte.unpackStringVec(W8VS.subslice(data, 0, SOME 8))
              val vers = PackWord32Little.subVec(bv, 2)
              in
                (kind = "BinFile " orelse kind = "StabArch")
                  andalso (vers >= 0wx20250801)
              end
          end

    val readGUid = fn inS => if isNewFormat(BinIO.getInstream inS)
          then readGUid inS
          else OldBinfile.readGUid inS

    val read = fn (arg as {stream, version}) => if isNewFormat(BinIO.getInstream stream)
          then read arg
          else OldBinfile.read arg

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
                BFIO.Out.string(sect, padString(arch, 8));
                BFIO.Out.string(sect, padString(opsys, 8)))
          in
            BFIO.Out.section (bf, BFIO.SectId.info, infoSize, outFn)
          end

    (** imports ('IMPT') section **)
    local
      structure IT = ImportTree
      val sizePackedInt = LEB128.sizeOfInt
    in
    (* compute the size of the tree and the number of leaves (imports) *)
    fun importTreeSize trees = let
          fun itreeSz (IT.ITNODE[], (nl, sz)) =
                (nl + 1, sz + 1)
            | itreeSz (IT.ITNODE l, (nl, sz)) =
                List.foldl specSz (nl, sz + sizePackedInt(length l)) l
          and specSz ((selector, tree), (nl, sz)) =
                itreeSz (tree, (nl, sz + sizePackedInt selector))
          and importSz ((pid, tree), (nl, sz)) = itreeSz (tree, (nl, sz + bytesPerPid))
          val (nl, sz) = List.foldl importSz (0, 0) trees
          in
            {nLeaves = nl, sz = sizePackedInt nl + sz}
          end
    (* add an import tree section to the binfile *)
    fun addImportTreeSection (bf, l) = let
          val {nLeaves, sz} = importTreeSize l
          fun outFn sect = let
                (* add a PID+tree to the buf *)
                fun emitPidAndTree (pid, tree) = let
                      fun emitImportSpec (selector, tree) = (
                            BFIO.Out.packedInt (sect, selector);
                            emitImportTree tree)
                      and emitImportTree (IT.ITNODE []) = BFIO.Out.packedInt (sect, 0)
                        | emitImportTree (IT.ITNODE l) = (
                            BFIO.Out.packedInt (sect, List.length l);
                            List.app emitImportSpec l)
                      in
                        BFIO.Out.pid (sect, pid);
                        emitImportTree tree
                      end
                in
                  BFIO.Out.packedInt (sect, nLeaves);
                  List.app emitPidAndTree l
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
                  | SOME pid => (bytesPerPid, fn sect => BFIO.Out.pid(sect, pid))
                (* end case *))
          in
            BFIO.Out.section (bf, BFIO.SectId.export, sz, outFn)
          end

    (** pids ('PIDS') section *)
    fun pidsSize pids = bytesPerPid * List.length pids
    fun addPidsSection (bf, pids) = let
          val sz = pidsSize pids
          fun outFn sect = List.app (fn pid => BFIO.Out.pid(sect, pid)) pids
          in
            BFIO.Out.section (bf, BFIO.SectId.pids, sz, outFn)
          end

    (** Guid ('GUID') section *)
    fun guidSz guid = String.size guid
    fun addGuidSection (bf, guid) = let
          val sz = guidSz guid
          fun outFn sect = BFIO.Out.string(sect, guid)
          in
            BFIO.Out.section (bf, BFIO.SectId.guid, sz, outFn)
          end

    (** literal ('LITS') section **)
    fun literalSize lits = W8V.length lits
    fun addLiteralSection (bf, lits) = let
          val sz = literalSize lits
          fun outFn sect = BFIO.Out.bytes(sect, lits)
          in
            BFIO.Out.section (bf, BFIO.SectId.literals, sz, outFn)
          end

    (** code ('CODE') section **)
    fun codeSize code = CodeObj.size code
    fun addCodeSection (bf, code) = let
          val sz = codeSize code
          fun outFn sect = (
                BFIO.Out.int32(sect, CodeObj.entrypoint code);
                BFIO.Out.codeObject(sect, code))
          in
            BFIO.Out.section (bf, BFIO.SectId.code, sz, outFn)
          end

    (** static environment ('SENV') section **)
    fun trueStaticEnvSize (_, _, true) = bytesPerPid
      | trueStaticEnvSize (_, pkl, false) = let
          val n = W8V.length pkl
          in
            bytesPerPid + LEB128.sizeOfInt n + n
          end
    fun staticEnvSize arg = BFIO.padSize(trueStaticEnvSize arg)
    fun addStaticEnvSection (bf, pid, pkl, nopickle) = let
          val trueSz = trueStaticEnvSize (pid, pkl, nopickle)
          val sz = BFIO.padSize trueSz
          fun outFn sect = (
                BFIO.Out.pid (sect, pid);
                if nopickle
                  then ()
                  else (
                    BFIO.Out.packedInt (sect, LEB128.sizeOfInt(W8V.length pkl));
                    BFIO.Out.bytes (sect, pkl);
                    BFIO.Out.pad (sect, sz - trueSz)))
          in
            BFIO.Out.section (bf, BFIO.SectId.staticEnv, sz, outFn)
          end

    fun size { contents, nopickle } = let
	  val BF{ version, imports, exportPid, cmData, senv, csegments, guid, ... } =
                contents
          val { bfVersion, arch, smlnjVersion } = version
	  val { pickle = senvPkl, pid = staticPid } = senv
          val { code, lits } = csegments
          in
            BFIO.Hdr.sizeOfHdr 8 (* there are eight sections *)
              + infoSize
              + BFIO.padSize (#sz (importTreeSize imports))
              + BFIO.padSize (exportSize exportPid)
              + BFIO.padSize (pidsSize cmData)
              + BFIO.padSize (String.size guid)
              + BFIO.padSize (literalSize lits)
              + BFIO.padSize (codeSize code)
              + BFIO.padSize (staticEnvSize (staticPid, senvPkl, nopickle))
          end

    fun write { stream, contents, nopickle } = let
	  val BF{ version, imports, exportPid, cmData, senv, csegments, guid, ... } =
                contents
          val { bfVersion, arch, smlnjVersion } = version
	  val { pickle = senvPkl, pid = staticPid } = senv
          val { code, lits } = csegments
          val smlnjVersion = (case SMLNJVersion.fromString smlnjVersion
                 of SOME vers => vers
                  | NONE => BFIO.error(concat[
                        "bogus SML/NJ version \"", smlnjVersion, "\""
                      ])
                (* end case *))
          val bf = BFIO.Out.openStream (stream, false, smlnjVersion)
          val stats = {
                    env = if nopickle then 0 else W8V.length senvPkl,
                    data = W8V.length lits,
                    code = CodeObj.size code
                  }
          in
(* FIXME: eventually, we should include useful OS info in the info section *)
            addInfoSection (bf, arch, "");
            addImportTreeSection (bf, imports);
            addExportSection (bf, exportPid);
            addPidsSection (bf, cmData);
            addGuidSection (bf, guid);
            addLiteralSection (bf, lits);
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

  end (* structure Binfile *)
