(* gen-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML code generation for ASDL.
 *)

structure GenSML : sig

    val options : unit GetOpt.opt_descr list

    val gen : {src : string, dir : string, stem : string, modules : AST.module list} -> unit

  (* generate the optional CM and/or MLB file(s) for the list of inputs *)
    val genBuildFiles : unit -> unit

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure S = SML
    structure Opt = Options
    structure PP = TextIOPP

  (* Generate the memory pickling code for the SML view *)
    structure GenMemoryPickle = GenPickleFn (
	val getPklModName = ModV.getPickleName
      (* the names of the functions for reading/writing bytes *)
	val getByte = "ASDLMemoryPickle.input1"
	val putByte = "ASDLMemoryPickle.output1")

  (* Generate the file pickling code for the SML view *)
    structure GenFilePickle = GenPickleFn (
	val getPklModName = ModV.getIOName
      (* the names of the functions for reading/writing bytes *)
	val getByte = "ASDLFilePickle.input1"
	val putByte = "ASDLFilePickle.output1")

    val baseStructureOpt = ref "ASDL"
    val cmFileOpt = ref (NONE : string option)
    val mlbFileOpt = ref (NONE : string option)

    val options = [
	    { short = "", long = ["base-structure"],
	      desc = GetOpt.ReqArg(fn s => baseStructureOpt := s, "<name>"),
	      help = "specify the structure that defines the ASDL primitive types"
	    },
	    { short = "", long = ["cm"],
	      desc = GetOpt.ReqArg(fn s => cmFileOpt := SOME s, "<name>"),
	      help = "generate a CM file"
	    },
	    { short = "", long = ["mlb"],
	      desc = GetOpt.ReqArg(fn s => mlbFileOpt := SOME s, "<name>"),
	      help = "generate an MLB file"
	    }
	  ]

  (* tests for generating the various possible components *)
    fun typesEnabled () = Opt.isEnabled Opt.TYPES
    fun memEnabled () = Opt.isEnabled Opt.MEMORY
    fun fileEnabled () = Opt.isEnabled Opt.FILE
    fun sexpEnabled () = Opt.isEnabled Opt.SEXP

  (* remove primitive modules from a list of modules *)
    val filterPrim = List.filter (fn (AST.Module{isPrim, ...}) => not isPrim)

  (* generate the file header as a verbatim top_decl *)
    fun genHeader (src, file) = let
	  val expand = StringSubst.expand [
		  ("FILENAME", file),
		  ("SRCFILE", src)
		]
	  in
	    S.VERBtop(List.map expand (V.File.getHeader()))
	  end

  (* output SML declarations to a file *)
    fun output (src, outFile, dcls) = let
	  val outS = TextIO.openOut outFile
(* FIXME: output width is a command-line option! *)
	  val ppStrm = TextIOPP.openOut {dst = outS, wid = Opt.lineWidth()}
	  in
	    List.app (PrintSML.output ppStrm) (genHeader (src, outFile) :: dcls);
	    TextIOPP.closeStream ppStrm;
	    TextIO.closeOut outS
	  end

  (* generate a file using the given code generator *)
    fun genFile targets codeGen (src, outFile, modules) =
	  if List.exists Opt.isEnabled targets
	    then output (src, outFile, List.map codeGen modules)
	  else if Opt.noOutput()
	    then print(outFile ^ "\n")
	    else ()

  (* generate the type-declaration file *)
    fun genTypes (src, outFile, modules) = (
	  case filterPrim modules
	   of [] => ()
	    | modules => if Opt.isEnabled Opt.TYPES
		  then output (src, outFile, List.map GenTypes.gen modules)
		else if Opt.noOutput()
		  then print(outFile ^ "\n")
		  else ()
	  (* end case *))

  (* generate the generic pickler signature *)
    val genPicklerSig =
	  genFile [Opt.MEMORY, Opt.FILE, Opt.SEXP] GenPickleSig.gen

  (* generate the pickler files *)
    val genMemoryStr = genFile [Opt.MEMORY] GenMemoryPickle.gen

  (* generate the pickle-io files *)
    val genFileStr = genFile [Opt.FILE] GenFilePickle.gen

  (* generate the S-Expression pickler files *)
    val genSExpStr = genFile [Opt.SEXP] GenSExpPickle.gen

  (* references to collect together the generated files and modules so that
   * we can generate CM and/or MLB files
   *)
    val genFiles : string list ref = ref []
    val genSigs : string list ref = ref []
    val genStructs : string list ref = ref []

  (* add the generated signatures and structures to the appropriate lists *)
    fun addModules modules = let
	  fun getSig (AST.Module{isPrim, id, ...}, sigs) = if isPrim
		then sigs
		else Util.sigName(ModV.getPickleSigName id, NONE) :: sigs
	  fun getStructs (AST.Module{id, ...}, structs) = let
		val structs = if sexpEnabled()
		      then ModV.getSExpName id :: structs
		      else structs
		in
		  ModV.getName id ::
		  ModV.getPickleName id ::
		  ModV.getIOName id ::
		    structs
		end
	  in
	    genSigs := List.foldr getSig (! genSigs) modules;
	    genStructs := List.foldr getStructs (! genStructs) modules
	  end

  (* keep track of the generated files *)
    fun addFile filename = (
	  genFiles := filename :: !genFiles;
	  filename)

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun gen {src, dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  fun smlFilename suffix =
		OS.Path.joinBaseExt{base=basePath ^ suffix, ext=SOME "sml"}
	  fun sigFilename suffix =
		OS.Path.joinBaseExt{base=basePath ^ suffix, ext=SOME "sig"}
	(* check for sharing *)
	  val hasSharing = Sharing.analysis modules
	(* record the inputs *)
	  val _ = addModules modules
	  in
	    genTypes (src, addFile(smlFilename ""), modules);
	    genPicklerSig (src, addFile(sigFilename "-pickle"), modules);
	    genMemoryStr (src, addFile(smlFilename "-memory-pickle"), modules);
	    genFileStr (src, addFile(smlFilename "-file-pickle"), modules);
	    genSExpStr (src, addFile(smlFilename "-sexp-pickle"), filterPrim modules)
	  end

  (**** CM/MLB file support ****)

    val stringSort = ListMergeSort.sort String.>

  (* Generate a CM file for the pickling code *)
    fun genCMFile (outS, files, sigs, structs) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl ss = pr(String.concat ss)
	  fun prSig id = prl["  signature ", id, "\n"]
	  fun prStruct id = prl["  structure ", id, "\n"]
	  fun prFile file = prl["  ", file, "\n"]
	  in
	    pr "Library\n\n";
	    List.app prSig sigs;
	    pr "\n";
	    List.app prStruct structs;
	    pr "\
		\\n\
		\is\n\
		\\n\
                \  $/basis.cm\n\
                \  $/asdl-lib.cm\n\
		\\n\
                \";
	    List.app prFile files
	  end

  (* Generate an MLB file for the pickling code *)
    fun genMLBFile (outS, files, sigs, structs) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl ss = pr(String.concat ss)
	  fun prSig id = prl["  signature ", id, "\n"]
	  fun prStruct id = prl["  structure ", id, "\n"]
	  fun prFile file = prl["  ", file, "\n"]
	  in
(* FIXME: the path for the asdl-lib.mlb will depend on where MLton puts it *)
	    pr "\
		\local\n\
		\\n\
                \  $(SML_LIB)/basis/basis.mlb\n\
                \  $(ASDL_LIB)/asdl-lib.cm\n\
		\\n\
                \";
	    List.app prFile files;
	    pr "\
		\\n\
		\in\n\
		\\n\
                \";
	    List.app prSig sigs;
	    pr "\n";
	    List.app prStruct structs;
	    pr "\
		\\n\
		\end\n\
                \"
	  end

    fun genBuildFiles () = let
	  val files = stringSort (!genFiles)
	  val sigs = stringSort (!genSigs)
	  val structs = stringSort (!genStructs)
	  fun genFile gen optFile = (case !optFile
		 of SOME file => if Opt.noOutput()
		      then print(file ^ "\n")
		      else let
			val outS = TextIO.openOut file
			in
			  gen (outS, files, sigs, structs);
			  TextIO.closeOut outS
			end
		  | NONE => ()
		(* end case *))
	  in
	    genFile genCMFile cmFileOpt;
	    genFile genMLBFile mlbFileOpt
	  end

  end
