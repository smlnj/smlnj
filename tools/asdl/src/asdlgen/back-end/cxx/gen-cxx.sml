(* gen-cxx.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the C++ view of the ASDL modules.
 *)

structure GenCxx : sig

    val options : unit GetOpt.opt_descr list

    val gen : {src : string, dir : string, stem : string, modules : AST.module list} -> unit

  end = struct

    structure V = CxxView
    structure CL = Cxx
    structure Opt = Options

    val arenaAllocOpt = ref false
    val baseIncludeOpt = ref "asdl/asdl.hxx"

    val options = [
	    { short = "", long = ["arena-alloc"],
	      desc = GetOpt.NoArg(fn () => arenaAllocOpt := true),
	      help = "use ASDL's arena allocator"
	    },
	    { short = "", long = ["base-include"],
	      desc = GetOpt.ReqArg(fn s => baseIncludeOpt := s, "<file>"),
	      help = "specify include file for ASDL primitive types"
	    }
	  ]

  (* include directives and header definitions to include in the .hxx and .cxx files *)
    fun hxxIncls true =  [
	    "#define ASDL_ENABLE_ARENA_ALLOC\n",
	    "#include \"@BASE_INCLUDE@\"\n"
	  ]
      | hxxIncls false = [
	    "#include \"@BASE_INCLUDE@\"\n"
	  ]
    fun cxxIncls true = [
	    "#include \"@HXX_FILENAME@\"\n",
	    "\n",
	    "asdl::alloc::__details::arena *asdl::alloc::__details::arena::allocArena = nullptr;\n"
	  ]
      | cxxIncls false = [
	    "#include \"@HXX_FILENAME@\"\n"
	  ]

    fun expand (src, file) = let
	  val hxxFile = OS.Path.joinBaseExt{
		  base = OS.Path.base file,
		  ext = SOME "hxx"
		}
	 val expand = StringSubst.expand [
		  ("FILENAME", file),
		  ("BASE_INCLUDE", !baseIncludeOpt),
		  ("HXX_FILENAME", hxxFile),
		  ("SRCFILE", src)
		]
	  in
	    fn [] => []
	     | strs => [CL.D_Verbatim(List.map expand strs)]
	  end

  (* output C++ declarations to a file *)
    fun output (src, outFile, {prologue, epilogue}, incls, dcls) = let
	  val outS = TextIO.openOut outFile
(* FIXME: output width is a command-line option! *)
	  val ppStrm = TextIOPP.openOut {dst = outS, wid = Options.lineWidth()}
	  val expand = expand (src, outFile)
	  fun pr dcl = PrintCxx.output (ppStrm, dcl)
	  in
	    List.app pr (expand (V.File.getHeader()));
	    List.app pr (expand incls);
	    List.app pr (expand prologue);
	    List.app pr dcls;
	    List.app pr (expand epilogue);
	    TextIOPP.closeStream ppStrm;
	    TextIO.closeOut outS
	  end

  (* generate a file using the given code generator *)
    fun genFile codeGen (src, outFile, extra, incls, modules, flags) =
	  if Options.noOutput()
	    then print(outFile ^ "\n")
	    else let
	      val codeGen = codeGen flags
	      in
		output (src, outFile, extra, incls, List.map codeGen modules)
	      end

    fun gen' (src, dir, stem, modules, suppress) = let
	  val flags : Util.flags = {
		  suppress = suppress,
		  arenaAlloc = !arenaAllocOpt orelse V.File.getArenaAlloc()
		}
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  fun cxxFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "cxx"}
	  fun hxxFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "hxx"}
	(* we only generate code for the non-primitive modules *)
	  val modules = List.filter (fn (AST.Module{isPrim, ...}) => not isPrim) modules
	  in
	  (* generate the header file *)
	    if not(#types suppress)
	      then genFile GenTypes.gen (
		src, hxxFilename basePath,
		V.File.getInterfaceCode(),
		hxxIncls(#arenaAlloc flags), modules, flags)
	      else ();
	  (* generate the pickler implementation *)
	    genFile GenPickle.gen
	      (src, cxxFilename basePath,
	      V.File.getImplementationCode(),
	      cxxIncls(#arenaAlloc flags), modules, flags)
	  end

  (* generate C++ code for the given list of modules using the "Cxx" view *)
    fun gen {src, dir, stem, modules} = let
	  val suppress = V.File.getSuppress()
	  in
	    if (#types suppress andalso #pickler suppress andalso #unpickler suppress)
	      then () (* all output is suppressed *)
	      else gen' (src, dir, stem, modules, suppress)
	  end

  end
