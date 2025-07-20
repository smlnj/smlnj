(* gen-cpp.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Generate the C++ view of the ASDL modules.
 *)

structure GenCpp : sig

    val options : unit GetOpt.opt_descr list

    val gen : {src : string, dir : string, stem : string, file : AST.file} -> unit

  end = struct

    structure V = CppView
    structure CL = Cpp
    structure Opt = Options

    val arenaAllocOpt = ref false
    val baseIncludeOpt = ref "asdl/asdl.hpp"

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

  (* include directives and header definitions to include in the .hpp and .cpp files *)
    fun hppIncls true =  [
            "#define ASDL_ENABLE_ARENA_ALLOC\n",
            "#include \"@BASE_INCLUDE@\"\n"
          ]
      | hppIncls false = [
            "#include \"@BASE_INCLUDE@\"\n"
          ]
    fun cppIncls true = [
            "#include \"@HPP_FILENAME@\"\n",
            "\n",
            "asdl::alloc::__details::arena *asdl::alloc::__details::arena::allocArena = nullptr;\n"
          ]
      | cppIncls false = [
            "#include \"@HPP_FILENAME@\"\n"
          ]

    fun expand (src, file) = let
          val hppFile = OS.Path.joinBaseExt{
                  base = OS.Path.base file,
                  ext = SOME "hpp"
                }
         val expand = StringSubst.expand [
                  ("FILENAME", file),
                  ("BASE_INCLUDE", !baseIncludeOpt),
                  ("HPP_FILENAME", hppFile),
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
          fun pr dcl = PrintCpp.output (ppStrm, dcl)
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
          fun cppFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "cpp"}
          fun hppFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "hpp"}
        (* we only generate code for the non-primitive modules *)
          val modules = List.filter (fn (AST.Module{isPrim, ...}) => not isPrim) modules
          in
          (* generate the header file *)
            if not(#types suppress)
              then genFile GenTypes.gen (
                src, hppFilename basePath,
                V.File.getInterfaceCode(),
                hppIncls(#arenaAlloc flags), modules, flags)
              else ();
          (* generate the pickler implementation *)
            genFile GenPickle.gen
              (src, cppFilename basePath,
              V.File.getImplementationCode(),
              cppIncls(#arenaAlloc flags), modules, flags)
          end

  (* generate C++ code for the given list of modules using the "Cpp" view *)
    fun gen {src, dir, stem, file=AST.File{modules, shared}} = let
          val suppress = V.File.getSuppress()
          in
            if (#types suppress andalso #pickler suppress andalso #unpickler suppress)
              then () (* all output is suppressed *)
              else gen' (src, dir, stem, modules, suppress)
          end

  end
