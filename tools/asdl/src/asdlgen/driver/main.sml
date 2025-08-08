(* main.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The driver for asdlgen.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

  (* register the supported views *)
    val () = List.app Options.registerGen [
            { names = ["c++"],
              opts = GenCpp.options,
              desc = "Generate C++ implementation",
              gen = GenCpp.gen,
              genBuild = fn () => () (* FIXME: makefile support? *)
            },
            { names = ["sml"],
              opts = GenSML.options,
              desc = "Generate SML implementation",
              gen = GenSML.gen,
              genBuild = GenSML.genBuildFiles
            }
(*
            { names = ["typ"],          ??
            }
*)
          ]

  (* check a file; return true if there is an error *)
    fun checkFile file = (case FrontEnd.doFile file
           of SOME _ => false
            | NONE => true
          (* end case *))

    fun doFile (gen : Options.generator) fname = let
          fun getStem fname = (case OS.Path.splitBaseExt fname
                 of {base, ext=SOME "asdl"} => base
                  | _ => fname
                (* end case *))
          in
            case FrontEnd.doFile fname
             of SOME file => let
                  val (dir, stem) = (
                        case (Options.outputDir(), OS.Path.splitDirFile fname)
                         of (NONE, {dir, file}) => (dir, getStem file)
                          | (SOME dir, {file, ...}) => (dir, getStem file)
                        (* end case *))
                  in
                    gen {src = fname, dir = dir, stem = stem, file = file};
                    false
                  end
              | NONE => true
            (* end case *)
          end

    fun err msg = TextIO.output(TextIO.stdErr, concat msg)

    fun fail (cmdName, msg) = (
          err [cmdName, ": ", msg, "\n", Options.usage()];
          OS.Process.failure)

    fun handleExn exn = (
          err [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ];
          List.app (fn s => err ["  raised at ", s, "\n"]) (SMLofNJ.exnHistory exn);
          OS.Process.failure)

    fun main (cmdName, args) = let
          val {command, files} = Options.parseCmdLine args
          in
            case command
             of Options.HELP => (
                  TextIO.output(TextIO.stdOut, Options.usage());
                  OS.Process.success)
              | Options.VERSION => (
                  TextIO.output(TextIO.stdOut, Config.version ^ "\n");
                  OS.Process.success)
              | Options.CHECK => if List.exists checkFile files
                  then OS.Process.failure
                  else OS.Process.success
              | Options.GENERATE{gen, genBuild} =>
                  if List.exists (doFile gen) files
                    then OS.Process.failure
                    else (
                      genBuild();
                      OS.Process.success)
            (* end case *)
          end
            handle Options.Usage msg => fail (cmdName, msg)
                | ex => handleExn ex

  end
