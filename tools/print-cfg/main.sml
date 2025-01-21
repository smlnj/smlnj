(* main.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * To build:
 *	ml-build print-cfg.cm Main.main print-cfg
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    fun doFile norm file = if OS.FileSys.access(file, [OS.FileSys.A_READ])
	  then let
            val stem = (case OS.Path.splitBaseExt file
                   of {base, ext=SOME "pkl"} => base
                    | _ => file
                  (* end case *))
            val outFile = OS.Path.joinBaseExt{base=stem, ext=SOME"cfg"}
	    val cu = ASDLFilePickle.fromFile CFGFilePickle.read_comp_unit file
		  handle exn => (
		    TextIO.output(TextIO.stdErr, concat[
			file, ": uncaught exception ", exnMessage exn, "\n"
		      ]);
		    OS.Process.exit OS.Process.failure)
            val cu = if norm then Normalize.normalize cu else cu
            val outS = TextIO.openOut outFile
	    in
	      Print.compUnit (outS, cu);
              TextIO.closeOut outS
	    end
	  else (
	    TextIO.output(TextIO.stdErr, concat[
		"CFG pickle file \"", file, "\" not found\n"
	      ]);
	    OS.Process.exit OS.Process.failure)

    fun main (_, "-n" :: (files as _::_)) = (
          List.app (doFile true) files;
	  OS.Process.success)
      | main (_, files as _::_) = (
          List.app (doFile false) files;
	  OS.Process.success)
      | main _ = (
	  TextIO.output(TextIO.stdErr, "usage: print-cfg [ -n ] <file> ...\n");
	  OS.Process.failure)

  end
