(* make-index.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This program is used to generate extract index information from
 * the document sources and then write it out as a JSON file.
 *
 * Usage:
 *
 *	make-index index.json
 *)

structure MakeIndex : sig

    val main : string * string list -> OS.Process.status

  end = struct

    fun usage sts = (
	  TextIO.print "usage: make-index <outfile>\n";
	  OS.Process.exit sts)

    fun main (cmd, args) = (case args
	   of "-h"::_ => usage OS.Process.success
	    | [outFile] => let
		val tree = FileTree.toJSON(ExtractIndex.extract ".")
		val outS = TextIO.openOut outFile
		in
		  JSONPrinter.print' {strm = outS, pretty = true} tree;
		  OS.Process.success
		end
	    | _ => usage OS.Process.failure
	  (* end case *))
            handle exn => (
              TextIO.output(TextIO.stdErr, concat[
                  "make-index: uncaught exception (", exnMessage exn, ")"
                ]);
              OS.Process.failure)

  end
