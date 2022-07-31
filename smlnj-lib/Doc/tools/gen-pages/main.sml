(* main.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Main function for *gen-pages* tool.  This program has the following
 * usage:
 *
 *	gen-pages [options]
 *
 *	Options:
 *
 *	    -h, --help			print help message and exit
 *	    -v, --verbose		run in verbose mode
 *	    --release-date=<date>
 *	    --version=<version>
 *	    --base-url=<url>
 *	    --basis-lib-url=<url>
 *	    --index=<file>
 *
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure FT = FileTree
    structure P = OS.Path

(* TODO: support relative path for @IMAGES_URL@.  The actual value depends on
 * the path of the file we are generating, since we may need an extra ".." prefix.
 *)

  (* create a copy-file function with the specified substitutions *)
    fun copy {meta : FT.meta, title, file, base} = let
	  val d = Date.fromTimeUniv(OS.FileSys.modTime file)
	  val date = Date.fmt "%Y-%m-%d" d
	  val time = Date.fmt "%H:%M:%S UTC" d
	  val imagesURL = Option.getOpt(!Options.imagesURL, base ^ "images")
	  val substs = [
		  ("STYLED-TITLE", Util.style title),
		  ("DATE", !Options.releaseDate),
		  ("VERSION", !Options.version),
		  ("FILEDATE", date),
		  ("FILETIME", time),
		  ("BASE", base),
		  ("IMAGES_URL", imagesURL)
		]
	  fun metaStr (k, v) = concat[
		  "\n  <meta name=\"", k, "\" content=\"", v, "\">"
		]
	(* the contents of the <title> element in the <head> *)
	  val substs = (case #title meta
		 of SOME s => ("TITLE", s) :: substs
		  | NONE => ("TITLE", Util.clean title) :: substs
		(* end case *))
	  val substs = (case #author meta
		 of SOME s => ("AUTHOR", metaStr("author", s)) :: substs
		  | NONE => ("AUTHOR", "") :: substs
		(* end case *))
	  val substs = (case #kws meta
		 of [] => ("KEYWORDS", "") :: substs
		  | l => ("KEYWORDS", metaStr("keywords", String.concatWith "," l))
		      :: substs
		(* end case *))
	  in
	    CopyFile.copy substs
	  end

    fun gen genTOC (FT.FILE{dir, stem, title, meta, ...}) = let
	  val srcFile = P.concat(dir, stem ^ ".adoc")
	  val htmlFile = P.concat(dir, stem ^ ".html")
	  val copy = copy {
		  meta = meta,
		  title = title,
		  file = srcFile,
		  base = if dir = "" then "" else "../"
		}
	  val outS = TextIO.openOut htmlFile
	  in
	    if !Options.verbose
	      then print(concat["generating ", htmlFile, "\n"])
	      else ();
	    copy (P.concat(Config.fragDir, "header.in"), outS);
	    genTOC outS;
	    copy (P.concat(Config.fragDir, "shim.in"), outS);
	    RunAsciidoctor.run (srcFile, outS);
	    copy (P.concat(Config.fragDir, "footer.in"), outS);
	    TextIO.closeOut outS
	  end

  (* generate the root page *)
    fun appRoot ft = gen (GenTOC.root ft) ft

  (* generate a library page *)
    fun appLib (ft, lib) = gen (GenTOC.lib (ft, lib)) lib

  (* generate a TOC file for a manual page *)
    fun appPage (ft, lib, page) = gen (GenTOC.page (ft, lib, page)) page

    fun loadIndex indexFile = FT.fromJSON (JSONParser.parseFile indexFile)

  (* for every documentation page, generate a table of contents file *)
    val walkTree = FT.app {root = appRoot, lib = appLib, page = appPage}

    fun main (cmd, opts) = (
	  Options.process opts;
	  walkTree (loadIndex (!Options.indexFile));
	  if not(!Options.verbose) then print " done\n" else print "done\n";
	  OS.Process.success)
	    handle ex => (
	      TextIO.output(TextIO.stdErr, concat[
		  "uncaught exception ", General.exnName ex,
		  " [", General.exnMessage ex, "]\n"
		]);
	      List.app
		(fn s => TextIO.output(TextIO.stdErr, concat ["  raised at ", s, "\n"]))
		  (SMLofNJ.exnHistory ex);
              OS.Process.failure)

  end
