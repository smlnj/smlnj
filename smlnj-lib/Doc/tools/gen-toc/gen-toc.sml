(* gen-toc.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: the TOC generation has been integrated into the gen-pages tool.
 * This program is preserved for historical interest only.
 *
 * This program is used to generate the HTML fragment for a sidebar
 * table of contents.
 *
 * Usage:
 *
 *	gen-toc index.json
 *
 * This command will produce a file file.toc for each source file in
 * the index (including the root).   These files are HTML fragments
 * for a navigation sidebar specialized to the corresponding documentation
 * file.
 *
 * NOTE: this program must be run from the root of the documentation source!
 *
 * TODO: eventually, we should include the individual definitions from
 * the file in the sidebar.
 *)

structure GenTOC : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure FT = FileTree
    structure P = OS.Path

    fun usage sts = (
	  TextIO.print "usage: gen-toc index.json\n";
	  OS.Process.exit sts)

    fun error msg = (
	  TextIO.print(concat["gen-toc: ", msg, "\n"]);
	  OS.Process.exit OS.Process.failure)

    fun loadIndex indexFile = FT.fromJSON (JSONParser.parseFile indexFile)

    fun withOutS file f = let
	  val outS = TextIO.openOut file
	  in
	    (f outS) handle ex => (TextIO.closeOut outS; raise ex);
	    TextIO.closeOut outS
	  end

    fun pr (outS, s) = TextIO.output(outS, s)
    fun prl (outS, s) = TextIO.output(outS, concat s)

    fun doTOC file f = let
	  fun f' outS = (
		prl (outS, [
		    "<div id=\"toc\">\n"
		  ]);
		f outS;
		prl (outS, [
		    "</div> <!-- toc -->\n"
		  ]))
	  in
	    withOutS file f'
	  end

  (* handle some simple style notations found in the Asciidoctor title text.
   * We handle code "`", bold "*", and italic "_" markup.  We assume that
   * these are _not_ nested.
   *)
    fun styleText s = let
	  fun bold (#"*" :: #"*" :: r, acc) = (r, "</b>" :: acc)
	    | bold (#"*" :: r, acc) = (r, "</b>" :: acc)
	    | bold (c :: r, acc) = bold (r, str c :: acc)
	    | bold arg = arg
	  and code (#"`" :: #"`" :: r, acc) = (r, "</code>" :: acc)
	    | code (#"`" :: r, acc) = (r, "</code>" :: acc)
	    | code (c :: r, acc) = code (r, str c :: acc)
	    | code arg = arg
	  and italic (#"_" :: #"_" :: r, acc) = (r, "</i>" :: acc)
	    | italic (#"_" :: r, acc) = (r, "</i>" :: acc)
	    | italic (c :: r, acc) = code (r, str c :: acc)
	    | italic arg = arg
	  and text (#"*" :: #"*" :: r, acc) = text (bold (r, "<b>" :: acc))
	    | text (#"*" :: r, acc) = text (bold (r, "<b>" :: acc))
	    | text (#"`" :: #"`" :: r, acc) = text (code (r, "<code>" :: acc))
	    | text (#"`" :: r, acc) = text (code (r, "<code>" :: acc))
	    | text (#"_" :: #"_" :: r, acc) = text (italic (r, "<i>" :: acc))
	    | text (#"_" :: r, acc) = text (italic (r, "<i>" :: acc))
	    | text (c :: r, acc) = text (r, str c :: acc)
	    | text ([], acc) = acc
	  in
	    String.concat (List.rev (text (explode s, [])))
	  end

  (* returns the title string for the opening of a library item in the library
   * list.  The flags are as follows:
   *	link	-- generate an HREF link
   *	cur     -- this library is the current page
   *	inRoot  -- the containing page is at root level
   *)
    fun libItemOpen {lib, link, cur, inRoot} = let
	  val FT.LIB{dir, stem, title, ...} = lib
	  val s = if link then ["</a>"] else []
	  val s = if cur
		then "<span id=\"toc:current\">" :: styleText title :: "</span>" :: s
		else "<span class=\"toc:lib-title\">" :: styleText title :: "</span>" :: s
	  val s = if link
		then let
		  val dir = if inRoot orelse cur then dir else "../" ^ dir
		  in
		    "<a href=\"" :: dir :: "/" :: stem :: ".html\">" :: s
		  end
		else s
	  val s = "<li class=\"toc:lib\">" :: s
	  in
	    concat s
	  end

  (* returns the string for a page item in the pages list.  The
   * flags are as follows:
   *	link	-- generate an HREF link
   *	cur     -- this library is the current page
   *)
    fun pageItem {page, link, cur} = let
	  val FT.PAGE{stem, title, ...} = page
	  val s = if link then ["</a></li>"] else ["</li>"]
	  val s = if cur
		then "<span id=\"toc:current\">" :: styleText title :: "</span>" :: s
		else "<span class=\"toc:lib-page\">" :: styleText title :: "</span>" :: s
	  val s = if link
		then "<a href=\"" ::  stem :: ".html\">" :: s
		else s
	  val s = "<li class=\"toc:page\">" :: s
	  in
	    concat s
	  end

    fun doRoot outS (true, _) = pr(outS, "<ul class=\"toc:lib-list\">\n")
      | doRoot outS (false, _) = pr(outS, "</ul>\n")

  (* generate a TOC file for the root page *)
    fun appRoot (ft as FT.ROOT{stem, ...}) = doTOC (stem ^ ".toc") (fn outS => let
	  fun doLib (true, _, lib) =
		prl(outS, [
		    "  ", libItemOpen{lib=lib, link=true, cur=false, inRoot=true}
		  ])
	    | doLib (false, _, _) = pr(outS, "</li>\n")
	  in
	    FT.walk {
		root = doRoot outS,
		lib = doLib,
		page = fn _ => ()
	      } ft
	  end)

  (* generate a TOC file for a library page *)
    fun appLib (ft, lib as FT.LIB{dir, stem, ...}) =
	  doTOC (P.concat(dir, stem ^ ".toc")) (fn outS => let
	    fun doLib (true, _, lib') = let
		  val cur = FT.sameLib(lib, lib')
		  in
		    prl(outS, [
			"  ", libItemOpen{lib=lib', link=false, cur=cur, inRoot=false}
		      ]);
		    if cur
		      then pr(outS, "\n    <ul class=\"toc:page-list\">\n")
		      else ()
		  end
	      | doLib (false, _, lib') = if FT.sameLib(lib, lib')
		  then prl(outS, ["    </ul>\n", "  </li>\n"])
		  else pr(outS, "<li>\n")
	    fun doPage (_, lib', page) = if FT.sameLib(lib, lib')
		  then prl(outS, [
		      "      ",
		      pageItem {page=page, link=true, cur=false},
		      "\n"
		    ])
		  else ()
	    in
	      FT.walk {
		  root = doRoot outS,
		  lib = doLib,
		  page = doPage
		} ft
	    end)

  (* generate a TOC file for a manual page *)
    fun appPage (ft, lib, page as FT.PAGE{dir, stem, ...}) =
	  doTOC (P.concat(dir, stem ^ ".toc")) (fn outS => let
	    fun doLib (true, _, lib') = let
		  val cur = FT.sameLib(lib, lib')
		  in
		    prl(outS, [
			"  ", libItemOpen{lib=lib', link=false, cur=false, inRoot=false}
		      ]);
		    if cur
		      then pr(outS, "\n    <ul class=\"toc:page-list\">\n")
		      else ()
		  end
	      | doLib (false, _, lib') = if FT.sameLib(lib, lib')
		  then prl(outS, ["    </ul>\n", "  </li>\n"])
		  else pr(outS, "<li>\n")
	    fun doPage (_, lib', page') = if FT.sameLib(lib, lib')
		  then let
		    val cur = FT.samePage(page, page')
		    in
		      prl(outS, [
		          "      ",
		          pageItem {page=page', link=not cur, cur=cur},
			  "\n"
			])
		    end
		  else ()
	    in
	      FT.walk {
		  root = doRoot outS,
		  lib = doLib,
		  page = doPage
		} ft
	    end)

  (* for every documentation page, generate a table of contents file *)
    val walkTree = FT.app {root = appRoot, lib = appLib, page = appPage}

    fun main (cmd, args) = (case args
	   of "-h"::_ => usage OS.Process.success
	    | [indexFile] => (
		walkTree (loadIndex indexFile);
		OS.Process.success)
	    | _ => usage OS.Process.failure
	  (* end case *))

  end
