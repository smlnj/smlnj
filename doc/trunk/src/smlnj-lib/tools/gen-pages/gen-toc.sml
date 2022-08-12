(* gen-toc.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module is used to generate the HTML fragment for a sidebar
 * table of contents.
 *
 * TODO: eventually, we should include the individual definitions from
 * the file in the sidebar.
 *)

structure GenTOC : sig

    val root : FileTree.t -> TextIO.outstream -> unit
    val lib  : FileTree.t * FileTree.library -> TextIO.outstream -> unit
    val page : FileTree.t * FileTree.library * FileTree.page -> TextIO.outstream -> unit

  end = struct

    structure FT = FileTree

    fun pr (outS, s) = TextIO.output(outS, s)
    fun prl (outS, s) = TextIO.output(outS, concat s)

    fun doTOC outS f = (
	  prl (outS, [
	      "<div id=\"layout-toc\">\n",
	      "<div id=\"toc\">\n"
	    ]);
	  f outS;
	  prl (outS, [
	      "</div> <!-- toc -->\n",
	      "</div> <!-- layout-toc -->\n"
	    ]))

    val styleText = Util.style

  (* returns the title string for the opening of a library item in the library
   * list.  The flags are as follows:
   *	link	-- generate an HREF link
   *	cur     -- this library is the current page
   *	inRoot  -- the containing page is at root level
   *)
    fun libItemOpen {lib, link, cur, inRoot} = let
	  val FT.FILE{dir, stem, title, ...} = lib
	  val s = if link then ["</a>"] else []
	  val s = if cur
		then "<span class=\"toc-lib-title\" id=\"toc-current\">"
		  :: styleText title :: "</span>" :: s
		else "<span class=\"toc-lib-title\">" :: styleText title :: "</span>" :: s
	  val s = if link
		then let
		  val dir = if inRoot orelse cur then dir else "../" ^ dir
		  in
		    "<a href=\"" :: dir :: "/" :: stem :: ".html\">" :: s
		  end
		else s
	  val s = "<li class=\"toc-lib\">" :: s
	  in
	    concat s
	  end

  (* returns the string for a page item in the pages list.  The
   * flags are as follows:
   *	link	-- generate an HREF link
   *	cur     -- this library is the current page
   *)
    fun pageItem {page, link, cur} = let
	  val title = styleText(FT.getTitle page)
	  val s = if link then ["</a></li>"] else ["</li>"]
	  val s = if cur
		then "<span class=\"toc-lib-page\" id=\"toc-current\">"
		  :: title :: "</span>" :: s
		else "<span class=\"toc-lib-page\">" :: title :: "</span>" :: s
	  val s = if link
		then "<a href=\"" :: FT.getStem page :: ".html\">" :: s
		else s
	  val s = "<li class=\"toc-page\">" :: s
	  in
	    concat s
	  end

    fun doRoot outS (true, _) = pr(outS, "<ul class=\"toc-lib-list\">\n")
      | doRoot outS (false, _) = pr(outS, "</ul>\n")

  (* generate a TOC file for the root page *)
    fun root (ft) outS = doTOC outS (fn outS => let
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
    fun lib (ft, lib) outS = doTOC outS (fn outS => let
	  fun doLib (true, _, lib') = let
		val cur = FT.same(lib, lib')
		in
		  prl(outS, [
		      "  ", libItemOpen{lib=lib', link=not cur, cur=cur, inRoot=false}
		    ]);
		  if cur
		    then pr(outS, "\n    <ul class=\"toc-page-list\">\n")
		    else ()
		end
	    | doLib (false, _, lib') = if FT.same(lib, lib')
		then prl(outS, ["    </ul>\n", "  </li>\n"])
		else pr(outS, "</li>\n")
	  fun doPage (_, lib', page) = if FT.same(lib, lib')
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
    fun page (ft, lib, page) outS =
	  doTOC outS (fn outS => let
	    fun doLib (true, _, lib') = let
		  val cur = FT.same(lib, lib')
		  in
		    prl(outS, [
			"  ", libItemOpen{lib=lib', link=true, cur=false, inRoot=false}
		      ]);
		    if cur
		      then pr(outS, "\n    <ul class=\"toc-page-list\">\n")
		      else ()
		  end
	      | doLib (false, _, lib') = if FT.same(lib, lib')
		  then prl(outS, ["    </ul>\n", "  </li>\n"])
		  else pr(outS, "</li>\n")
	    fun doPage (_, lib', page') = if FT.same(lib, lib')
		  then let
		    val cur = FT.same(page, page')
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

  end
