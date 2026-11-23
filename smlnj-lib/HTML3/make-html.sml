(* make-html.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This is a collection of constructors for building some of the common
 * kinds of HTML elements.
 *)

structure MakeHTML : sig

    val blockList : HTML.block list -> HTML.block
    val textList  : HTML.text list -> HTML.text

    val mkH : (int * HTML.pcdata) -> HTML.block

    val mkP : HTML.text -> HTML.block
    val mkUL : HTML.list_item list -> HTML.block
    val mkOL : HTML.list_item list -> HTML.block
    val mkDL : {dt : HTML.text list, dd : HTML.block} list -> HTML.block
    val HR : HTML.block
    val BR : HTML.text

    val mkLI : HTML.block -> HTML.list_item

    val mkA_HREF : {href : HTML.url, content : HTML.text} -> HTML.text
    val mkA_NAME : {name : HTML.cdata, content : HTML.text} -> HTML.text

    val mkTR : HTML.table_cell list -> HTML.tr
    val mkTH : HTML.block -> HTML.table_cell
    val mkTH_COLSPAN : {colspan : int, content : HTML.block} -> HTML.table_cell
    val mkTD : HTML.block -> HTML.table_cell
    val mkTD_COLSPAN : {colspan : int, content : HTML.block} -> HTML.table_cell

  end = struct

    fun blockList [b] = b
      | blockList bl = HTML.BlockList bl

    fun textList [t] = t
      | textList tl = HTML.TextList tl

    fun mkH (n, hdr) = HTML.Hn{n = n, align=NONE, content=HTML.PCDATA hdr}

    fun mkP content = HTML.P{align=NONE, content=content}

    fun mkUL items = HTML.UL{compact=false, ty=NONE, content=items}

    fun mkOL items = HTML.OL{compact=false, ty=NONE, start = NONE, content=items}

    fun mkDL items = HTML.DL{compact=false, content=items}

    val HR = HTML.HR{align=NONE, noshade=false, size=NONE, width=NONE}

    val BR = HTML.BR{clear = NONE}

    fun mkLI blk = HTML.LI{ty=NONE, value=NONE, content=blk}

    fun mkA_HREF {href, content} = HTML.A{
	    href = SOME href,
	    title = NONE,
	    name = NONE,
	    rel = NONE,
	    rev = NONE,
	    content = content
	  }

    fun mkA_NAME {name, content} = HTML.A{
	    href = NONE,
	    title = NONE,
	    name = SOME name,
	    rel = NONE,
	    rev = NONE,
	    content = content
	  }

    fun mkTR content = HTML.TR{
	    align = NONE,
	    valign = NONE,
	    content = content
	  }

    fun mkTH content = HTML.TH{
	    nowrap = false,
	    rowspan = NONE,
	    colspan = NONE,
	    align = NONE,
	    valign = NONE,
	    width = NONE,
	    height = NONE,
	    content = content
	  }
    fun mkTH_COLSPAN {colspan, content} = HTML.TH{
	    nowrap = false,
	    rowspan = NONE,
	    colspan = SOME colspan,
	    align = NONE,
	    valign = NONE,
	    width = NONE,
	    height = NONE,
	    content = content
	  }

    fun mkTD content = HTML.TD{
	    nowrap = false,
	    rowspan = NONE,
	    colspan = NONE,
	    align = NONE,
	    valign = NONE,
	    width = NONE,
	    height = NONE,
	    content = content
	  }
    fun mkTD_COLSPAN {colspan, content} = HTML.TD{
	    nowrap = false,
	    rowspan = NONE,
	    colspan = SOME colspan,
	    align = NONE,
	    valign = NONE,
	    width = NONE,
	    height = NONE,
	    content = content
	  }

  end;
