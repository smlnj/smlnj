(* pr-html.sml
 *
 * COPYRIGHT (c) 1996 AT&T REsearch.
 *
 * Pretty-print an HTML tree.
 *)

structure PrHTML : sig

    val prHTML : {
	    putc    : char -> unit,
	    puts    : string -> unit
	  } -> HTML.html -> unit

  end = struct

    structure H = HTML
    structure F = Format

    datatype outstream = OS of {
	putc : char -> unit,
	puts : string -> unit
      }

    fun putc (OS{putc, ...}, c) = putc c
    fun puts (OS{puts, ...}, s) = puts s

    datatype attr_data
      = IMPLICIT of bool
      | CDATA of string option
      | NAME of string option
      | NUMBER of int option

    local
      fun name toString NONE = NAME NONE
        | name toString (SOME x) = NAME(SOME(toString x))
    in
    val httpMethod	= name HTML.HttpMethod.toString
    val inputType	= name HTML.InputType.toString
    val iAlign		= name HTML.IAlign.toString
    val hAlign		= name HTML.HAlign.toString
    val cellVAlign	= name HTML.CellVAlign.toString
    val captionAlign	= name HTML.CaptionAlign.toString
    val ulStyle		= name HTML.ULStyle.toString
    val shape		= name HTML.Shape.toString
    val textFlowCtl	= name HTML.TextFlowCtl.toString
    end (* local *)

    fun fmtTag (tag, []) = concat["<", tag, ">"]
      | fmtTag (tag, attrs) = let
	  fun fmtAttr (attrName, IMPLICIT true) = SOME attrName
	    | fmtAttr (attrName, CDATA(SOME s)) =
		SOME(F.format "%s=\"%s\"" [F.STR attrName, F.STR s])
	    | fmtAttr (attrName, NAME(SOME s)) =
		SOME(F.format "%s=%s" [F.STR attrName, F.STR s])
	    | fmtAttr (attrName, NUMBER(SOME n)) =
		SOME(F.format "%s=%d" [F.STR attrName, F.INT n])
	    | fmtAttr _ = NONE
	  val attrs = List.mapPartial fmtAttr attrs
	  in
	    ListFormat.fmt {
		init = "<",
		sep = " ",
		final = ">",
		fmt = fn x => x
	      } (tag :: attrs)
	  end

    fun fmtEndTag tag = concat["</", tag, ">"]

    fun prTag (OS{puts, ...}, tag, attrs) = puts(fmtTag (tag, attrs))
    fun prEndTag (OS{puts, ...}, tag) = puts(fmtEndTag tag)
    fun newLine (OS{putc, ...}) = putc #"\n"
    fun space (OS{putc, ...}) = putc #" "

  (** NOTE: once we are doing linebreaks for text, this becomes
   ** important.
   **)
    fun setPre (_, _) = ()

    fun prBlock (strm, blk : HTML.block) = (case blk
	   of (HTML.BlockList bl) =>
		List.app (fn b => prBlock (strm, b)) bl
	    | (HTML.TextBlock txt) => (prText (strm, txt); newLine strm)
	    | (HTML.Hn{n, align, content}) => let
		val tag = "H" ^ Int.toString n
		in
		  prTag (strm, tag, [("align", hAlign align)]);
		  prText (strm, content);
		  prEndTag (strm, tag);
		  newLine strm
		end
	    | (HTML.ADDRESS blk) => (
		prTag (strm, "ADDRESS", []);
		newLine strm;
		prBlock (strm, blk);
		prEndTag (strm, "ADDRESS");
		newLine strm)
	    | (HTML.P{align, content}) => (
		prTag (strm, "P", [("ALIGN", hAlign align)]);
		newLine strm;
		prText (strm, content);
		newLine strm)
	    | (HTML.UL{ty, compact, content}) => (
		prTag (strm, "UL", [
		    ("TYPE", ulStyle ty),
		    ("COMPACT", IMPLICIT compact)
		  ]);
		newLine strm;
		prListItems (strm, content);
		prEndTag (strm, "UL");
		newLine strm)
	    | (HTML.OL{ty, start, compact, content}) => (
		prTag (strm, "OL", [
		    ("TYPE", CDATA ty),
		    ("START", NUMBER start),
		    ("COMPACT", IMPLICIT compact)
		  ]);
		newLine strm;
		prListItems (strm, content);
		prEndTag (strm, "OL");
		newLine strm)
	    | (HTML.DIR{compact, content}) => (
		prTag (strm, "DIR", [("COMPACT", IMPLICIT compact)]);
		newLine strm;
		prListItems (strm, content);
		prEndTag (strm, "DIR");
		newLine strm)
	    | (HTML.MENU{compact, content}) => (
		prTag (strm, "MENU", [("COMPACT", IMPLICIT compact)]);
		newLine strm;
		prListItems (strm, content);
		prEndTag (strm, "MENU");
		newLine strm)
	    | (HTML.DL{compact, content}) => (
		prTag (strm, "DL", [("COMPACT", IMPLICIT compact)]);
		newLine strm;
		prDLItems (strm, content);
		prEndTag (strm, "DL");
		newLine strm)
	    | (HTML.PRE{width, content}) => (
		prTag (strm, "PRE", [("WIDTH", NUMBER width)]);
		newLine strm;
		setPre (strm, true);
		  prText (strm, content);
		setPre (strm, false);
		newLine strm;
		prEndTag (strm, "PRE");
		newLine strm)
	    | (HTML.DIV{align, content}) => (
		prTag (strm, "DIV", [("ALIGN", hAlign(SOME align))]);
		newLine strm;
		prBlock (strm, content);
		prEndTag (strm, "DIV");
		newLine strm)
	    | (HTML.CENTER bl) => (
		prTag (strm, "CENTER", []);
		newLine strm;
		prBlock (strm, bl);
		prEndTag (strm, "CENTER");
		newLine strm)
	    | (HTML.BLOCKQUOTE bl) => (
		prTag (strm, "BLOCKQUOTE", []);
		newLine strm;
		prBlock (strm, bl);
		prEndTag (strm, "BLOCKQUOTE");
		newLine strm)
	    | (HTML.FORM{action, method, enctype, content}) => (
		prTag (strm, "FORM", [
		    ("ACTION", CDATA action),
		    ("METHOD", httpMethod(SOME method)),
		    ("ENCTYPE", CDATA enctype)
		  ]);
		newLine strm;
		prBlock (strm, content);
		prEndTag (strm, "FORM");
		newLine strm)
	    | (HTML.ISINDEX{prompt}) => (
		prTag (strm, "ISINDEX", [("PROMPT", CDATA prompt)]);
		newLine strm)
	    | (HTML.HR{align, noshade, size, width}) => (
		prTag (strm, "HR", [
		    ("ALIGN", hAlign align),
		    ("NOSHADE", IMPLICIT noshade),
		    ("SIZE", CDATA size),
		    ("WIDTH", CDATA width)
		  ]);
		newLine strm)
	    | (HTML.TABLE{
		  align, width, border, cellspacing, cellpadding,
		  caption, content
		}) => (
		  prTag (strm, "TABLE", [
		      ("ALIGN", hAlign align),
		      ("WIDTH", CDATA width),
		      ("BORDER", CDATA border),
		      ("CELLSPACING", CDATA cellspacing),
		      ("CELLPADDING", CDATA cellpadding)
		    ]);
		  newLine strm;
		  prCaption (strm, caption);
		  prTableRows (strm, content);
		  prEndTag (strm, "TABLE");
		  newLine strm)
	  (* end case *))

    and prListItems (strm, items) = let
	  fun prItem (HTML.LI{ty, value, content}) = (
		prTag (strm, "LI", [("TYPE", CDATA ty), ("VALUE", NUMBER value)]);
		newLine strm;
		prBlock (strm, content))
	  in
	    List.app prItem items
	  end

    and prDLItems (strm, items) = let
	  fun prDT txt = (
		prTag (strm, "DT", []);
		space strm;
		prText (strm, txt);
		newLine strm)
	  fun prDD blk = (
		prTag (strm, "DD", []);
		newLine strm;
		prBlock (strm, blk))
	  fun prItem ({dt, dd}) = (List.app prDT dt; prDD dd)
	  in
	    List.app prItem items
	  end

    and prCaption (strm, NONE) = ()
      | prCaption (strm, SOME(HTML.CAPTION{align, content})) = (
	  prTag (strm, "CAPTION", [("ALIGN", captionAlign align)]);
	  newLine strm;
	  prText (strm, content);
	  prEndTag (strm, "CAPTION");
	  newLine strm)

    and prTableRows (strm, rows) = let
	  fun prTR (HTML.TR{align, valign, content}) = (
		prTag (strm, "TR", [
		    ("ALIGN", hAlign align),
		    ("VALIGN", cellVAlign valign)
		  ]);
		newLine strm;
		List.app (prTableCell strm) content)
	  in
	    List.app prTR rows
	  end

    and prTableCell strm cell = let
	  fun prCell (tag, {
		nowrap, rowspan, colspan , align, valign, width, height,
		content
	      }) = (
		prTag (strm, tag, [
		    ("NOWRAP", IMPLICIT nowrap),
		    ("ROWSPAN", NUMBER rowspan),
		    ("COLSPAN", NUMBER colspan),
		    ("ALIGN", hAlign align),
		    ("VALIGN", cellVAlign valign),
		    ("WIDTH", CDATA width),
		    ("HEIGHT", CDATA height)
		  ]);
		newLine strm;
		prBlock (strm, content))
	  in
	    case cell
	     of (HTML.TH stuff) => prCell ("TH", stuff)
	      | (HTML.TD stuff) => prCell ("TD", stuff)
	    (* end case *)
	  end

    and prEmph (strm, tag, text) = (
	  prTag (strm, tag, []);
	  prText (strm, text);
	  prEndTag (strm, tag))

    and prText (strm, text) = (case text
	   of (HTML.TextList tl) =>
		List.app (fn txt => prText(strm, txt)) tl
	    | (HTML.PCDATA pcdata) => prPCData(strm, pcdata)
	    | (HTML.TT txt) => prEmph (strm, "TT", txt)
	    | (HTML.I txt) => prEmph (strm, "I", txt)
	    | (HTML.B txt) => prEmph (strm, "B", txt)
	    | (HTML.U txt) => prEmph (strm, "U", txt)
	    | (HTML.STRIKE txt) => prEmph (strm, "STRIKE", txt)
	    | (HTML.BIG txt) => prEmph (strm, "BIG", txt)
	    | (HTML.SMALL txt) => prEmph (strm, "SMALL", txt)
	    | (HTML.SUB txt) => prEmph (strm, "SUB", txt)
	    | (HTML.SUP txt) => prEmph (strm, "SUP", txt)
	    | (HTML.EM txt) => prEmph (strm, "EM", txt)
	    | (HTML.STRONG txt) => prEmph (strm, "STRONG", txt)
	    | (HTML.DFN txt) => prEmph (strm, "DFN", txt)
	    | (HTML.CODE txt) => prEmph (strm, "CODE", txt)
	    | (HTML.SAMP txt) => prEmph (strm, "SAMP", txt)
	    | (HTML.KBD txt) => prEmph (strm, "KBD", txt)
	    | (HTML.VAR txt) => prEmph (strm, "VAR", txt)
	    | (HTML.CITE txt) => prEmph (strm, "CITE", txt)
	    | (HTML.A{name, href, rel, rev, title, content}) => (
		prTag (strm, "A", [
		    ("NAME", CDATA name),
		    ("HREF", CDATA href),
		    ("REL", CDATA rel),
		    ("REV", CDATA rev),
		    ("TITLE", CDATA title)
		  ]);
		prText (strm, content);
		prEndTag (strm, "A"))
	    | (HTML.IMG{
		  src, alt, align, height, width, border,
		  hspace, vspace, usemap, ismap
		}) => prTag (strm, "IMG", [
		    ("SRC", CDATA(SOME src)),
		    ("ALT", CDATA alt),
		    ("ALIGN", iAlign align),
		    ("HEIGHT", CDATA height),
		    ("WIDTH", CDATA width),
		    ("BORDER", CDATA border),
		    ("HSPACE", CDATA hspace),
		    ("VSPACE", CDATA vspace),
		    ("USEMAP", CDATA usemap),
		    ("ISMAP", IMPLICIT ismap)
		  ])
	    | (HTML.APPLET{
		  codebase, code, name, alt, align, height, width,
		  hspace, vspace, content
		}) => (
		prTag (strm, "APPLET", [
		    ("CODEBASE", CDATA codebase),
		    ("CODE", CDATA(SOME code)),
		    ("NAME", CDATA name),
		    ("ALT", CDATA alt),
		    ("ALIGN", iAlign align),
		    ("HEIGHT", CDATA height),
		    ("WIDTH", CDATA width),
		    ("HSPACE", CDATA hspace),
		    ("VSPACE", CDATA vspace)
		  ]);
		prText (strm, content);
		prEndTag (strm, "APPLET"))
	    | (HTML.PARAM{name, value}) =>
		prTag (strm, "PARAM", [
		    ("NAME", NAME(SOME name)),
		    ("VALUE", CDATA value)
		  ])
	    | (HTML.FONT{size, color, content}) => (
		prTag (strm, "FONT", [
		    ("SIZE", CDATA size),
		    ("COLOR", CDATA color)
		  ]);
		prText (strm, content);
		prEndTag (strm, "FONT"))
	    | (HTML.BASEFONT{size, content}) => (
		prTag (strm, "BASEFONT", [("SIZE", CDATA size)]);
		prText (strm, content);
		prEndTag (strm, "BASEFONT"))
	    | (HTML.BR{clear}) => (
		prTag (strm, "BR", [("CLEAR", textFlowCtl clear)]);
		newLine strm)
	    | (HTML.MAP{name, content}) => (
		prTag (strm, "MAP", [("NAME", CDATA name)]);
		List.app (prArea strm) content;
		prEndTag (strm, "MAP"))
	    | (HTML.INPUT{
		  ty, name, value, checked, size, maxlength, src, align
		}) => prTag (strm, "INPUT", [
		    ("TYPE", inputType ty),
		    ("NAME", NAME name),
		    ("VALUE", CDATA value),
		    ("CHECKED", IMPLICIT checked),
		    ("SIZE", CDATA size),
		    ("MAXLENGTH", NUMBER maxlength),
		    ("SRC", CDATA src),
		    ("ALIGN", iAlign align)
		  ])
	    | (HTML.SELECT{name, size, content}) => (
		prTag (strm, "SELECT", [
		    ("NAME", NAME(SOME name)),
		    ("SIZE", NUMBER size)
		  ]);
		List.app (prOption strm) content;
		prEndTag (strm, "SELECT"))
	    | (HTML.TEXTAREA{name, rows, cols, content}) => (
		prTag (strm, "TEXTAREA", [
		    ("NAME", NAME(SOME name)),
		    ("ROWS", NUMBER(SOME rows)),
		    ("COLS", NUMBER(SOME cols))
		  ]);
		prPCData (strm, content);
		prEndTag (strm, "TEXTAREA"))
	  (* SCRIPT elements are placeholders for the next version of HTML *)
	    | (HTML.SCRIPT pcdata) => ()
	  (* end case *))

    and prArea strm (HTML.AREA{shape=s, coords, href, nohref, alt}) =
	  prTag (strm, "AREA", [
	      ("SHAPE", shape s),
	      ("COORDS", CDATA coords),
	      ("HREF", CDATA href),
	      ("nohref", IMPLICIT nohref),
	      ("ALT", CDATA(SOME alt))
	    ])

    and prOption strm (HTML.OPTION{selected, value, content}) = (
	  prTag (strm, "OPTION", [
	      ("SELECTED", IMPLICIT selected),
	      ("VALUE", CDATA value)
	    ]);
	  prPCData (strm, content))

    and prPCData (strm, data) = puts (strm, data)

    fun prBody (strm, HTML.BODY{
	  background, bgcolor, text, link, vlink, alink, content
	}) = (
	  prTag (strm, "BODY", [
	      ("BACKGROUND", CDATA background),
	      ("BGCOLOR", CDATA bgcolor),
	      ("TEXT", CDATA text),
	      ("LINK", CDATA link),
	      ("VLINK", CDATA vlink),
	      ("ALINK", CDATA alink)
	    ]);
	  prBlock (strm, content);
	  prEndTag (strm, "BODY"))

    fun prHeadElement strm elem = (case elem
	   of (HTML.Head_TITLE pcdata) => (
		prTag (strm, "TITLE", []);
		prPCData(strm, pcdata);
		prEndTag (strm, "TITLE");
		newLine strm)
	    | (HTML.Head_ISINDEX{prompt}) =>  (
		prTag (strm, "ISINDEX", [("PROMPT", CDATA prompt)]);
		newLine strm)
	    | (HTML.Head_BASE{href}) => (
		prTag (strm, "BASE", [("HREF", CDATA(SOME href))]);
		newLine strm)
	    | (HTML.Head_META{httpEquiv, name, content}) => (
		prTag (strm, "META", [
		    ("HTTP-EQUIV", NAME httpEquiv),
		    ("NAME", NAME name),
		    ("CONTENT", CDATA(SOME content))
		  ]);
		newLine strm)
	    | (HTML.Head_LINK{id, href, rel, rev, title}) => (
		prTag (strm, "LINK", [
		    ("ID", NAME id),
		    ("HREF", CDATA href),
		    ("REL", CDATA rel),
		    ("REV", CDATA rev),
		    ("TITLE", CDATA title)
		  ]);
		newLine strm)
	      (* SCRIPT/STYLE elements are placeholders for the next version of HTML *)
	    | (HTML.Head_SCRIPT pcdata) => ()
	    | (HTML.Head_STYLE pcdata) => ()
	  (* end case *))

    fun prHTML {putc, puts} html = let
	  val strm = OS{putc=putc, puts=puts}
	  val HTML.HTML{head, body, version} = html
	  in
	    case version
	     of NONE => ()
	      | (SOME v) => puts (F.format
		  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML %s//EN\">\n"
		  [F.STR v])
	    (* end case *);
	    puts "<HTML>\n";
	    puts "<HEAD>\n";
	    List.app (prHeadElement strm) head;	
	    puts "</HEAD>\n";
	    prBody (strm, body);
	    puts "</HTML>\n"
	  end

  end

