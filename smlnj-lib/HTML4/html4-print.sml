(* html4-print.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * To print to an output stream (i.e., TextIO.outstream):
 *
 *      fun output outS = HTML4Print.prHTML {
 *              putc = fn c => TextIO.output1 (outS, c),
 *              puts = fn s => TextIO.output (outS, s)
 *            }
 *
 * To generate a string, use a character buffer:
 *
 *      fun toString html = let
 *            val buf = CharBuffer.new 1024
 *            in
 *              HTML4Print.prHTML {
 *                  putc = fn c => CharBuffer.add1 (buf, c),
 *                  puts = fn s => CharBuffer.addVec (buf, s)
 *                } html;
 *              CharBuffer.contents buf
 *            end
 *)

structure HTML4Print : sig

    val prHTML : {
	    putc    : char -> unit,
	    puts    : string -> unit
	  } -> HTML4.html -> unit

    val prBODY : {
	    putc    : char -> unit,
	    puts    : string -> unit
	  } -> HTML4.body -> unit

  end = struct

    structure H = HTML4
    structure F = Format

    datatype outstream = OS of {
	putc : char -> unit,
	puts : string -> unit
      }

    fun putc (OS{putc, ...}, c) = putc c
    fun puts (OS{puts, ...}, s) = puts s

  (* format an open tag *)
    fun fmtTag (tag, attrs) = let
	  fun fmtAttr ((attrName, NONE), l) = " " :: Atom.toString attrName :: l
	    | fmtAttr ((attrName, SOME s), l) = " " :: Atom.toString attrName :: "=\"" :: s :: "\"" :: l
	  in
	    String.concat("<" :: tag :: List.foldr fmtAttr [">"] attrs)
	  end

  (* format an tag with no content tag *)
    fun fmtEmptyTag (tag, attrs) = let
	  fun fmtAttr ((attrName, NONE), l) = " " :: Atom.toString attrName :: l
	    | fmtAttr ((attrName, SOME s), l) = " " :: Atom.toString attrName :: "=\"" :: s :: "\"" :: l
	  in
	    String.concat("<" :: tag :: List.foldr fmtAttr ["/>"] attrs)
	  end

    fun fmtEndTag tag = concat["</", tag, ">"]

    fun prTag (OS{puts, ...}, tag, attrs) = puts(fmtTag (tag, attrs))
    fun prEmptyTag (OS{puts, ...}, tag, attrs) = puts(fmtEmptyTag (tag, attrs))
    fun prEndTag (OS{puts, ...}, tag) = puts(fmtEndTag tag)
    fun newline (OS{putc, ...}) = putc #"\n"
    fun space (OS{putc, ...}) = putc #" "

   (* the various HTML4 headers *)
    val strictHdr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
    val looseHdr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
    val framesetHdr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">"
    val xhtmlHdr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

    fun isStrict _ = true  (* FIXME: should check content to see if there are uses of Loose elements *)

    fun prCDATA (outS, txt) = let
	  fun pr (H.CHAR chNum) = puts (outS, concat["&#", (IntInf.toString chNum), ";"])
	    | pr (H.COMMENT com) = puts (outS, concat["<!-- ", com, " -->"])
	    | pr (H.ENTITY ent) = puts (outS, concat["&", Atom.toString ent, ";"])
	    | pr (H.PCDATA s) = puts (outS, s)
	  in
	    List.app pr txt
	  end

    fun prScript (outS, H.SCRIPT(attrs, content)) =  (
	  prTag (outS, "STYLE", attrs); prCDATA (outS, content); prEndTag (outS, "STYLE"))

    fun prParam (outS, H.PARAM attrs) = prEmptyTag (outS, "PARAM", attrs)

  (* because an OBJECT element can appear in the HEAD and it contains a flow, we introduce
   * a union type for the argument to the body printing code
   *)
    datatype body_or_flow = Body of H.body | Flow of H.flow

    fun prBodyOrFlow (outS, element) = let
	  fun prFlowList (nl, []) = nl
	    | prFlowList (nl, H.Flow_BLOCK blk :: r) = (
		if nl then () else newline outS;
		prBlock blk;
		prFlowList (true, r))
	    | prFlowList (_, H.Flow_INLINE txt :: t) = (
		prInline txt;
		prFlowList (false, t))
	  and prFlowListElem (inline, tag, attrs, content) = (
		if inline
		andalso not (List.exists (fn (H.Flow_BLOCK _) => true | _ => false) content)
		  then (
		    prTag (outS, tag, attrs);
		    ignore (prFlowList (false, content));
		    prEndTag (outS, tag))
		  else (
		    prTag (outS, tag, attrs);
		    if prFlowList (true, content) then () else newline outS;
		    prEndTag (outS, tag)))
	  and prBlock elem = (
	        case elem
		 of H.P(attrs, content) => prInlineElem("P", attrs, content)
		  | H.H1(attrs, content) => prInlineElem("H1", attrs, content)
		  | H.H2(attrs, content) => prInlineElem("H2", attrs, content)
		  | H.H3(attrs, content) => prInlineElem("H3", attrs, content)
		  | H.H4(attrs, content) => prInlineElem("H4", attrs, content)
		  | H.H5(attrs, content) => prInlineElem("H5", attrs, content)
		  | H.H6(attrs, content) => prInlineElem("H6", attrs, content)
		  | H.UL(attrs, content) => prListElem("UL", attrs, content)
		  | H.OL(attrs, content) => prListElem("OL", attrs, content)
		  | H.DIR(attrs, content) => prListElem("DIR", attrs, content)
		  | H.MENU(attrs, content) => prListElem("MENU", attrs, content)
		  | H.PRE(attrs, content) => prInlineElem("PRE", attrs, content)
		  | H.DL(attrs, content) => let
		      fun prItem (H.DT(attrs, content)) = (
			    prInlineElem ("DT", attrs, content); newline outS)
			| prItem (H.DD(attrs, content)) = (
			    prFlowListElem (false, "DD", attrs, content); newline outS)
		      in
			prTag (outS, "DL", attrs); newline outS;
			List.app prItem content;
			prEndTag (outS, "DL")
		      end
		  | H.DIV(attrs, content) => prFlowListElem (false, "DIV", attrs, content)
		  | H.NOSCRIPT(attrs, content) => (
		      prTag (outS, "NOSCRIPT", attrs);
		      List.app prBlock content;
		      prEndTag (outS, "NOSCRIPT"))
		  | H.BLOCKQUOTE(attrs, content) => (
		      prTag (outS, "BLOCKQUOTE", attrs); newline outS;
		      List.app prBlockOrScript content;
		      prEndTag (outS, "BLOCKQUOTE"))
		  | H.FORM(attrs, content) => (
		      prTag (outS, "FORM", attrs); newline outS;
		      List.app prBlockOrScript content;
		      prEndTag (outS, "FORM"))
		  | H.HR attrs => prEmptyTag(outS, "HR", attrs)
		  | H.TABLE(attrs, content) => let
		      fun prCOL attrs = prTag (outS, "COL", attrs)
		      fun prData (H.CAPTION(attrs, content)) = (
			    prInlineElem("CAPTION", attrs, content); newline outS)
			| prData (H.COL attrs) = (prCOL attrs; newline outS)
			| prData (H.COLGROUP(attrs, content)) = (
			    prTag (outS, "COLGROUP", attrs);
			    List.app prCOL content;
			    prEndTag (outS, "COLGROUP"); newline outS)
			| prData (H.THEAD(attrs, content)) = prTableElem ("THEAD", attrs, content)
			| prData (H.TFOOT(attrs, content)) = prTableElem ("TFOOT", attrs, content)
			| prData (H.TBODY(attrs, content)) = prTableElem ("TBODY", attrs, content)
		      and prTableElem (tag, attrs, content) = let
			    fun prRow (H.TR(attrs, content)) = let
				  fun prCell (H.TH(attrs, content)) =
					prFlowListElem (true, "TH", attrs, content)
				    | prCell (H.TD(attrs, content)) =
					prFlowListElem (true, "TD", attrs, content)
				  in
				    prTag (outS, "TR", attrs); newline outS;
				    List.app prCell content; newline outS;
				    prEndTag (outS, "TR"); newline outS
				  end
			    in
			      prTag (outS, "TR", attrs); newline outS;
			      List.app prRow content;
			      prEndTag (outS, "TR"); newline outS
			    end
		      in
			prTag (outS, "TABLE", attrs); newline outS;
			List.app prData content;
			prEndTag (outS, "TABLE")
		      end
		  | H.FIELDSET(attrs, legend, content) => (
		      prTag (outS, "FIELDSET", attrs); newline outS;
		      case legend
		       of SOME(H.LEGEND(attrs, content)) => (
			    prInlineElem("LEGEND", attrs, content); newline outS)
			| NONE => ()
		      (* end case *);
		      if prFlowList (false, content) then () else newline outS;
		      prEndTag (outS, "FIELDSET"))
		  | H.ADDRESS(attrs, content) => prInlineElem("ADDRESS", attrs, content)
		  | H.CENTER(attrs, content) => prFlowListElem (false, "CENTER", attrs, content)
		  | H.ISINDEX attrs => prEmptyTag(outS, "ISINDEX", attrs)
		(* end case *);
		newline outS)
	  and prListElem (tag, attrs, content) = let
		fun prItem (H.LI(attrs, content)) = (
		      prFlowListElem (true, "LI", attrs, content);
		      newline outS)
		in
		  prTag (outS, tag, attrs); newline outS;
		  List.app prItem content;
		  prEndTag (outS, tag); newline outS
		end
	  and prInline elem = (case elem
		 of H.TT(attrs, content) => prInlineElem("TT", attrs, content)
		  | H.I(attrs, content) => prInlineElem("I", attrs, content)
		  | H.B(attrs, content) => prInlineElem("B", attrs, content)
		  | H.BIG(attrs, content) => prInlineElem("BIG", attrs, content)
		  | H.SMALL(attrs, content) => prInlineElem("SMALL", attrs, content)
		  | H.U(attrs, content) => prInlineElem("U", attrs, content)
		  | H.S(attrs, content) => prInlineElem("S", attrs, content)
		  | H.STRIKE(attrs, content) => prInlineElem("STRIKE", attrs, content)
		  | H.EM(attrs, content) => prInlineElem("EM", attrs, content)
		  | H.STRONG(attrs, content) => prInlineElem("STRONG", attrs, content)
		  | H.DFN(attrs, content) => prInlineElem("DFN", attrs, content)
		  | H.CODE(attrs, content) => prInlineElem("CODE", attrs, content)
		  | H.SAMP(attrs, content) => prInlineElem("SAMP", attrs, content)
		  | H.KBD(attrs, content) => prInlineElem("KBD", attrs, content)
		  | H.VAR(attrs, content) => prInlineElem("VAR", attrs, content)
		  | H.CITE(attrs, content) => prInlineElem("CITE", attrs, content)
		  | H.ABBR(attrs, content) => prInlineElem("ABBR", attrs, content)
		  | H.ACRONYM(attrs, content) => prInlineElem("ACRONYM", attrs, content)
		  | H.A(attrs, content) => prInlineElem("A", attrs, content)
		  | H.IMG attrs => prEmptyTag(outS, "IMG", attrs)
		  | H.OBJECT(attrs, content) => (
		      prTag (outS, "OBJECT", attrs); newline outS;
		      prFlowOrParamList content;
		      prEndTag (outS, "OBJECT"); newline outS)
		  | H.BR attrs => prEmptyTag(outS, "BR", attrs)
		  | H.Inline_SCRIPT script => (newline outS; prScript (outS, script))
		  | H.MAP(attrs, content) => raise Fail "FIXME"
		  | H.Q(attrs, content) => prInlineElem("Q", attrs, content)
		  | H.SUB(attrs, content) => prInlineElem("SUB", attrs, content)
		  | H.SUP(attrs, content) => prInlineElem("SUP", attrs, content)
		  | H.SPAN(attrs, content) => prInlineElem("SPAN", attrs, content)
		  | H.BDO(attrs, content) => prInlineElem("BDO", attrs, content)
		  | H.APPLET(attrs, content) => (
		      prTag (outS, "APPLET", attrs); newline outS;
		      prFlowOrParamList content;
		      prEndTag (outS, "APPLET"); newline outS)
		  | H.BASEFONT attrs => prEmptyTag(outS, "BASEFONT", attrs)
		  | H.FONT(attrs, content) => prInlineElem("FONT", attrs, content)
		  | H.IFRAME(attrs, content) => prFlowListElem (true, "IFRAME", attrs, content)
		  | H.INPUT attrs => prEmptyTag(outS, "INPUT", attrs)
		  | H.SELECT(attrs, content) => raise Fail "FIXME"
		  | H.TEXTAREA(attrs, content) => (
		      prTag (outS, "TEXTAREA", attrs);
		      prCDATA (outS, content);
		      prEndTag (outS, "TEXTAREA"))
		  | H.LABEL(attrs, content) => prInlineElem("LABEL", attrs, content)
		  | H.BUTTON(attrs, content) => prFlowListElem (true, "BUTTON", attrs, content)
		  | H.CDATA txt => prCDATA (outS, txt)
		(* end case *))
	  and prInlineElem (tag, attrs, content) = (
		prTag (outS, tag, attrs);
		List.app prInline content;
		prEndTag (outS, tag))
	  and prBlockOrScript (H.BlockOrScript_BLOCK blk) = prBlock blk
	    | prBlockOrScript (H.BlockOrScript_SCRIPT script) = (prScript (outS, script); newline outS)
	  and prFlowOrParamList content = let
		fun pr (H.FlowOrParam_FLOW(H.Flow_BLOCK blk), nl) = (
			  if nl then () else newline outS; prBlock blk; true)
		  | pr (H.FlowOrParam_FLOW(H.Flow_INLINE txt), _) = (prInline txt; false)
		  | pr (H.FlowOrParam_PARAM param, _) = (prParam (outS, param); false)
		in
		(* This function is always called after a newline *)
		  ignore (List.foldl pr true content)
		end
	  in
	    case element
	     of Body(H.BODY(attrs, content)) => (
		  prTag (outS, "BODY", attrs); newline outS;
		  List.app prBlockOrScript content;
		  prEndTag (outS, "BODY"); newline outS)
	      | Flow(H.Flow_BLOCK blk) => prBlock blk
	      | Flow(H.Flow_INLINE txt) => prInline txt
	    (* end case *)
	  end (* prBodyOrFlow *)

    fun prHTML outS (H.HTML{version, head, content}) = let
	  val outS = OS outS
	  fun prHeadContent (H.Head_TITLE(attrs, txt)) = (
		prTag (outS, "TITLE", attrs); newline outS;
		prCDATA (outS, txt); newline outS;
		prEndTag (outS, "TITLE"); newline outS)
	    | prHeadContent (H.Head_BASE attrs) = (
		prTag (outS, "TITLE", attrs); newline outS)
	    | prHeadContent (H.Head_SCRIPT script) = (
		prScript (outS, script); newline outS)
	    | prHeadContent (H.Head_STYLE(attrs, content)) = (
		prTag (outS, "STYLE", attrs); newline outS;
		prCDATA (outS, content); newline outS;
		prEndTag (outS, "STYLE"); newline outS)
	    | prHeadContent (H.Head_META attrs) = (
		prTag (outS, "META", attrs); newline outS)
	    | prHeadContent (H.Head_LINK attrs) = (
		prTag (outS, "LINK", attrs); newline outS)
	    | prHeadContent (H.Head_OBJECT(attrs, content)) = raise Fail "FIXME"
	  and prBodyOrFrameset (H.BodyOrFrameset_BODY body) = prBodyOrFlow (outS, Body body)
	    | prBodyOrFrameset (H.BodyOrFrameset_FRAMESET frameset) = prFrameset frameset
	  and prFrameset (H.FRAMESET(attrs, content, noframes)) = raise Fail "FIXME"
	  in
	    case (version, content)
	     of (SOME vers, _) => puts (outS, vers)
	      | (NONE, H.BodyOrFrameset_BODY(H.BODY(attrs, children))) =>
		  if isStrict children
		    then puts (outS, strictHdr)
		    else puts (outS, looseHdr)
	      | (NONE, H.BodyOrFrameset_FRAMESET _) => puts (outS, framesetHdr)
	    (* end case *);
	    newline outS;
	    puts (outS, "<HTML>\n");
	    puts (outS, "<HEAD>\n");
	    List.app prHeadContent head;
	    puts (outS, "</HEAD>\n");
	    prBodyOrFrameset content;
	    puts (outS, "</HTML>\n")
	  end

    fun prBODY outS body = prBodyOrFlow (OS outS, Body body)

  end
