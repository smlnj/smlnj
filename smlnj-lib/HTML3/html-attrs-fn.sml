(* html-attrs-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This provides support for parsing element start tags.
 *)

functor HTMLAttrsFn (Err : HTML_ERROR) : HTML_ATTRS =
  struct

    open HTMLAttrVals	(* inherit types *)

    fun attrValToString (NAME s) = s
      | attrValToString (STRING s) = s
      | attrValToString IMPLICIT = ""

    datatype attr_ty
      = AT_TEXT			(* either a string or name value *)
      | AT_NAMES of string list	(* one of a list of names *)
      | AT_NUMBER		(* an integer attribute *)
      | AT_IMPLICIT
      | AT_INSTANCE		(* if an attribute FOO has type AT_NAMES with *)
				(* values BAR and BAZ, then BAR and BAZ are *)
				(* legal attributes, being shorthand for *)
				(* FOO=BAR and FOO=BAZ.  We introduce an *)
				(* (k, AT_INSTANCE) entry for BAR and BAZ, where *)
				(* k is the slot that FOO has been assigned. *)

    type context = Err.context

    structure HTbl = HashTableFn (struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey = (op = : (string * string) -> bool)
      end)

  (* an attribute map (attr_map) is a map from attribute names to attribute
   * value slots and types.
   *)
    abstype attr_map = AMap of {
	numAttrs : int,
	attrTbl : (int * attr_ty) HTbl.hash_table
      }
    and attr_vec = AVec of {
	vec : attr_val option Array.array,
	ctx : context
      }
    with
  (* create an attr_map from the list of attribute names and types. *)
    fun mkAttrs data = let
	  val n = length data
	  val tbl = HTbl.mkTable (n, Fail "Attrs")
	  fun ins ((name, ty), id) = (
		HTbl.insert tbl (name, (id, ty));
		case ty
		 of (AT_NAMES l) => let
		      fun ins' nm = if (nm <> name)
			    then HTbl.insert tbl (nm, (id, AT_INSTANCE))
			    else ()
		      in
			List.app ins' l
		      end
		  | _ => ()
		(* end case *);
		id+1)
	  in
	    List.foldl ins 0 data;
	    AMap{numAttrs = n, attrTbl = tbl}
	  end
  (* create an atttribute vector of attribute values using the attribute
   * map to assign slots and typecheck the values.
   *)
    fun attrListToVec (ctx, AMap{numAttrs, attrTbl}, attrs) = let
	  val attrArray = Array.array (numAttrs, NONE)
	  fun update (_, NONE) = ()
	    | update (id, SOME v) = (case Array.sub(attrArray, id)
		 of NONE => Array.update(attrArray, id, SOME v)
		  | (SOME _) => (* ignore multiple attribute definition *) ()
		(* end case *))
	(* compare two names for case-insensitive equality, where the second
	 * name is known to be all uppercase.
	 *)
	  fun eqName name name' = let
		fun cmpC (c1, c2) = Char.compare(Char.toUpper c1, c2)
		in
		  (String.collate cmpC (name, name')) = EQUAL
		end
	  fun ins (attrName, attrVal) = let
		fun error () = (
		      Err.badAttrVal ctx (attrName, attrValToString attrVal);
		      NONE)
		fun cvt (AT_IMPLICIT, IMPLICIT) = SOME IMPLICIT
		  | cvt (AT_INSTANCE, IMPLICIT) = SOME(NAME attrName)
		  | cvt (AT_TEXT, v) = SOME v
		  | cvt (AT_NUMBER, v) = SOME v
		  | cvt (AT_NAMES names, (NAME s | STRING s)) = (
		      case (List.find (eqName s) names)
		       of NONE => error()
			| (SOME name) => SOME(NAME name)
		      (* end case *))
		  | cvt (AT_IMPLICIT, (NAME s | STRING s)) =
		      if (s = attrName)
			then SOME IMPLICIT
			else error()
		  | cvt _ = error()
		in
		  case (HTbl.find attrTbl attrName)
		   of NONE => Err.unknownAttr ctx attrName
		    | (SOME(id, ty)) => update (id, cvt (ty, attrVal))
		  (* end case *)
		end
	  in
	    List.app ins attrs;
	    AVec{vec = attrArray, ctx = ctx}
	  end
  (* given an attribute map and attribute name, return a function that
   * fetches a value from the attribute's slot in an attribute vector.
   *)
    fun bindFindAttr (AMap{attrTbl, ...}, attr) = let
	  val (id, _) = HTbl.lookup attrTbl attr
	  in
	    fn (AVec{vec, ...}) => Array.sub(vec, id)
	  end
  (* return the context of the element that contains the attribute vector *)
    fun getContext (AVec{ctx, ...}) = ctx
    end (* abstype *)

    fun getFlag (attrMap, attr) = let
	  val getFn = bindFindAttr (attrMap, attr)
	  fun get attrVec = (case (getFn attrVec)
		 of NONE => false
		 | _ => true
		(* end case *))
	  in
	    get
	  end
    fun getCDATA (attrMap, attr) = let
	  val getFn = bindFindAttr (attrMap, attr)
	  fun get attrVec = (case (getFn attrVec)
		 of NONE => NONE
		  | (SOME((STRING s) | (NAME s))) => SOME s
		  | _ => (
		      Err.missingAttrVal (getContext attrVec) attr;
		      NONE)
		(* end case *))
	  in
	    get
	  end
    fun getNAMES fromString (attrMap, attr) = let
	  val getFn = bindFindAttr (attrMap, attr)
	  fun get attrVec = (case (getFn attrVec)
		 of NONE => NONE
		  | (SOME(NAME s)) => fromString s
		  | (SOME v) =>
		    (** This case should be impossible, since attrListToVec
		     ** ensures that AT_NAMES valued attributes are always NAME.
		     **)
		      raise Fail "getNAMES"
		(* end case *))
	  in
	    get
	  end
    fun getNUMBER (attrMap, attr) = let
	  val getFn = bindFindAttr (attrMap, attr)
	  fun get attrVec = (case (getFn attrVec)
		 of NONE => NONE
		  | (SOME((STRING s) | (NAME s))) => (case (Int.fromString s)
		       of NONE =>  (
			    Err.badAttrVal (getContext attrVec) (attr, s);
			    NONE)
			| someN => someN
		      (* end case *))
		  | SOME IMPLICIT => raise Fail "getNUMBER: IMPLICIT unexpected"
		(* end case *))
	  in
	    get
	  end
    fun getChar (attrMap, attr) = let
	  val getFn = bindFindAttr (attrMap, attr)
	  fun get attrVec = (case (getFn attrVec)
		 of NONE => NONE
		  | (SOME((STRING s) | (NAME s))) =>
		      if (size s = 1) then SOME(String.sub(s, 0))
(** NOTE: we should probably accept &#xx; as a character value **)
			else  (
			Err.badAttrVal (getContext attrVec) (attr, s);
			NONE)
		  | SOME IMPLICIT => raise Fail "getChar: IMPLICIT unexpected"
		(* end case *))
	  in
	    get
	  end

    fun require (getFn, attrMap, attr, dflt) = let
	  val getFn = getFn (attrMap, attr)
	  fun get attrVec = (case getFn attrVec
		 of NONE => (Err.missingAttr (getContext attrVec) attr; dflt)
		  | (SOME v) => v
		(* end case *))
	  in
	    get
	  end

  (**** Element ISINDEX ****)
    local
      val attrMap = mkAttrs [
	      ("PROMPT",	AT_TEXT)
	    ]
      val getPROMPT	= getCDATA (attrMap, "PROMPT")
    in
  (* the ISINDEX element can occur in both the HEAD an BODY, so there are
   * two datatype constructors for it.  We just define the argument of the
   * constructor here.
   *)
    fun mkISINDEX (ctx, attrs) = {
	    prompt	= getPROMPT (attrListToVec(ctx, attrMap, attrs))
	  }
    end (* local *)

  (**** Element BASE ****)
    local
      val attrMap = mkAttrs [
	      ("HREF",		AT_TEXT)
	    ]
      val getHREF	= require (getCDATA, attrMap, "HREF", "")
    in
    fun mkBASE (ctx, attrs) = HTML.Head_BASE{
	    href = getHREF(attrListToVec(ctx, attrMap, attrs))
	  }
    end (* local *)

  (**** Element META ****)
    local
      val attrMap = mkAttrs [
	      ("HTTP-EQUIV",	AT_TEXT),
	      ("NAME",		AT_TEXT),
	      ("CONTENT",	AT_TEXT)
	    ]
      val getHTTP_EQUIV	= getCDATA (attrMap, "HTTP-EQUIV")
      val getNAME	= getCDATA (attrMap, "NAME")
      val getCONTENT	= require (getCDATA, attrMap, "CONTENT", "")
    in
    fun mkMETA (ctx, attrs) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.Head_META{
		httpEquiv = getHTTP_EQUIV attrVec,
		name = getNAME attrVec,
		content = getCONTENT attrVec
	      }
	  end
    end (* local *)

  (**** Element LINK ****)
    local
      val attrMap = mkAttrs [
	      ("HREF",		AT_TEXT),
	      ("ID",		AT_TEXT),
	      ("TITLE",		AT_TEXT),
	      ("REL",		AT_TEXT),
	      ("REV",		AT_TEXT)
	    ]
      val getHREF	= getCDATA (attrMap, "HREF")
      val getID		= getCDATA (attrMap, "ID")
      val getREL	= getCDATA (attrMap, "REL")
      val getREV	= getCDATA (attrMap, "REV")
      val getTITLE	= getCDATA (attrMap, "TITLE")
    in
    fun mkLINK (ctx, attrs) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.Head_LINK{
		href = getHREF attrVec,
		id = getID attrVec,
		rel = getREL attrVec,
		rev = getREV attrVec,
		title = getTITLE attrVec
	      }
	  end
    end (* local *)

  (**** Element BODY ****)
    local
      val attrMap = mkAttrs [
	      ("BACKGROUND",	AT_TEXT),
	      ("BGCOLOR",	AT_TEXT),
	      ("TEXT",		AT_TEXT),
	      ("LINK",		AT_TEXT),
	      ("VLINK",		AT_TEXT),
	      ("ALINK",		AT_TEXT)
	    ]
      val getBACKGROUND	= getCDATA (attrMap, "BACKGROUND")
      val getBGCOLOR	= getCDATA (attrMap, "BGCOLOR")
      val getTEXT	= getCDATA (attrMap, "TEXT")
      val getLINK	= getCDATA (attrMap, "LINK")
      val getVLINK	= getCDATA (attrMap, "VLINK")
      val getALINK	= getCDATA (attrMap, "ALINK")
    in
    fun mkBODY (ctx, attrs, blk) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.BODY{
		background = getBACKGROUND attrVec,
		bgcolor = getBGCOLOR attrVec,
		text = getTEXT attrVec,
		link = getLINK attrVec,
		vlink = getVLINK attrVec,
		alink = getALINK attrVec,
		content = blk
	      }
	  end
    end (* local *)

  (**** Elements H1, H2, H3, H4, H5, H6 and P ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["LEFT", "CENTER", "RIGHT"])
	    ]
      val getALIGN	= getNAMES HTML.HAlign.fromString (attrMap, "ALIGN")
    in
    fun mkHn (n, ctx, attrs, text) = HTML.Hn{
	    n = n,
	    align = getALIGN(attrListToVec(ctx, attrMap, attrs)),
	    content = text
	  }
    fun mkP (ctx, attrs, text) = HTML.P{
	    align = getALIGN(attrListToVec(ctx, attrMap, attrs)),
	    content = text
	  }
    end (* local *)

  (**** Element UL ****)
    local
      val attrMap = mkAttrs [
	      ("COMPACT",	AT_IMPLICIT),
	      ("TYPE",		AT_NAMES["DISC", "SQUARE", "CIRCLE"])
	    ]
      val getCOMPACT = getFlag(attrMap, "COMPACT")
      val getTYPE = getNAMES HTML.ULStyle.fromString (attrMap, "TYPE")
    in
    fun mkUL (ctx, attrs, items) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.UL{
		ty = getTYPE attrVec,
		compact = getCOMPACT attrVec,
		content = items
	      }
	  end
    end (* local *)

  (**** Element OL ****)
    local
      val attrMap = mkAttrs [
	      ("COMPACT",	AT_IMPLICIT),
	      ("START",		AT_NUMBER),
	      ("TYPE",		AT_TEXT)
	    ]
      val getCOMPACT = getFlag(attrMap, "COMPACT")
      val getSTART = getNUMBER(attrMap, "START")
      val getTYPE = getCDATA(attrMap, "TYPE")
    in
    fun mkOL (ctx, attrs, items) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.OL{
		compact = getCOMPACT attrVec,
		start = getSTART attrVec,
		ty = getTYPE attrVec,
		content = items
	      }
	  end
    end (* local *)

  (**** Elements DIR, MENU and DL ****)
    local
      val attrMap = mkAttrs [
	      ("COMPACT",	AT_IMPLICIT)
	    ]
      val getCOMPACT = getFlag(attrMap, "COMPACT")
    in
    fun mkDIR (ctx, attrs, items) = HTML.DIR{
	    compact = getCOMPACT (attrListToVec(ctx, attrMap, attrs)),
	    content = items
	  }
    fun mkMENU (ctx, attrs, items) = HTML.MENU{
	    compact = getCOMPACT (attrListToVec(ctx, attrMap, attrs)),
	    content = items
	  }
    fun mkDL (ctx, attrs, items) = HTML.DL{
	    compact = getCOMPACT (attrListToVec(ctx, attrMap, attrs)),
	    content = items
	  }
    end (* local *)

  (**** Element LI ****)
    local
      val attrMap = mkAttrs [
	      ("TYPE",		AT_TEXT),
	      ("VALUE",		AT_NUMBER)
	    ]
      val getTYPE = getCDATA(attrMap, "TYPE")
      val getVALUE = getNUMBER(attrMap, "VALUE")
    in
    fun mkLI (ctx, attrs, text) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.LI{
		ty = getTYPE attrVec,
		value = getVALUE attrVec,
		content = text
	      }
	  end
    end (* local *)

  (**** Element PRE ****)
    local
      val attrMap = mkAttrs [
	      ("WIDTH",		AT_NUMBER)
	    ]
      val getWIDTH = getNUMBER(attrMap, "WIDTH")
    in
    fun mkPRE (ctx, attrs, text) = HTML.PRE{
	    width = getWIDTH (attrListToVec (ctx, attrMap, attrs)),
	    content = text
	  }
    end (* local *)

  (**** Element DIV ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["LEFT", "CENTER", "RIGHT"])
	    ]
      val getALIGN	= require (getNAMES HTML.HAlign.fromString,
			    attrMap, "ALIGN", HTML.HAlign.left)
    in
    fun mkDIV (ctx, attrs, content) = HTML.DIV{
	    align = getALIGN(attrListToVec(ctx, attrMap, attrs)),
	    content = content
	  }
    end (* local *)

  (**** Element FORM ****)
    local
      val attrMap = mkAttrs [
	      ("ACTION",	AT_TEXT),
	      ("METHOD",	AT_NAMES["GET", "PUT"]),
	      ("ENCTYPE",	AT_TEXT)
	    ]
      val getACTION	= getCDATA (attrMap, "ACTION")
      val getMETHOD	= require (getNAMES HTML.HttpMethod.fromString,
			    attrMap, "METHOD", HTML.HttpMethod.get)
      val getENCTYPE	= getCDATA (attrMap, "ENCTYPE")
    in
    fun mkFORM (ctx, attrs, contents) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.FORM{
		action = getACTION attrVec,
		method = getMETHOD attrVec,
		enctype = getENCTYPE attrVec,
		content = contents
	      }
	  end
    end (* local *)

  (**** Element HR ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["LEFT", "CENTER", "RIGHT"]),
	      ("NOSHADE",	AT_IMPLICIT),
	      ("SIZE",		AT_TEXT),
	      ("WIDTH",		AT_TEXT)
	    ]
      val getALIGN	= getNAMES HTML.HAlign.fromString (attrMap, "ALIGN")
      val getNOSHADE	= getFlag (attrMap, "NOSHADE")
      val getSIZE	= getCDATA (attrMap, "SIZE")
      val getWIDTH	= getCDATA (attrMap, "WIDTH")
    in
    fun mkHR (ctx, attrs) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.HR{
		align = getALIGN attrVec,
		noshade = getNOSHADE attrVec,
		size = getSIZE attrVec,
		width = getWIDTH attrVec
	      }
	  end		
    end (* local *)

  (**** Element TABLE ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["LEFT", "CENTER", "RIGHT"]),
	      ("BORDER",	AT_TEXT),
	      ("CELLSPACING",	AT_TEXT),
	      ("CELLPADDING",	AT_TEXT),
	      ("WIDTH",		AT_TEXT)
	    ]
      val getALIGN		= getNAMES HTML.HAlign.fromString (attrMap, "ALIGN")
      val getBORDER		= getCDATA (attrMap, "BORDER")
      val getCELLSPACING	= getCDATA (attrMap, "CELLSPACING")
      val getCELLPADDING	= getCDATA (attrMap, "CELLPADDING")
      val getWIDTH		= getCDATA (attrMap, "WIDTH")
    in
    fun mkTABLE (ctx, attrs, {caption, body}) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.TABLE{
		align = getALIGN attrVec,
		border = getBORDER attrVec,
		cellspacing = getCELLSPACING attrVec,
		cellpadding = getCELLPADDING attrVec,
		width = getWIDTH attrVec,
		caption = caption,
		content = body
	      }
	  end
    end (* local *)

  (**** Element CAPTION ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["TOP", "BOTTOM"])
	    ]
      val getALIGN	= getNAMES HTML.CaptionAlign.fromString (attrMap, "ALIGN")
    in
    fun mkCAPTION (ctx, attrs, text) = HTML.CAPTION{
	    align = getALIGN(attrListToVec(ctx, attrMap, attrs)),
	    content = text
	  }
    end (* local *)

  (**** Element TR ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["LEFT", "CENTER", "RIGHT"]),
	      ("VALIGN",	AT_NAMES["TOP", "MIDDLE", "BOTTOM", "BASELINE"])
	    ]
      val getALIGN	= getNAMES HTML.HAlign.fromString (attrMap, "ALIGN")
      val getVALIGN	= getNAMES HTML.CellVAlign.fromString (attrMap, "VALIGN")
    in
    fun mkTR (ctx, attrs, cells) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.TR{
		align = getALIGN attrVec,
		valign = getVALIGN attrVec,
		content = cells
	      }
	  end
    end (* local *)

  (**** Elements TH and TD ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["LEFT", "CENTER", "RIGHT"]),
	      ("COLSPAN",	AT_NUMBER),
	      ("HEIGHT",	AT_TEXT),
	      ("NOWRAP",	AT_IMPLICIT),
	      ("ROWSPAN",	AT_NUMBER),
	      ("VALIGN",	AT_NAMES["TOP", "MIDDLE", "BOTTOM", "BASELINE"]),
	      ("WIDTH",		AT_TEXT)
	    ]
      val getALIGN	= getNAMES HTML.HAlign.fromString (attrMap, "ALIGN")
      val getCOLSPAN	= getNUMBER (attrMap, "COLSPAN")
      val getHEIGHT	= getCDATA (attrMap, "HEIGHT")
      val getNOWRAP	= getFlag (attrMap, "NOWRAP")
      val getROWSPAN	= getNUMBER (attrMap, "ROWSPAN")
      val getVALIGN	= getNAMES HTML.CellVAlign.fromString (attrMap, "VALIGN")
      val getWIDTH	= getCDATA (attrMap, "WIDTH")
      fun mkCell (ctx, attrs, cells) = let
	    val attrVec = attrListToVec(ctx, attrMap, attrs)
	    in
	      { align = getALIGN attrVec,
		colspan = getCOLSPAN attrVec,
		height = getHEIGHT attrVec,
		nowrap = getNOWRAP attrVec,
		rowspan = getROWSPAN attrVec,
		valign = getVALIGN attrVec,
		width = getWIDTH attrVec,
		content = cells
	      }
	    end
    in
    fun mkTH arg = HTML.TH(mkCell arg)
    fun mkTD arg = HTML.TD(mkCell arg)
    end (* local *)

  (**** Element A ****)
    local
      val attrMap = mkAttrs [
	      ("HREF",		AT_TEXT),
	      ("NAME",		AT_TEXT),
	      ("REL",		AT_TEXT),
	      ("REV",		AT_TEXT),
	      ("TITLE",		AT_TEXT)
	    ]
      val getHREF	= getCDATA (attrMap, "HREF")
      val getNAME	= getCDATA (attrMap, "NAME")
      val getREL	= getCDATA (attrMap, "REL")
      val getREV	= getCDATA (attrMap, "REV")
      val getTITLE	= getCDATA (attrMap, "TITLE")
    in
    fun mkA (ctx, attrs, contents) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.A{
		name = getNAME attrVec,
		href = getHREF attrVec,
		rel = getREL attrVec,
		rev = getREV attrVec,
		title = getTITLE attrVec,
		content = contents
	      }
	  end
    end (* local *)

  (**** Element IMG ****)
     local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["TOP", "MIDDLE", "BOTTOM", "LEFT", "RIGHT"]),
	      ("ALT",		AT_TEXT),
	      ("BORDER",	AT_TEXT),
	      ("HEIGHT",	AT_TEXT),
	      ("HSPACE",	AT_TEXT),
	      ("ISMAP",		AT_IMPLICIT),
	      ("SRC",		AT_TEXT),
	      ("USEMAP",	AT_TEXT),
	      ("VSPACE",	AT_TEXT),
	      ("WIDTH",		AT_TEXT)
	    ]
      val getALIGN	= getNAMES HTML.IAlign.fromString (attrMap, "ALIGN")
      val getALT	= getCDATA (attrMap, "ALT")
      val getBORDER	= getCDATA (attrMap, "BORDER")
      val getHEIGHT	= getCDATA (attrMap, "HEIGHT")
      val getHSPACE	= getCDATA (attrMap, "HSPACE")
      val getISMAP	= getFlag (attrMap, "ISMAP")
      val getSRC	= require (getCDATA, attrMap, "SRC", "")
      val getUSEMAP	= getCDATA (attrMap, "USEMAP")
      val getVSPACE	= getCDATA (attrMap, "VSPACE")
      val getWIDTH	= getCDATA (attrMap, "WIDTH")
    in
    fun mkIMG (ctx, attrs) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.IMG{
		src = getSRC attrVec,
		alt = getALT attrVec,
		align = getALIGN attrVec,
		height = getHEIGHT attrVec,
		width = getWIDTH attrVec,
		border = getBORDER attrVec,
		hspace = getHSPACE attrVec,
		vspace = getVSPACE attrVec,
		usemap = getUSEMAP attrVec,
		ismap = getISMAP attrVec
	      }
	  end
    end (* local *)

  (**** Element APPLET ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["TOP", "MIDDLE", "BOTTOM", "LEFT", "RIGHT"]),
	      ("ALT",		AT_TEXT),
	      ("CODE",		AT_TEXT),
	      ("CODEBASE",	AT_TEXT),
	      ("HEIGHT",	AT_TEXT),
	      ("HSPACE",	AT_TEXT),
	      ("NAME",		AT_TEXT),
	      ("VSPACE",	AT_TEXT),
	      ("WIDTH",		AT_TEXT)
	    ]
      val getALIGN	= getNAMES HTML.IAlign.fromString (attrMap, "ALIGN")
      val getALT	= getCDATA (attrMap, "ALT")
      val getCODE	= require (getCDATA, attrMap, "CODE", "")
      val getCODEBASE	= getCDATA (attrMap, "CODEBASE")
      val getHEIGHT	= getCDATA (attrMap, "HEIGHT")
      val getHSPACE	= getCDATA (attrMap, "HSPACE")
      val getNAME	= getCDATA (attrMap, "NAME")
      val getVSPACE	= getCDATA (attrMap, "VSPACE")
      val getWIDTH	= getCDATA (attrMap, "WIDTH")
    in
    fun mkAPPLET (ctx, attrs, content) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.APPLET{
		codebase = getCODEBASE attrVec,
		code = getCODE attrVec,
		name = getNAME attrVec,
		alt = getALT attrVec,
		align = getALIGN attrVec,
		height = getHEIGHT attrVec,
		width = getWIDTH attrVec,
		hspace = getHSPACE attrVec,
		vspace = getVSPACE attrVec,
		content = content
	      }
	  end
    end (* local *)

  (**** Element PARAM ****)
    local
      val attrMap = mkAttrs [
	      ("NAME",		AT_TEXT),
	      ("VALUE",		AT_TEXT)
	    ]
      val getNAME	= require (getCDATA, attrMap, "NAME", "")
      val getVALUE	= getCDATA (attrMap, "VALUE")
    in
    fun mkPARAM (ctx, attrs) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.PARAM{
		name = getNAME attrVec,
		value = getVALUE attrVec
	      }
	  end
    end (* local *)

  (**** Element FONT ****)
    local
      val attrMap = mkAttrs [
	      ("COLOR",		AT_TEXT),
	      ("SIZE",		AT_TEXT)
	    ]
      val getCOLOR	= getCDATA (attrMap, "COLOR")
      val getSIZE	= getCDATA (attrMap, "SIZE")
    in
    fun mkFONT (ctx, attrs, content) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.FONT{
		size = getSIZE attrVec,
		color = getCOLOR attrVec,
		content = content
	      }
	  end
    end (* local *)

  (**** Element BASEFONT ****)
    local
      val attrMap = mkAttrs [
	      ("SIZE",		AT_TEXT)
	    ]
      val getSIZE	= getCDATA (attrMap, "SIZE")
    in
    fun mkBASEFONT (ctx, attrs, content) = HTML.BASEFONT{
	    size = getSIZE(attrListToVec(ctx, attrMap, attrs)),
	    content = content
	  }
    end (* local *)

  (**** Element BR ****)
    local
      val attrMap = mkAttrs [
	      ("CLEAR",		AT_NAMES["LEFT", "RIGHT", "ALL", "NONE"])
	    ]
      val getCLEAR = getNAMES HTML.TextFlowCtl.fromString (attrMap, "CLEAR")
    in
    fun mkBR (ctx, attrs) = HTML.BR{
	    clear = getCLEAR(attrListToVec(ctx, attrMap, attrs))
	  }
    end (* local *)

  (**** Element MAP ****)
    local
      val attrMap = mkAttrs [
	      ("NAME",		AT_TEXT)
	    ]
      val getNAME	= getCDATA (attrMap, "NAME")
    in
    fun mkMAP (ctx, attrs, content) = HTML.MAP{
	    name = getNAME (attrListToVec(ctx, attrMap, attrs)),
	    content = content
	  }
    end (* local *)

  (**** Element INPUT ****)
    local
      val attrMap = mkAttrs [
	      ("ALIGN",		AT_NAMES["TOP", "MIDDLE", "BOTTOM", "LEFT", "RIGHT"]),
	      ("CHECKED",	AT_IMPLICIT),
	      ("MAXLENGTH",	AT_NUMBER),
	      ("NAME",		AT_TEXT),
	      ("SIZE",		AT_TEXT),
	      ("SRC",		AT_TEXT),
	      ("TYPE",		AT_NAMES[
				    "TEXT", "PASSWORD", "CHECKBOX",
				    "RADIO", "SUBMIT", "RESET",
				    "FILE", "HIDDEN", "IMAGE"
				  ]),
	      ("VALUE",		AT_TEXT)
	    ]
      val getALIGN	= getNAMES HTML.IAlign.fromString (attrMap, "ALIGN")
      val getCHECKED	= getFlag (attrMap, "CHECKED")
      val getMAXLENGTH	= getNUMBER (attrMap, "MAXLENGTH")
      val getNAME	= getCDATA (attrMap, "NAME")
      val getSIZE	= getCDATA (attrMap, "SIZE")
      val getSRC	= getCDATA (attrMap, "SRC")
      val getTYPE	= getNAMES HTML.InputType.fromString (attrMap, "TYPE")
      val getVALUE	= getCDATA (attrMap, "VALUE")
    in
    fun mkINPUT (ctx, attrs) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.INPUT{
		ty = getTYPE attrVec,
		name = getNAME attrVec,
		value = getVALUE attrVec,
		src = getSRC attrVec,
		checked = getCHECKED attrVec,
		size = getSIZE attrVec,
		maxlength = getMAXLENGTH attrVec,
		align = getALIGN attrVec
	      }
	  end
    end (* local *)

  (**** Element SELECT ****)
    local
      val attrMap = mkAttrs [
	      ("NAME",		AT_TEXT),
	      ("SIZE",		AT_TEXT)
	    ]
      val getNAME	= require (getCDATA, attrMap, "NAME", "")
      val getSIZE	= getNUMBER (attrMap, "SIZE")
    in
    fun mkSELECT (ctx, attrs, contents) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.SELECT{
		name = getNAME attrVec,
		size = getSIZE attrVec,
		content = contents
	      }
	  end
    end (* local *)

  (**** Element TEXTAREA ****)
    local
      val attrMap = mkAttrs [
	      ("NAME",		AT_TEXT),
	      ("ROWS",		AT_NUMBER),
	      ("COLS",		AT_NUMBER)
	    ]
      val getNAME	= require (getCDATA, attrMap, "NAME", "")
      val getROWS	= require (getNUMBER, attrMap, "ROWS", 0)
      val getCOLS	= require (getNUMBER, attrMap, "COLS", 0)
    in
    fun mkTEXTAREA (ctx, attrs, contents) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.TEXTAREA{
		name = getNAME attrVec,
		rows = getROWS attrVec,
		cols = getCOLS attrVec,
		content = contents
	      }
	  end
    end (* local *)

  (**** Element AREA ****)
    local
      val attrMap = mkAttrs [
	      ("ALT",		AT_TEXT),
	      ("COORDS",	AT_TEXT),
	      ("HREF",		AT_TEXT),
	      ("NOHREF",	AT_IMPLICIT),
	      ("SHAPE",		AT_NAMES["RECT", "CIRCLE", "POLY", "DEFAULT"])
	    ]
      val getALT	= require (getCDATA, attrMap, "ALT", "")
      val getCOORDS	= getCDATA (attrMap, "COORDS")
      val getHREF	= getCDATA (attrMap, "HREF")
      val getNOHREF	= getFlag (attrMap, "NOHREF")
      val getSHAPE	= getNAMES HTML.Shape.fromString (attrMap, "SHAPE")
    in
    fun mkAREA (ctx, attrs) = let
	   val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.AREA{
		shape = getSHAPE attrVec,
		coords = getCOORDS attrVec,
		href = getHREF attrVec,
		nohref = getNOHREF attrVec,
		alt = getALT attrVec
	      }
	  end
    end (* local *)

  (**** Element OPTION ****)
    local
      val attrMap = mkAttrs [
	      ("SELECTED",	AT_IMPLICIT),
	      ("VALUE",		AT_TEXT)
	    ]
      val getSELECTED	= getFlag (attrMap, "SELECTED")
      val getVALUE	= getCDATA (attrMap, "VALUE")
    in
    fun mkOPTION (ctx, attrs, contents) = let
	  val attrVec = attrListToVec(ctx, attrMap, attrs)
	  in
	    HTML.OPTION{
		selected = getSELECTED attrVec,
		value = getVALUE attrVec,
		content = contents
	      }
	  end
    end (* local *)

  end

