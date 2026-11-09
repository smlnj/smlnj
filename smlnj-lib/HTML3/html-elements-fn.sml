(* html-elements-fn.sml
 *
 * COPYRIGHT (c) 1996 AT&T REsearch.
 *
 * This module builds element tags for the lexer.
 *)

functor HTMLElementsFn (
    structure Tokens : HTML_TOKENS
    structure Err : HTML_ERROR
    structure HTMLAttrs : HTML_ATTRS
  ) : sig

    structure T : HTML_TOKENS

    type pos = int

    val startTag : string option
	  -> (string * pos * pos) -> (T.svalue, pos) T.token option
    val endTag : string option
	  -> (string * pos * pos) -> (T.svalue, pos) T.token option

  end = struct

    structure T = Tokens
    structure A = HTMLAttrs

    type pos = int

    datatype start_tag
      = WAttrs of ((A.attrs * pos * pos) -> (T.svalue, pos) T.token)
      | WOAttrs of ((pos * pos) -> (T.svalue, pos) T.token) 
    datatype end_tag
      = End of ((pos * pos) -> (T.svalue, pos) T.token)
      | Empty

    val tokenData = [
	    ("A",		WAttrs T.START_A,		End T.END_A),
	    ("ADDRESS",		WOAttrs T.START_ADDRESS,	End T.END_ADDRESS),
	    ("APPLET",		WAttrs T.START_APPLET,		End T.END_APPLET),
	    ("AREA",		WAttrs T.TAG_AREA,		Empty),
	    ("B",		WOAttrs T.START_B,		End T.END_B),
	    ("BASE",		WAttrs T.TAG_BASE,		Empty),
	    ("BASEFONT",	WAttrs T.START_BASEFONT,	End T.END_BASEFONT),
	    ("BIG",		WOAttrs T.START_BIG,		End T.END_BIG),
	    ("BLOCKQUOTE",	WOAttrs T.START_BLOCKQUOTE,	End T.END_BLOCKQUOTE),
	    ("BODY",		WAttrs T.START_BODY,		End T.END_BODY),
	    ("BR",		WAttrs T.TAG_BR,		Empty),
	    ("CAPTION",		WAttrs T.START_CAPTION,		End T.END_CAPTION),
	    ("CENTER",		WOAttrs T.START_CENTER,		End T.END_CENTER),
	    ("CITE",		WOAttrs T.START_CITE,		End T.END_CITE),
	    ("CODE",		WOAttrs T.START_CODE,		End T.END_CODE),
	    ("DD",		WOAttrs T.START_DD,		End T.END_DD),
	    ("DFN",		WOAttrs T.START_DFN,		End T.END_DFN),
	    ("DIR",		WAttrs T.START_DIR,		End T.END_DIR),
	    ("DIV",		WAttrs T.START_DIV,		End T.END_DIV),
	    ("DL",		WAttrs T.START_DL,		End T.END_DL),
	    ("DT",		WOAttrs T.START_DT,		End T.END_DT),
	    ("EM",		WOAttrs T.START_EM,		End T.END_EM),
	    ("FONT",		WAttrs T.START_FONT,		End T.END_FONT),
	    ("FORM",		WAttrs T.START_FORM,		End T.END_FORM),
	    ("H1",		WAttrs T.START_H1,		End T.END_H1),
	    ("H2",		WAttrs T.START_H2,		End T.END_H2),
	    ("H3",		WAttrs T.START_H3,		End T.END_H3),
	    ("H4",		WAttrs T.START_H4,		End T.END_H4),
	    ("H5",		WAttrs T.START_H5,		End T.END_H5),
	    ("H6",		WAttrs T.START_H6,		End T.END_H6),
	    ("HEAD",		WOAttrs T.START_HEAD,		End T.END_HEAD),
	    ("HR",		WAttrs T.TAG_HR,		Empty),
	    ("HTML",		WOAttrs T.START_HTML,		End T.END_HTML),
	    ("I",		WOAttrs T.START_I,		End T.END_I),
	    ("IMG",		WAttrs T.TAG_IMG,		Empty),
	    ("INPUT",		WAttrs T.TAG_INPUT,		Empty),
	    ("ISINDEX",		WAttrs T.TAG_ISINDEX,		Empty),
	    ("KBD",		WOAttrs T.START_KBD,		End T.END_KBD),
	    ("LI",		WAttrs T.START_LI,		End T.END_LI),
	    ("LINK",		WAttrs T.TAG_LINK,		Empty),
	    ("MAP",		WAttrs T.START_MAP,		End T.END_MAP),
	    ("MENU",		WAttrs T.START_MENU,		End T.END_MENU),
	    ("META",		WAttrs T.TAG_META,		Empty),
	    ("OL",		WAttrs T.START_OL,		End T.END_OL),
	    ("OPTION",		WAttrs T.START_OPTION,		End T.END_OPTION),
	    ("P",		WAttrs T.START_P,		End T.END_P),
	    ("PARAM",		WAttrs T.TAG_PARAM,		Empty),
	    ("PRE",		WAttrs T.START_PRE,		End T.END_PRE),
	    ("SAMP",		WOAttrs T.START_SAMP,		End T.END_SAMP),
	    ("SCRIPT",		WOAttrs T.START_SCRIPT,		End T.END_SCRIPT),
	    ("SELECT",		WAttrs T.START_SELECT,		End T.END_SELECT),
	    ("SMALL",		WOAttrs T.START_SMALL,		End T.END_SMALL),
	    ("STRIKE",		WOAttrs T.START_STRIKE,		End T.END_STRIKE),
	    ("STRONG",		WOAttrs T.START_STRONG,		End T.END_STRONG),
	    ("STYLE",		WOAttrs T.START_STYLE,		End T.END_STYLE),
	    ("SUB",		WOAttrs T.START_SUB,		End T.END_SUB),
	    ("SUP",		WOAttrs T.START_SUP,		End T.END_SUP),
	    ("TABLE",		WAttrs T.START_TABLE,		End T.END_TABLE),
	    ("TD",		WAttrs T.START_TD,		End T.END_TD),
	    ("TEXTAREA",	WAttrs T.START_TEXTAREA,	End T.END_TEXTAREA),
	    ("TH",		WAttrs T.START_TH,		End T.END_TH),
	    ("TITLE",		WOAttrs T.START_TITLE,		End T.END_TITLE),
	    ("TR",		WAttrs T.START_TR,		End T.END_TR),
	    ("TT",		WOAttrs T.START_TT,		End T.END_TT),
	    ("U",		WOAttrs T.START_U,		End T.END_U),
	    ("UL",		WAttrs T.START_UL,		End T.END_UL),
	    ("VAR",		WOAttrs T.START_VAR,		End T.END_VAR)
	  ]

    structure HTbl = HashTableFn (struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey = (op = : (string * string) -> bool)
      end)

    val elemTbl = let
	  val tbl = HTbl.mkTable (length tokenData, Fail "HTMLElements")
	  fun ins (tag, startTok, endTok) =
		HTbl.insert tbl (tag, {startT=startTok, endT=endTok})
	  in
	    List.app ins tokenData; tbl
	  end

    structure SS = Substring

    fun canonName name = SS.translate (String.str o Char.toUpper) name

    fun find name = (HTbl.find elemTbl (canonName name))

    val skipWS = SS.dropl Char.isSpace

    fun scanStr (ctx, quoteChar, ss) = let
	  val (str, rest) = SS.splitl (fn c => (c <> quoteChar)) ss
	  in
	    if (SS.isEmpty rest)
	      then (
		Err.lexError ctx "missing close quote for string";
		(A.STRING(SS.string str), rest))
	      else (A.STRING(SS.string str), SS.triml 1 rest)
	  end

  (* scan an attribute value from a substring, returning the value, and
   * the rest of the substring.  Attribute values have one of the following
   * forms:
   *   1) a name token (a sequence of letters, digits, periods, or hyphens).
   *   2) a string literal enclosed in ""
   *   3) a string literal enclosed in ''
   *)
    fun scanAttrVal (ctx, attrName, ss) = let
	  fun isNameChar (#"." | #"-") = true
	    | isNameChar c = (Char.isAlphaNum c)
	  in
	    case SS.getc ss
	     of NONE => (A.IMPLICIT, ss)
	      | (SOME(#"\"", rest)) => scanStr (ctx, #"\"", rest)
	      | (SOME(#"'", rest)) => scanStr (ctx, #"'", rest)
	      | (SOME(c, _)) => let
		(**
		 * Unquoted attributes should be Names, but this is often not
		 * the case, so we terminate them on whitespace or ">".
		 *)
		  val notNameChar = ref false
		  fun isAttrChar c =
			if ((Char.isSpace c) orelse (c = #">"))
			  then false
			else (
			  if isNameChar c then () else notNameChar := true;
			  true)
		  val (value, rest) = SS.splitl isAttrChar ss
		  in
		    if (SS.isEmpty value)
		      then (
			Err.badAttrVal ctx (SS.string attrName, "");
			(A.IMPLICIT, ss))
		      else if (! notNameChar)
			then (
			  Err.unquotedAttrVal ctx (SS.string attrName);
			  (A.STRING(SS.string value), rest))
			else (A.NAME(SS.string value), rest)
		  end
	    (* end case *)
	  end

    fun scanStartTag (ctx, ss) = let
	  val (name, rest) = SS.splitl (not o Char.isSpace) ss
	  fun scanAttrs (rest, attrs) = let
		val rest = skipWS rest
		in
		  case SS.getc rest
		   of NONE => (name, List.rev attrs)
		    | (SOME(#"\"", rest)) => (
			Err.lexError ctx "bogus text in element";
			scanAttrs (#2(scanStr (ctx, #"\"", rest)), attrs))
		    | (SOME(#"'", rest)) => (
			Err.lexError ctx "bogus text in element";
			scanAttrs (#2(scanStr (ctx, #"'", rest)), attrs))
		    | (SOME(c, rest')) =>
			if Char.isAlpha c
			  then let
			    val (aName, rest) = SS.splitl Char.isAlphaNum rest
			    val rest = skipWS rest
			    in
			      case (SS.getc rest)
			       of (SOME(#"=", rest)) => let
				  (* get the attribute value *)
				    val (aVal, rest) =
					  scanAttrVal (ctx, aName, skipWS rest)
				    in
				      scanAttrs (rest, (canonName aName, aVal)::attrs)
				    end
				| _ => scanAttrs (rest,
				    (canonName aName, A.IMPLICIT)::attrs)
			      (* end case *)
			    end
			  else (
			    Err.lexError ctx "bogus character in element";
			    scanAttrs (rest', attrs))
		  (* end case *)
		end
	  in
	    scanAttrs(rest, [])
	  end

    fun startTag file (tag, p1, p2) = let
	  val ctx = {file=file, line=p1}
	  val tag' = SS.triml 1 (SS.trimr 1 (SS.full tag))
	  val (name, attrs) = scanStartTag (ctx, tag')
	  in
	    case (find name, attrs)
	     of (NONE, _) => (Err.badStartTag ctx (SS.string name); NONE)
	      | (SOME{startT=WOAttrs _, ...}, _::_) => (
		  List.app (Err.unknownAttr ctx o #1) attrs; NONE)
	      | (SOME{startT=WOAttrs tag, ...}, []) =>
		  SOME(tag (p1, p2))
	      | (SOME{startT=WAttrs tag, ...}, attrs) =>
		  SOME(tag (attrs, p1, p2))
	    (* end case *)
	  end

    fun endTag file (tag, p1, p2) = let
	  val ctx = {file=file, line=p1}
	  val name = SS.triml 2 (SS.trimr 1 (SS.full tag))
	  in
	    case (find name)
	     of NONE => (Err.badEndTag ctx (SS.string name); NONE)
	      | (SOME{endT=Empty, ...}) => (Err.badEndTag ctx (SS.string name); NONE)
	      | (SOME{endT=End endTok, ...}) => SOME(endTok (p1, p2))
	    (* end case *)
	  end

  end

