(* xml-parser-fn.sml
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ http://www.smlnj.org)
 * All rights reserved.
 *)

signature XML_PARSER =
  sig

    structure XMLTree : XML_TREE

    val parseFile : string -> XMLTree.tree

    exception ParseError of string

  end

functor XMLParserFn (XT : XML_TREE) : XML_PARSER =
  struct

    structure XMLTree = XT
    structure XS = XT.Schema
    structure Tok = XMLTokens

  (***** Error messages *****)

    exception ParseError of string

    datatype error_tag
      = S of string
      | TK of Tok.token
      | E of XT.Schema.element

    fun error msg = let
	  fun cvt (S s, l) = s :: l
	    | cvt (TK tok, l) = XMLTokens.toString tok :: l
	    | cvt (E elem, l) = XS.toString elem :: l
	  in
	    raise ParseError(String.concat(List.foldr cvt [] msg))
	  end

  (***** Token streams wrap the ML-ULex generated lexer *****
   *
   * We cache tokens to avoid rescanning the source.
   *)

    type lexer_state = XMLLexer.prestrm * XMLLexer.yystart_state

    datatype token_strm_rep
      = TOK of {tok : Tok.token, span : XMLLexer.span, more : token_strm}
      | MORE of {
	  state : lexer_state,
	  get : lexer_state -> Tok.token * XMLLexer.span * lexer_state
	}

    withtype token_strm = token_strm_rep ref

    fun newTokenStrm (initialState, lexFn) =
	  ref(MORE{state = initialState, get=lexFn})

    fun nextTok (ref(TOK{tok, span, more})) = (tok, span, more)
      | nextTok (strm as ref(MORE{state, get})) = let
	  val (tok, span, state) = get state
	  val more = ref(MORE{state=state, get=get})
	  val rep = TOK{tok=tok, span=span, more=more}
	  in
	    strm := rep; (* cache lexer result *)
	    (tok, span, more)
	  end

  (* skip whitespace and comments *)
    fun skipWS tokStrm = (case nextTok tokStrm
	   of (Tok.WS _, _, tokStrm) => skipWS tokStrm
	    | (Tok.COM _, _, tokStrm) => skipWS tokStrm
	    | _ => tokStrm
	  (* end case *))

  (****** Tracking the content of an element *****)

    type content = XT.content list

    type state = {
	content : content,	(* parsed content in reverse order *)
	preWS : string option	(* preceeding WS when we are not preserving whitespace *)
      }

(* FIXME: this function doesn't seem right *)
    fun mergeWS (NONE, content) = content
      | mergeWS (SOME ws, XT.TEXT txt :: content) = XT.TEXT(txt ^ ws) :: content
      | mergeWS (SOME s, content) = XT.TEXT s :: content

    fun addElem ({content, preWS}, elem) =
	  {content = elem :: mergeWS (preWS, content), preWS = NONE}

    fun addWS ({content, preWS}, ws) = (case preWS
	   of SOME ws' => {content = content, preWS = SOME(ws' ^ ws)}
	    | NONE => {content = content, preWS = SOME ws}
	  (* end case *))

    fun addCom (state, com) = state (* FIXME*)

    fun addText ({content, preWS}, txt) = let
	  val content = (case (preWS, content)
		 of (NONE, XT.TEXT txt' :: content) => XT.TEXT(txt' ^ txt) :: content
		  | (NONE, content) => XT.TEXT txt :: content
		  | (SOME ws, XT.TEXT txt' :: content) => XT.TEXT(concat[txt', ws, txt]) :: content
		  | (SOME ws, content) => XT.TEXT(txt ^ ws) :: content
		(* end case *))
	  in
	    {content = content, preWS = NONE}
	  end

    fun addCData ({content, preWS}, cdata) =
	  {content = XT.CDATA cdata :: mergeWS (preWS, content), preWS = NONE}

    fun finish ({content, preWS} : state) = List.rev content

  (***** Parsing *****)

    fun parser (name, inStrm) = let
	  val srcMap = AntlrStreamPos.mkSourcemap' name
	  fun err (span, msg) =
		error(S "Error [" :: S(AntlrStreamPos.spanToString srcMap span) :: S "]: " :: msg)
	(* scan an element identifier *)
	  fun getElementId tokStrm = (case nextTok tokStrm
		 of (Tok.ID id, span, tokStrm) => (case XS.element id
		       of SOME elem => (elem, tokStrm)
			| NONE => err(span, [S "unrecognized element ", S id])
		      (* end case *))
		  | (tok, span, _) => err(span, [S "expected identifier, but found ", TK tok])
		(* end case *))
	(* parse the attributes of a start tag.  We expect: (ID "=" LIT)* *)
	  fun parseAttributes tokStrm = let
		fun parseAttr (tokStrm, attrs) = (case nextTok tokStrm
		       of (Tok.ID id, _, tokStrm) => (case nextTok tokStrm
			     of (Tok.SYM_EQ, _, tokStrm) => (case nextTok tokStrm
				   of (Tok.LIT v, _, tokStrm) =>
					parseAttr (tokStrm, XS.attribute(id, v)::attrs)
				    | (tok, span, _) => err(span, [S "expected attribute value, but found ", TK tok])
				  (* end case *))
			      | (tok, span, _) => err(span, [S "expected \"=\", but found ", TK tok])
			    (* end case *))
			| _ => (List.rev attrs, tokStrm)
		      (* end case *))
		in
		  parseAttr (tokStrm, [])
		end
	(* parse an element.  We assume that the initial "<" has been consumed. *)
	  fun parseElement (tokStrm, preserveWS) = let
		val (elem, tokStrm) = getElementId tokStrm
		val (attrs, tokStrm) = parseAttributes tokStrm
		in
		  case (nextTok tokStrm)
		   of (Tok.CLOSE_TAG, _, tokStrm) => let
			val preserveWS = preserveWS orelse XS.preserveWS elem
			val (content, tokStrm) = parseContent (tokStrm, preserveWS, XS.preserveComment elem)
			in
			  (* here we expect to see the matching close tag for the element *)
			  case nextTok tokStrm
			   of (Tok.OPEN_END_TAG, span, tokStrm) => let
				val (elem', tokStrm) = getElementId tokStrm
				in
				  if XS.same(elem, elem')
				    then (case nextTok tokStrm
				       of (Tok.CLOSE_TAG, _, tokStrm) =>
					    (XT.ELEMENT{name=elem, attrs=attrs, content=content}, tokStrm)
					| (tok, span, _) => err (span, [
					      S "expected \">\", but found ", TK tok
					    ])
				      (* end case *))
				    else err (span, [
					S "mismatched close tag: expected ", E elem, S ", but found ", E elem'
				      ])
				end
			    | (tok, span, _) => err(span, [
				  S "impossible: unexpected ", TK tok,
				  S " when </", S(XS.toString elem), S "> expected"
				])
			  (* end case *)
			end
		    | (Tok.CLOSE_EMPTY_TAG, _, tokStrm) =>
			(XT.ELEMENT{name=elem, attrs=attrs, content=[]}, tokStrm)
		    | (tok, span, _) => err(span, [S "expected \">\" or \"/>\", but found ", TK tok])
		  (* end case *)
		end
	(* parse the content of an element; we return when we  *)
	  and parseContent (tokStrm, preserveWS, preserveCom) : (XT.content list * token_strm) = let
		fun parse (tokStrm, state) = (case nextTok tokStrm
		       of (Tok.EOF, _, _) => (finish state, tokStrm)
			| (Tok.OPEN_START_TAG, _, tokStrm) => let
			    val (elem, tokStrm) = parseElement (tokStrm, preserveWS)
			    in
			      parse (tokStrm, addElem(state, elem))
			    end
			| (Tok.OPEN_END_TAG, _, _) => (finish state, tokStrm)
			| (Tok.WS s, _, tokStrm) =>
			    if preserveWS
			      then parse (tokStrm, addText(state, s))
			      else parse (tokStrm, addWS(state, s))
			| (Tok.TEXT s, _, tokStrm) => parse (tokStrm, addText(state, s))
			| (Tok.COM s, _, tokStrm) =>
			    if preserveCom
			      then parse (tokStrm, addCom(state, s))
			      else parse (tokStrm, state)
			| (Tok.CDATA s, _, tokStrm) => parse (tokStrm, addCData(state, s))
			| (tok, span, _) => err(span, [S "impossible: unexpected ", TK tok])
		      (* end case *))
		in
		  parse (tokStrm, {preWS=NONE, content=[]})
		end
	(* expect: Attributes "?>" *)
	  and parseXMLDecl tokStrm = let
		val (attrs, tokStrm) = parseAttributes tokStrm
		in
		  case nextTok tokStrm
		   of (Tok.CLOSE_PI_TAG, _, tokStrm) => (SOME attrs, tokStrm)
		    | (tok, span, _) => err(span, [S "expected \"?>\", but found ", TK tok])
		  (* end case *)
		end
	(* expect: ID (S ExternalID)? S? '>'
	 * where
	 *	ExternalID ::= 'SYSTEM' LIT
	 *	            |  'PUBLIC' LIT LIT
	 *)
	  fun parseDOCTYPE tokStrm = let
		val (id, tokStrm) = (case nextTok tokStrm
		       of (Tok.ID id, _, tokStrm) => (id, tokStrm)
			| (tok, span, _) => err(span, [S "expected identifier, but found ", TK tok])
		      (* end case *))
		fun getLiteral tokStrm = (case nextTok tokStrm
		       of (Tok.LIT lit, _, tokStrm) => (lit, tokStrm)
			| (tok, span, _) => err (span, [S "expected literal, but found ", TK tok])
		      (* end case *))
		val (external, tokStrm) = (case nextTok tokStrm
		       of (Tok.SYSTEM, _, tokStrm) => let
			    val (lit, tokStrm) = getLiteral tokStrm
			    in
			      (SOME(XT.SYSTEM lit), tokStrm)
			    end
			| (Tok.PUBLIC, _, tokStrm) => let
			    val (lit1, tokStrm) = getLiteral tokStrm
			    val (lit2, tokStrm) = getLiteral tokStrm
			    in
			      (SOME(XT.PUBLIC(lit1, lit2)), tokStrm)
			    end
			| _ => (NONE, tokStrm)
		      (* end case *))
		in
		(* expect ">" *)
		  case nextTok tokStrm
		   of (Tok.CLOSE_TAG, _, tokStrm) => (SOME(XT.DOCTYPE(id, external)), tokStrm)
		    | (tok, span, tokStrm) => err(span, [S "expected \">\", but found ", TK tok])
		  (* end case *)
		end
	(* initialize the token stream *)
	  val tokStrm = newTokenStrm (
		XMLLexer.streamifyInstream inStrm,
		XMLLexer.lex srcMap (fn (pos, msg) => err((pos, pos), List.map S msg)))
	(* parse the XML Decl (if any) *)
	  val (xmlDecl, tokStrm) = (case nextTok (skipWS tokStrm)
		 of (Tok.OPEN_XML_TAG, _, tokStrm) => parseXMLDecl tokStrm
		  | _ => (NONE, tokStrm)
		(* end case *))
	(* parse the DOCTYPE (if any) *)
	  val (doctype, tokStrm) = (case nextTok (skipWS tokStrm)
		 of (Tok.OPEN_DOCTYPE, _, tokStrm) => parseDOCTYPE tokStrm
		  | _ => (NONE, tokStrm)
		(* end case *))
(* QUESTION: should we preserve comments at top-level by default? *)
	  val (body, _) = parseContent (skipWS tokStrm, false, false)
	  in
	    case body
	     of [] => error [S "empty document"]
	      | [elem as XT.ELEMENT _] => {
		    xmlDecl = xmlDecl,
		    doctype = doctype,
		    content = elem
		  }
	      | _ => error [S "body of document is not a single element"]
	    (* end case *)
	  end (* parser *)

(*
	(* parse XMLDecl? Content *)
	  and parse tokStrm = let
		fun parse tokStrm = (case nextTok tokStrm
		       of (EOF, _) => {xmlDecl = xmlDecl, content = TEXT ""}
			| (Tok.OPEN_START_TAG, tokStrm) => let
			    val finalState = parseStartTag (tokStrm, content, stk)
			    in
			      {xmlDecl = xmlDecl, content = ??}
			    end
			| Tok.WS _ => parse tokStrm
			| tok, _) => err(?, [S "impossible: unexpected ", TK tok])
		      (* end case *))
		in
		  parse tokStrm before close tokStrm
		end
*)

    fun parseFile file = let
	  val inStrm = TextIO.openIn file
	  val tree = parser (file, inStrm)
		handle ex => (TextIO.closeIn inStrm; raise ex)
	  in
	    TextIO.closeIn inStrm;
	    tree
	  end

  end
