(* html4-parser.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Defines the HTML4Parser structure, which defunctorizes the
 * automatically generated parser, defines an additional set of
 * utilities for working with the parser.
 *)

structure HTML4Parser = struct

structure H4 = HTML4
structure H4U = HTML4Utils
structure H4T = HTML4Tokens
structure H4TU = HTML4TokenUtils
structure AtomMap = H4TU.AtomMap

local
    structure TheParser = HTML4ParseFn(HTML4Lexer)
in
open TheParser
end

fun parseStream inStream =
    let
        val sourceMap = AntlrStreamPos.mkSourcemap ()
        val lex = HTML4Lexer.lex sourceMap
        val stream = HTML4Lexer.streamifyInstream inStream
        val (result, _, _) = parse lex stream
    in
        result
    end

exception IllFormedHTMLParseStream of
          H4T.token H4U.parsevisitation H4U.stream * string option

val tokVisitationToString = H4U.visitationToString H4T.toString

val strVisitationToString = H4U.visitationToString (fn x : string => x)

val visitationSimilar = let
    fun tokSimilarToString (tok1, tokStr2) = (H4T.toString tok1) = tokStr2
in H4U.visitationSame tokSimilarToString end

fun expect expectedVisit pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
            handle _ => H4U.VisitT H4T.EOF
        fun expectationError () =
            let val msg = String.concat
                              ["Expected ",
                               strVisitationToString expectedVisit, ", got ",
                               tokVisitationToString pstrmHd, " instead."]
            in IllFormedHTMLParseStream(pstrm, SOME msg) end
    in
        if visitationSimilar(pstrmHd, expectedVisit) then H4U.stream_tl pstrm
        else raise (expectationError())
    end

fun expectEnterNT nt = expect (H4U.EnterNT (Atom.atom nt))

fun expectExitNT nt = expect (H4U.ExitNT (Atom.atom nt))

fun expectVisitT tokStr = expect (H4U.VisitT tokStr)

fun expectEnterNTInDomain ntMap pstrm = let
      val pstrmHd = H4U.stream_hd pstrm
            handle _ => H4U.VisitT H4T.EOF
      fun expectationError () = let
	    val expectedNTs = String.concatWith ", " (map Atom.toString (AtomMap.listKeys ntMap))
	    val msg = String.concat [
                    "Expected entry of one of ", expectedNTs, "; got ",
		    tokVisitationToString pstrmHd, " instead."
		  ]
            in
	      IllFormedHTMLParseStream(pstrm, SOME msg)
	    end
      in
	case pstrmHd
	 of H4U.EnterNT ntAtom =>
	      if AtomMap.inDomain (ntMap, ntAtom)
		then AtomMap.lookup (ntMap, ntAtom)
		else raise (expectationError ())
	  | _ => raise (expectationError ())
        (* end case *)
      end

fun optional optVisit (strm as H4U.StreamCons(strmHd, _)) =
    if visitationSimilar(strmHd, optVisit)
    then (H4U.stream_tl strm, SOME strmHd)
    else (strm, NONE)
  | optional _ _ = (H4U.StreamNil, NONE)

fun optVisitTok tokName strm =
    case optional (H4U.VisitT tokName) strm
     of (strm', SOME (H4U.VisitT tok)) => (strm', SOME tok)
      | _ => (strm, NONE)

fun isEnterNT nt pstrm = (expectEnterNT nt pstrm; true)
    handle IllFormedHTMLParseStream _ => false

fun isExitNT nt pstrm = (expectExitNT nt pstrm; true)
    handle IllFormedHTMLParseStream _ => false

fun isVisitT tokStr pstrm = (expectVisitT tokStr pstrm; true)
    handle IllFormedHTMLParseStream _ => false

fun isEither (is1, is2) pstrm = (is1 pstrm) orelse (is2 pstrm)

fun streamSkipUntil _ H4U.StreamNil = H4U.StreamNil
  | streamSkipUntil pred (strm as H4U.StreamCons (strmHd, _)) =
    if pred strmHd then strm else streamSkipUntil pred (H4U.stream_tl strm)

fun streamSkipWhile pred = streamSkipUntil (fn strmHd => not (pred strmHd))

fun streamConsumeUntil consumer until strm =
    let fun streamConsumeUntil' strm' acc =
            if until strm' then (strm', rev acc)
            else let val (strm'', consumerVal) = consumer strm'
                 in streamConsumeUntil' strm'' (consumerVal :: acc) end
    in streamConsumeUntil' strm [] end

fun tokIsSpace (H4T.PCDATA pcstr) = CharVector.all Char.isSpace pcstr
  | tokIsSpace _ = false

fun tokIsComment (H4T.COMMENT _) = true
  | tokIsComment _ = false

fun visitationIsSpace (H4U.VisitT tok) = tokIsSpace tok
  | visitationIsSpace _ = false

(* XXX I don't like the solution of skipping both whitespace and
comments, but I don't know how to munge CDATA and comments into block
elements, given the current HTML 4 data structure (I could add these,
but it would break the "purity" of the existing data type). *)

fun visitationIsSpaceOrComment (H4U.VisitT tok) = (tokIsSpace tok) orelse
                                                  (tokIsComment tok)
  | visitationIsSpaceOrComment _ = false

val skipWhitespace = streamSkipWhile visitationIsSpace

val skipWhitespaceOrComment = streamSkipWhile visitationIsSpaceOrComment

fun tokIsCdata (H4T.PCDATA _) = true
  | tokIsCdata (H4T.ENTITY_REF _) = true
  | tokIsCdata (H4T.CHAR_REF _) = true
  | tokIsCdata (H4T.COMMENT _) = true
  | tokIsCdata _ = false

fun isNotCdata (H4U.StreamCons(H4U.VisitT tok, _)) = not (tokIsCdata tok)
  | isNotCdata _ = true

exception InvalidToken of H4T.token

fun tokToCdata (H4T.PCDATA str) = H4.PCDATA str
  | tokToCdata (H4T.ENTITY_REF ent) = H4.ENTITY ent
  | tokToCdata (H4T.CHAR_REF chr) = H4.CHAR chr
  | tokToCdata (H4T.COMMENT cmt) = H4.COMMENT cmt
  | tokToCdata tok = raise (InvalidToken tok)

(*+DEBUG*)
fun tokToString (H4T.DOCTYPE doctypeStr) = doctypeStr
  | tokToString (H4T.PCDATA dataStr) = ("PCDATA \"" ^ (String.toString dataStr)
                                        ^ "\"")
  | tokToString (H4T.COMMENT commentStr) = commentStr
  | tokToString tok = H4TU.tokToString tok

fun printVisitationStream strm =
    print ((H4U.visitationToString tokToString (H4U.stream_hd strm)) ^ "\n")

fun printIllFormedErr (IllFormedHTMLParseStream (strm, msgOpt)) = (
      print "Error in parse stream at: ";
      printVisitationStream strm;
      (case msgOpt of SOME msg => print(concat["Message: ", msg, "\n"]) | _ => ()))
  | printIllFormedErr exn = raise exn
(*-DEBUG*)

fun getAttrsFromStream (H4U.StreamCons (H4U.VisitT tok, _)) =
    (case H4TU.tokGetAttrs tok
      of SOME attrs => attrs
       | NONE => [])
  | getAttrsFromStream _ = []

fun html0aryFromParseStream tag ctor pstrm =
    let val pstrm1 = expectEnterNT tag pstrm
        val pstrm2 = expectVisitT ("START" ^ tag) pstrm1
        val attrs = getAttrsFromStream pstrm1
        val pstrm3 = expectExitNT tag (skipWhitespaceOrComment pstrm2)
    in
        (pstrm3, SOME (ctor attrs))
    end

fun listOfOptsToList lst = map Option.valOf lst
(*DEBUG*) handle ex => raise ex

fun htmlNaryFromParseStream tag ctor childFromParseStream pstrm0 =
    let val endTag = "END" ^ tag
        val pstrm1 = expectEnterNT tag pstrm0
        val pstrm2 = expectVisitT ("START" ^ tag) pstrm1
        val attrs = getAttrsFromStream pstrm1
        val (pstrm3, children) =
            streamConsumeUntil childFromParseStream
                               (isEither (isVisitT endTag, isExitNT tag))
                               (skipWhitespaceOrComment pstrm2)
        val (pstrm4, _) = optVisitTok endTag pstrm3
        val pstrm5 = expectExitNT tag (skipWhitespaceOrComment pstrm4)
    in (pstrm5, SOME (ctor (attrs, listOfOptsToList children))) end

type parseVisitStream = H4T.token H4U.parsevisitation H4U.stream

(* FIXME: might as well use AtomTable.hash_table for these, since we are initializing them later *)
val headContentNTMap : (parseVisitStream -> parseVisitStream * H4.head_content option) AtomMap.map ref =
      ref AtomMap.empty

val blockNTMap : (parseVisitStream -> parseVisitStream * H4.block option) AtomMap.map ref =
      ref AtomMap.empty

val inlineNTMap : (parseVisitStream -> parseVisitStream * H4.inline option) AtomMap.map ref =
      ref AtomMap.empty

val tableDataNTMap : (parseVisitStream -> parseVisitStream * H4.table_data option) AtomMap.map ref =
      ref AtomMap.empty

fun cvtBlock ctor (SOME block) = SOME (ctor block)
  | cvtBlock _ NONE = NONE

fun cvtInline ctor (SOME inline) = SOME (ctor inline)
  | cvtInline _ NONE = NONE

fun cvtFlow ctor (SOME flow) = SOME (ctor flow)
  | cvtFlow _ _ = NONE

fun cvtOption ctor (SOME htmlopt) = SOME (ctor htmlopt)
  | cvtOption _ _ = NONE

fun cvtParam ctor (SOME param) = SOME (ctor param)
  | cvtParam _ _ = NONE

fun cvtFrameset ctor (SOME frameset) = SOME (ctor frameset)
  | cvtFrameset _ NONE = NONE

fun cvtScript ctor (SOME script) = SOME (ctor script)
  | cvtScript _ _ = NONE

fun cdataFromParseStream pstrm =
    if isNotCdata pstrm
    then raise (IllFormedHTMLParseStream(pstrm,
                                         SOME "Expected character data."))
    else
        let val pstrmHd = H4U.stream_hd pstrm
            val pstrmTl = H4U.stream_tl pstrm
        in case pstrmHd
            of H4U.VisitT tok => (pstrmTl, SOME (tokToCdata tok))
             | _ => (pstrmTl, NONE)
        end

fun htmlFromParseStream pstrm0 =
    let val pstrm1 =
            (skipWhitespaceOrComment o (expectEnterNT "DOCUMENT")) pstrm0
        val (pstrm2, doctypeTokOpt) = optVisitTok "DOCTYPE" pstrm1
        val theVersion = (case doctypeTokOpt
                           of SOME (H4T.DOCTYPE doctype) => SOME doctype
                            | _ => NONE)
        val (pstrm3, starthtmlTokOpt) =
            optVisitTok "STARTHTML" (skipWhitespaceOrComment pstrm2)
        val (pstrm4, headDataListOpt) = headFromParseStream
                                            (skipWhitespaceOrComment pstrm3)
    in if not (isSome headDataListOpt)
      then (pstrm4, NONE)
      else (case bodyOrFramesetFromParseStream pstrm4
	of (pstrm5, SOME content) => let
	     val (pstrm6, _) = optVisitTok "ENDHTML" pstrm5
	     val pstrm7 = (skipWhitespaceOrComment o
			  (expectExitNT "DOCUMENT") o
			  skipWhitespaceOrComment) pstrm6
	     in (
	       pstrm7,
	       SOME (H4.HTML{
		   version = theVersion,
		   head = [],
		   content = content
		 })
	     ) end
	 | (pstrm5, NONE) => (pstrm5, NONE)
       (* end case *))
    end
and headFromParseStream pstrm0 =
    let val pstrm1 = (skipWhitespaceOrComment o (expectEnterNT "HEAD")) pstrm0
        val (pstrm2, startheadTokOpt) = optVisitTok "STARTHEAD" pstrm1
        val (pstrm3, children) =
            streamConsumeUntil headContentFromParseStream
                               (isEither(isExitNT "HEAD", isVisitT "ENDHEAD"))
                               (skipWhitespaceOrComment pstrm2)
        val (pstrm4, _) = optVisitTok "ENDHEAD" pstrm3
        val pstrm5 = expectExitNT "HEAD" (skipWhitespaceOrComment pstrm4)
    in (pstrm5, SOME (listOfOptsToList children)) end
and headContentFromParseStream pstrm =
    let val ntFunc = expectEnterNTInDomain (!headContentNTMap) pstrm
        val (pstrm', resultOpt) = ntFunc pstrm
    in (skipWhitespaceOrComment pstrm', resultOpt) end
and bodyOrFramesetFromParseStream pstrm =
    let fun cvtBody (SOME body) = SOME (H4.BodyOrFrameset_BODY body)
          | cvtBody _ = NONE
    in
        if isEnterNT "BODY" pstrm
        then let val (pstrm', bodyOpt) = bodyFromParseStream pstrm
             in (pstrm', cvtBody bodyOpt) end
        else let val (pstrm', framesetOpt) = framesetFromParseStream pstrm
             in (pstrm',
                 cvtFrameset H4.BodyOrFrameset_FRAMESET framesetOpt) end
    end
and bodyFromParseStream pstrm0 =
    let val pstrm1 = expectEnterNT "BODY" pstrm0
        val (pstrm2, startbodyTokOpt) = optVisitTok "STARTBODY" pstrm1
        val attrs = (case startbodyTokOpt
	       of SOME startbody => (case H4TU.tokGetAttrs startbody
		     of SOME attrs => attrs
		      | NONE => []
		    (* end case *))
		| NONE => []
	      (* end case *))
        val (pstrm3, children) =
            streamConsumeUntil blockOrScriptFromParseStream
                               (isEither(isExitNT "BODY", isVisitT "ENDBODY"))
                               (skipWhitespaceOrComment pstrm2)
        val (pstrm4, _) = optVisitTok "ENDBODY" pstrm3
        val pstrm5 = expectExitNT "BODY" (skipWhitespaceOrComment pstrm4)
    in (pstrm5, SOME (H4.BODY (attrs, listOfOptsToList children))) end 
and framesetFromParseStream pstrm0 =
    let val pstrm1 = expectEnterNT "FRAMESET" pstrm0
        val pstrm2 = expectVisitT "STARTFRAMESET" pstrm1
        val attrs = getAttrsFromStream pstrm1
        val (pstrm3, children) =
            streamConsumeUntil framesetOrFrameFromParseStream
                               (isEither(isVisitT "ENDFRAMESET",
                                         isEnterNT "NOFRAMES"))
                               (skipWhitespaceOrComment pstrm2)
        val (pstrm4, noframesOpt) =
            if isEnterNT "NOFRAMES" pstrm3 then
                let val (pstrm4', noframesOpt') =
                        noFramesFromParseStream pstrm3
                in (skipWhitespaceOrComment pstrm4', noframesOpt') end
            else (pstrm3, NONE)
        val pstrm5 = expectVisitT "ENDFRAMESET" pstrm4
        val pstrm6 = expectExitNT "FRAMESET" (skipWhitespaceOrComment pstrm5)
    in
        (pstrm6, SOME (H4.FRAMESET (attrs, listOfOptsToList children,
                                    noframesOpt)))
    end
and framesetOrFrameFromParseStream pstrm0 =
    let val pstrm1 = skipWhitespaceOrComment pstrm0
        val (pstrm2, result) =
            if isEnterNT "FRAMESET" pstrm1
            then let val (pstrm', result') = framesetFromParseStream pstrm1
                 in (pstrm',
                     cvtFrameset H4.FramesetOrFrame_FRAMESET result') end
            else html0aryFromParseStream "FRAME" H4.FRAME pstrm1
    in (skipWhitespaceOrComment pstrm2, result) end
and noFramesFromParseStream pstrm0 =
    let val pstrm1 = expectEnterNT "NOFRAMES" pstrm0
        val pstrm2 = expectVisitT "STARTNOFRAMES" pstrm1
        val attrs = getAttrsFromStream pstrm1
        val (pstrm3, bodyOpt) = bodyFromParseStream pstrm2
        val pstrm4 = expectVisitT "ENDNOFRAMES" pstrm3
        val pstrm5 = expectExitNT "NOFRAMES" pstrm4
    in (pstrm5, SOME (H4.NOFRAMES (attrs, valOf bodyOpt)))
(* DEBUG *)handle ex => raise ex
end
and flowFromParseStream pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
        fun procInline pstrm =
            let val (pstrm', result') = inlineFromParseStream pstrm
            in (pstrm', cvtInline H4.Flow_INLINE result') end
    in case pstrmHd
        of H4U.EnterNT ntAtom =>
           if AtomMap.inDomain (!blockNTMap, ntAtom)
           then let val (pstrm', result') = blockFromParseStream pstrm
                in (pstrm', cvtBlock H4.Flow_BLOCK result') end
           else procInline pstrm
         | _ => procInline pstrm
    end
and blockFromParseStream pstrm =
    (expectEnterNTInDomain (!blockNTMap) pstrm) pstrm
and inlineFromParseStream pstrm =
    let val pstrmHd = H4U.stream_hd pstrm
    in case pstrmHd
        of H4U.VisitT tok =>
           let val (pstrm', cdataOptList) =
                   streamConsumeUntil cdataFromParseStream isNotCdata pstrm
           in (pstrm', SOME (H4.CDATA (listOfOptsToList cdataOptList))) end
         | _ => (expectEnterNTInDomain (!inlineNTMap) pstrm) pstrm
    end
and listItemFromParseStream pstrm =
    htmlNaryFromParseStream "LI" H4.LI flowFromParseStream pstrm
and scriptFromParseStream pstrm =
    htmlNaryFromParseStream "SCRIPT" H4.SCRIPT cdataFromParseStream pstrm
and paramFromParseStream pstrm =
    html0aryFromParseStream "PARAM" H4.PARAM pstrm
and legendFromParseStream pstrm =
    htmlNaryFromParseStream "LEGEND" H4.LEGEND inlineFromParseStream pstrm
and defTermOrDescFromParseStream pstrm =
    if isEnterNT "DT" pstrm
    then htmlNaryFromParseStream "DT" H4.DT inlineFromParseStream pstrm
    else htmlNaryFromParseStream "DD" H4.DD flowFromParseStream pstrm
and tableDataFromParseStream pstrm =
    (expectEnterNTInDomain (!tableDataNTMap) pstrm) pstrm
and trFromParseStream pstrm =
    htmlNaryFromParseStream "TR" H4.TR thOrTdFromParseStream pstrm
and thOrTdFromParseStream pstrm =
    if isEnterNT "TH" pstrm
    then htmlNaryFromParseStream "TH" H4.TH flowFromParseStream pstrm
    else htmlNaryFromParseStream "TD" H4.TD flowFromParseStream pstrm
and optgroupOrOptionFromParseStream pstrm =
    if isEnterNT "OPTGROUP" pstrm
      then let
	fun parseOpt pstrm = (case htmlOptionFromParseStream pstrm
	       of (pstrm', SOME(H4.OPTION stuff)) => (pstrm', SOME stuff)
		| (pstrm', _) => (pstrm', NONE)
	      (* end case *))
	in htmlNaryFromParseStream "OPTGROUP" H4.OPTGROUP parseOpt pstrm end
      else htmlOptionFromParseStream pstrm
and htmlOptionFromParseStream pstrm =
    htmlNaryFromParseStream "OPTION" H4.OPTION cdataFromParseStream pstrm
and flowOrParamFromParseStream pstrm =
    if isEnterNT "PARAM" pstrm
    then let val (pstrm', paramOpt) = paramFromParseStream pstrm
         in (pstrm', cvtParam H4.FlowOrParam_PARAM paramOpt) end
    else let val (pstrm', flowOpt) = flowFromParseStream pstrm
         in (pstrm', cvtFlow H4.FlowOrParam_FLOW flowOpt) end
and blockOrScriptFromParseStream pstrm =
    if isEnterNT "SCRIPT" pstrm
    then let val (pstrm', scriptOpt) = scriptFromParseStream pstrm
         in (skipWhitespaceOrComment pstrm',
             cvtScript H4.BlockOrScript_SCRIPT scriptOpt) end
    else let val (pstrm', blockOpt) = blockFromParseStream pstrm
         in (skipWhitespaceOrComment pstrm',
             cvtBlock H4.BlockOrScript_BLOCK blockOpt) end
and blockOrAreaFromParseStream pstrm =
    if isEnterNT "AREA" pstrm
    then html0aryFromParseStream "AREA" H4.AREA pstrm
    else let val (pstrm', blockOpt) = blockFromParseStream pstrm
         in (pstrm', cvtBlock H4.BlockOrArea_BLOCK blockOpt) end
and headObjectFromParseStream pstrm =
    htmlNaryFromParseStream "OBJECT" H4.Head_OBJECT flowOrParamFromParseStream
                            pstrm
and headScriptFromParseStream pstrm =
    let val (pstrm', scriptOpt) = scriptFromParseStream pstrm
    in (pstrm', cvtScript H4.Head_SCRIPT scriptOpt) end

val titleFromParseStream =
    htmlNaryFromParseStream "TITLE" H4.Head_TITLE cdataFromParseStream
val baseFromParseStream = html0aryFromParseStream "BASE" H4.Head_BASE
val metaFromParseStream = html0aryFromParseStream "META" H4.Head_META
val linkFromParseStream = html0aryFromParseStream "LINK" H4.Head_LINK
val pFromParseStream = htmlNaryFromParseStream "P" H4.P inlineFromParseStream
val h1FromParseStream =
    htmlNaryFromParseStream "H1" H4.H1 inlineFromParseStream
val h2FromParseStream =
    htmlNaryFromParseStream "H2" H4.H2 inlineFromParseStream
val h3FromParseStream =
    htmlNaryFromParseStream "H3" H4.H3 inlineFromParseStream
val h4FromParseStream =
    htmlNaryFromParseStream "H4" H4.H4 inlineFromParseStream
val h5FromParseStream =
    htmlNaryFromParseStream "H5" H4.H5 inlineFromParseStream
val h6FromParseStream =
    htmlNaryFromParseStream "H6" H4.H6 inlineFromParseStream
val ulFromParseStream =
    htmlNaryFromParseStream "UL" H4.UL listItemFromParseStream
val olFromParseStream =
    htmlNaryFromParseStream "OL" H4.OL listItemFromParseStream
val dirFromParseStream =
    htmlNaryFromParseStream "DIR" H4.DIR listItemFromParseStream
val menuFromParseStream =
    htmlNaryFromParseStream "MENU" H4.MENU listItemFromParseStream
val preFromParseStream =
    (* XXX This will not properly track whitespace currently. *)
    htmlNaryFromParseStream "PRE" H4.PRE inlineFromParseStream
val dlFromParseStream =
    htmlNaryFromParseStream "DL" H4.DL defTermOrDescFromParseStream
val divFromParseStream =
    htmlNaryFromParseStream "DIV" H4.DIV flowFromParseStream
val noscriptFromParseStream =
    htmlNaryFromParseStream "NOSCRIPT" H4.NOSCRIPT blockFromParseStream
val blockquoteFromParseStream =
    htmlNaryFromParseStream "BLOCKQUOTE" H4.BLOCKQUOTE
                            blockOrScriptFromParseStream
val formFromParseStream =
    htmlNaryFromParseStream "FORM" H4.FORM blockOrScriptFromParseStream
val hrFromParseStream = html0aryFromParseStream "HR" H4.HR
val tableFromParseStream =
    htmlNaryFromParseStream "TABLE" H4.TABLE tableDataFromParseStream
fun fieldsetFromParseStream pstrm0 =
    let val pstrm1 = expectEnterNT "FIELDSET" pstrm0
        val pstrm2 = expectVisitT "STARTFIELDSET" pstrm1
        val attrs = getAttrsFromStream pstrm1
        val (pstrm3, legendOpt) =
            legendFromParseStream (skipWhitespaceOrComment pstrm2)
        val (pstrm4, flows) =
            streamConsumeUntil flowFromParseStream (isVisitT "ENDFIELDSET")
                               pstrm3
        val pstrm5 = expectVisitT "ENDFIELDSET" pstrm4
        val pstrm6 = expectExitNT "FIELDSET" pstrm5
    in (pstrm5, SOME (H4.FIELDSET (attrs, legendOpt,
                                   listOfOptsToList flows))) end
val addressFromParseStream =
    htmlNaryFromParseStream "ADDRESS" H4.ADDRESS inlineFromParseStream
val centerFromParseStream =
    htmlNaryFromParseStream "CENTER" H4.CENTER flowFromParseStream
val isindexFromParseStream = html0aryFromParseStream "ISINDEX" H4.ISINDEX
val ttFromParseStream =
    htmlNaryFromParseStream "TT" H4.TT inlineFromParseStream
val iFromParseStream =
    htmlNaryFromParseStream "I" H4.I inlineFromParseStream
val bFromParseStream =
    htmlNaryFromParseStream "B" H4.B inlineFromParseStream
val bigFromParseStream =
    htmlNaryFromParseStream "BIG" H4.BIG inlineFromParseStream
val smallFromParseStream =
    htmlNaryFromParseStream "SMALL" H4.SMALL inlineFromParseStream
val uFromParseStream =
    htmlNaryFromParseStream "U" H4.U inlineFromParseStream
val sFromParseStream =
    htmlNaryFromParseStream "S" H4.S inlineFromParseStream
val strikeFromParseStream =
    htmlNaryFromParseStream "STRIKE" H4.STRIKE inlineFromParseStream
val emFromParseStream =
    htmlNaryFromParseStream "EM" H4.EM inlineFromParseStream
val strongFromParseStream =
    htmlNaryFromParseStream "STRONG" H4.STRONG inlineFromParseStream
val dfnFromParseStream =
    htmlNaryFromParseStream "DFN" H4.DFN inlineFromParseStream
val codeFromParseStream =
    htmlNaryFromParseStream "CODE" H4.CODE inlineFromParseStream
val sampFromParseStream =
    htmlNaryFromParseStream "SAMP" H4.SAMP inlineFromParseStream
val kbdFromParseStream =
    htmlNaryFromParseStream "KBD" H4.KBD inlineFromParseStream
val varFromParseStream =
    htmlNaryFromParseStream "VAR" H4.VAR inlineFromParseStream
val citeFromParseStream =
    htmlNaryFromParseStream "CITE" H4.CITE inlineFromParseStream
val abbrFromParseStream =
    htmlNaryFromParseStream "ABBR" H4.ABBR inlineFromParseStream
val acronymFromParseStream =
    htmlNaryFromParseStream "ACRONYM" H4.ACRONYM inlineFromParseStream
val aFromParseStream =
    htmlNaryFromParseStream "A" H4.A inlineFromParseStream
val imgFromParseStream =
    html0aryFromParseStream "IMG" H4.IMG
val objectFromParseStream =
    htmlNaryFromParseStream "OBJECT" H4.OBJECT flowOrParamFromParseStream
val brFromParseStream =
    html0aryFromParseStream "BR" H4.BR
fun inlineScriptFromParseStream pstrm =
    let val (pstrm', scriptOpt) = scriptFromParseStream pstrm
    in (pstrm', cvtScript H4.Inline_SCRIPT scriptOpt) end
val mapFromParseStream =
    htmlNaryFromParseStream "MAP" H4.MAP blockOrAreaFromParseStream
val qFromParseStream =
    htmlNaryFromParseStream "Q" H4.Q inlineFromParseStream
val subFromParseStream =
    htmlNaryFromParseStream "SUB" H4.SUB inlineFromParseStream
val supFromParseStream =
    htmlNaryFromParseStream "SUP" H4.SUP inlineFromParseStream
val spanFromParseStream =
    htmlNaryFromParseStream "SPAN" H4.SPAN inlineFromParseStream
val bdoFromParseStream =
    htmlNaryFromParseStream "BDO" H4.BDO inlineFromParseStream
val appletFromParseStream =
    htmlNaryFromParseStream "APPLET" H4.APPLET flowOrParamFromParseStream
val basefontFromParseStream =
    html0aryFromParseStream "BASEFONT" H4.BASEFONT
val fontFromParseStream =
    htmlNaryFromParseStream "FONT" H4.FONT inlineFromParseStream
val iframeFromParseStream =
    htmlNaryFromParseStream "IFRAME" H4.IFRAME flowFromParseStream
val inputFromParseStream =
    html0aryFromParseStream "INPUT" H4.INPUT
val selectFromParseStream =
    htmlNaryFromParseStream "SELECT" H4.SELECT optgroupOrOptionFromParseStream
val textareaFromParseStream =
    htmlNaryFromParseStream "TEXTAREA" H4.TEXTAREA cdataFromParseStream
val labelFromParseStream =
    htmlNaryFromParseStream "LABEL" H4.LABEL inlineFromParseStream
val buttonFromParseStream =
    htmlNaryFromParseStream "BUTTON" H4.BUTTON flowFromParseStream
val captionFromParseStream =
    htmlNaryFromParseStream "CAPTION" H4.CAPTION inlineFromParseStream
val colFromParseStream =
    html0aryFromParseStream "COL" H4.COL
val colgroupFromParseStream =
    let fun consumeCol pstrm =
            let val (pstrm', colOptVal) = colFromParseStream pstrm
                fun cvtCol (SOME (H4.COL attrs)) = SOME attrs
                  | cvtCol _ = NONE
            in (skipWhitespaceOrComment pstrm', cvtCol colOptVal) end
    in htmlNaryFromParseStream "COLGROUP" H4.COLGROUP consumeCol end
val theadFromParseStream =
    htmlNaryFromParseStream "THEAD" H4.THEAD trFromParseStream
val tfootFromParseStream =
    htmlNaryFromParseStream "TFOOT" H4.TFOOT trFromParseStream
val tbodyFromParseStream =
    htmlNaryFromParseStream "TBODY" H4.TBODY trFromParseStream

val _ =
    (headContentNTMap
     := (foldl AtomMap.insert' AtomMap.empty
               [ (Atom.atom "TITLE", titleFromParseStream),
                 (Atom.atom "BASE", baseFromParseStream),
                 (Atom.atom "SCRIPT", headScriptFromParseStream),
                 (Atom.atom "META", metaFromParseStream),
                 (Atom.atom "LINK", linkFromParseStream),
                 (Atom.atom "OBJECT", headObjectFromParseStream)]),
     blockNTMap
     := (foldl AtomMap.insert' AtomMap.empty
               [ (Atom.atom "P", pFromParseStream),
                 (Atom.atom "H1", h1FromParseStream),
                 (Atom.atom "H2", h2FromParseStream),
                 (Atom.atom "H3", h3FromParseStream),
                 (Atom.atom "H4", h4FromParseStream),
                 (Atom.atom "H5", h5FromParseStream),
                 (Atom.atom "H6", h6FromParseStream),
                 (Atom.atom "UL", ulFromParseStream),
                 (Atom.atom "OL", olFromParseStream),
                 (Atom.atom "DIR", dirFromParseStream),
                 (Atom.atom "MENU", menuFromParseStream),
                 (Atom.atom "PRE", preFromParseStream),
                 (Atom.atom "DL", dlFromParseStream),
                 (Atom.atom "DIV", divFromParseStream),
                 (Atom.atom "NOSCRIPT", noscriptFromParseStream),
                 (Atom.atom "BLOCKQUOTE", blockquoteFromParseStream),
                 (Atom.atom "FORM", formFromParseStream),
                 (Atom.atom "HR", hrFromParseStream),
                 (Atom.atom "TABLE", tableFromParseStream),
                 (Atom.atom "FIELDSET", fieldsetFromParseStream),
                 (Atom.atom "ADDRESS", addressFromParseStream),
                 (Atom.atom "ISINDEX", isindexFromParseStream),
                 (Atom.atom "CENTER", centerFromParseStream)]),
     inlineNTMap
     := (foldl AtomMap.insert' AtomMap.empty
               [ (Atom.atom "TT", ttFromParseStream),
                 (Atom.atom "I", iFromParseStream),
                 (Atom.atom "B", bFromParseStream),
                 (Atom.atom "BIG", bigFromParseStream),
                 (Atom.atom "SMALL", smallFromParseStream),
                 (Atom.atom "U", uFromParseStream),
                 (Atom.atom "S", sFromParseStream),
                 (Atom.atom "STRIKE", strikeFromParseStream),
                 (Atom.atom "EM", emFromParseStream),
                 (Atom.atom "STRONG", strongFromParseStream),
                 (Atom.atom "DFN", dfnFromParseStream),
                 (Atom.atom "CODE", codeFromParseStream),
                 (Atom.atom "SAMP", sampFromParseStream),
                 (Atom.atom "KBD", kbdFromParseStream),
                 (Atom.atom "VAR", varFromParseStream),
                 (Atom.atom "CITE", citeFromParseStream),
                 (Atom.atom "ABBR", abbrFromParseStream),
                 (Atom.atom "ACRONYM", acronymFromParseStream),
                 (Atom.atom "A", aFromParseStream),
                 (Atom.atom "IMG", imgFromParseStream),
                 (Atom.atom "OBJECT", objectFromParseStream),
                 (Atom.atom "BR", brFromParseStream),
                 (Atom.atom "SCRIPT", inlineScriptFromParseStream),
                 (Atom.atom "MAP", mapFromParseStream),
                 (Atom.atom "Q", qFromParseStream),
                 (Atom.atom "SUB", subFromParseStream),
                 (Atom.atom "SUP", supFromParseStream),
                 (Atom.atom "SPAN", spanFromParseStream),
                 (Atom.atom "BDO", bdoFromParseStream),
                 (Atom.atom "APPLET", appletFromParseStream),
                 (Atom.atom "BASEFONT", basefontFromParseStream),
                 (Atom.atom "FONT", fontFromParseStream),
                 (Atom.atom "IFRAME", iframeFromParseStream),
                 (Atom.atom "INPUT", inputFromParseStream),
                 (Atom.atom "SELECT", selectFromParseStream),
                 (Atom.atom "TEXTAREA", textareaFromParseStream),
                 (Atom.atom "LABEL", labelFromParseStream),
                 (Atom.atom "BUTTON", buttonFromParseStream)]),
     tableDataNTMap
     := (foldl AtomMap.insert' AtomMap.empty
               [ (Atom.atom "CAPTION", captionFromParseStream),
                 (Atom.atom "COL", colFromParseStream),
                 (Atom.atom "COLGROUP", colgroupFromParseStream),
                 (Atom.atom "THEAD", theadFromParseStream),
                 (Atom.atom "TFOOT", tfootFromParseStream),
                 (Atom.atom "TBODY", tbodyFromParseStream)])
    )

fun fromParseTree pt =
    let val (_, result) =
            htmlFromParseStream (H4U.parsetreeToVisitationStream pt)
    in result end

fun fromString str = let
    val pt_opt = parseStream (TextIO.openString str)
in case pt_opt
    of NONE => NONE 
     | SOME pt => fromParseTree pt
end

end (* HTML4ParserUtils *)

(* ______________________________________________________________________
   End of html4-parser.sml
   ______________________________________________________________________ *)
