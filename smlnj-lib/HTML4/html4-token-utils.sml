(* html4-token-utils.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A set of utilities used for working with tokens used in the HTML 4
 * parser.
 *)

structure HTML4TokenUtils = struct

structure H4U = HTML4Utils

(* ____________________________________________________________ *)
(* Attribute handling *)
(* XXX Is this too heavyweight?  It certainly gives us some
   flexibility in the future. *)

structure HTML4AttrParser = HTML4AttrParseFn(HTML4AttrLexer)

fun parseAttrsFromStream inStream = let
      val sourceMap = AntlrStreamPos.mkSourcemap ()
      val lex = HTML4AttrLexer.lex sourceMap
      val stream = HTML4AttrLexer.streamifyInstream inStream
      in
	case HTML4AttrParser.parse lex stream
	 of (SOME result, _, _) => result
	  | _ => []
	(* end case *)
      end

fun parseAttrs inStr = parseAttrsFromStream (TextIO.openString inStr)
(*DEBUG*)handle ex => (print(concat["parseAttrs: \"", String.toString inStr, "\"\n"]); raise ex)

(* ____________________________________________________________ *)
open HTML4Tokens

val strict_tuple_list = [
    ("A", STARTA, SOME ENDA),
    ("ABBR", STARTABBR, SOME ENDABBR),
    ("ACRONYM", STARTACRONYM, SOME ENDACRONYM),
    ("ADDRESS", STARTADDRESS, SOME ENDADDRESS),
    ("AREA", STARTAREA, NONE),
    ("B", STARTB, SOME ENDB),
    ("BASE", STARTBASE, NONE),
    ("BDO", STARTBDO, SOME ENDBDO),
    ("BIG", STARTBIG, SOME ENDBIG),
    ("BLOCKQUOTE", STARTBLOCKQUOTE, SOME ENDBLOCKQUOTE),
    ("BODY", STARTBODY, SOME ENDBODY),
    ("BR", STARTBR, NONE),
    ("BUTTON", STARTBUTTON, SOME ENDBUTTON),
    ("CAPTION", STARTCAPTION, SOME ENDCAPTION),
    ("CITE", STARTCITE, SOME ENDCITE),
    ("CODE", STARTCODE, SOME ENDCODE),
    ("COL", STARTCOL, NONE),
    ("COLGROUP", STARTCOLGROUP, SOME ENDCOLGROUP),
    ("DD", STARTDD, SOME ENDDD),
    ("DEL", STARTDEL, SOME ENDDEL),
    ("DFN", STARTDFN, SOME ENDDFN),
    ("DIV", STARTDIV, SOME ENDDIV),
    ("DL", STARTDL, SOME ENDDL),
    ("DT", STARTDT, SOME ENDDT),
    ("EM", STARTEM, SOME ENDEM),
    ("FIELDSET", STARTFIELDSET, SOME ENDFIELDSET),
    ("FORM", STARTFORM, SOME ENDFORM),
    ("H1", STARTH1, SOME ENDH1),
    ("H2", STARTH2, SOME ENDH2),
    ("H3", STARTH3, SOME ENDH3),
    ("H4", STARTH4, SOME ENDH4),
    ("H5", STARTH5, SOME ENDH5),
    ("H6", STARTH6, SOME ENDH6),
    ("HEAD", STARTHEAD, SOME ENDHEAD),
    ("HR", STARTHR, NONE),
    ("HTML", STARTHTML, SOME ENDHTML),
    ("I", STARTI, SOME ENDI),
    ("IMG", STARTIMG, NONE),
    ("INPUT", STARTINPUT, NONE),
    ("INS", STARTINS, SOME ENDINS),
    ("KBD", STARTKBD, SOME ENDKBD),
    ("LABEL", STARTLABEL, SOME ENDLABEL),
    ("LEGEND", STARTLEGEND, SOME ENDLEGEND),
    ("LI", STARTLI, SOME ENDLI),
    ("LINK", STARTLINK, NONE),
    ("MAP", STARTMAP, SOME ENDMAP),
    ("META", STARTMETA, NONE),
    ("NOSCRIPT", STARTNOSCRIPT, SOME ENDNOSCRIPT),
    ("OBJECT", STARTOBJECT, SOME ENDOBJECT),
    ("OL", STARTOL, SOME ENDOL),
    ("OPTGROUP", STARTOPTGROUP, SOME ENDOPTGROUP),
    ("OPTION", STARTOPTION, SOME ENDOPTION),
    ("P", STARTP, SOME ENDP),
    ("PARAM", STARTPARAM, NONE),
    ("PRE", STARTPRE, SOME ENDPRE),
    ("Q", STARTQ, SOME ENDQ),
    ("SAMP", STARTSAMP, SOME ENDSAMP),
    ("SCRIPT", STARTSCRIPT, SOME ENDSCRIPT),
    ("SELECT", STARTSELECT, SOME ENDSELECT),
    ("SMALL", STARTSMALL, SOME ENDSMALL),
    ("SPAN", STARTSPAN, SOME ENDSPAN),
    ("STRONG", STARTSTRONG, SOME ENDSTRONG),
    ("STYLE", STARTSTYLE, SOME ENDSTYLE),
    ("SUB", STARTSUB, SOME ENDSUB),
    ("SUP", STARTSUP, SOME ENDSUP),
    ("TABLE", STARTTABLE, SOME ENDTABLE),
    ("TBODY", STARTTBODY, SOME ENDTBODY),
    ("TD", STARTTD, SOME ENDTD),
    ("TEXTAREA", STARTTEXTAREA, SOME ENDTEXTAREA),
    ("TFOOT", STARTTFOOT, SOME ENDTFOOT),
    ("TH", STARTTH, SOME ENDTH),
    ("THEAD", STARTTHEAD, SOME ENDTHEAD),
    ("TITLE", STARTTITLE, SOME ENDTITLE),
    ("TR", STARTTR, SOME ENDTR),
    ("TT", STARTTT, SOME ENDTT),
    ("UL", STARTUL, SOME ENDUL),
    ("VAR", STARTVAR, SOME ENDVAR)
]

val loose_tuple_list = [
    ("APPLET", STARTAPPLET, SOME ENDAPPLET),
    ("BASEFONT", STARTBASEFONT, NONE),
    ("CENTER", STARTCENTER, SOME ENDCENTER),
    ("DIR", STARTDIR, SOME ENDDIR),
    ("FONT", STARTFONT, SOME ENDFONT),
    ("IFRAME", STARTIFRAME, SOME ENDIFRAME),
    ("ISINDEX", STARTISINDEX, NONE),
    ("MENU", STARTMENU, SOME ENDMENU),
    ("S", STARTS, SOME ENDS),
    ("STRIKE", STARTSTRIKE, SOME ENDSTRIKE),
    ("U", STARTU, SOME ENDU)
]

val frameset_tuple_list = [
    ("FRAME", STARTFRAME, NONE),
    ("FRAMESET", STARTFRAMESET, SOME ENDFRAMESET),
    ("NOFRAMES", STARTNOFRAMES, SOME ENDNOFRAMES)
]

val endTagNameTest = Char.notContains " \t\r\n>"

fun splitTagStart inStr =
      Substring.splitl endTagNameTest (Substring.full inStr)

fun extractTag str = let
      val (tagNameChs, _) = splitTagStart str
      val tagNameChs = (case CharVectorSlice.getItem tagNameChs
	     of SOME(#"<", r) => (case CharVectorSlice.getItem r
		   of SOME(#"/", r) => r
		    | _ => r
		  (* end case *))
	      | _ => tagNameChs
	    (* end case *))
      in
	Atom.atom (CharVectorSlice.map Char.toUpper tagNameChs)
      end

fun extractAttrs str =
    let 
        val (_, tagRest) = splitTagStart str
        val (tagRest', _) = Substring.splitr (fn c => c = #">") tagRest
    in
        parseAttrs (Substring.string tagRest')
    end

structure AtomMap : ORD_MAP = RedBlackMapFn(struct
                                            type ord_key = Atom.atom
                                            val compare = Atom.compare
                                            end)

fun element_tuple_to_ctor_maps ((tag_name, open_ctor, close_ctor_opt),
                                (open_map, close_map)) =
    let val tag_atom = Atom.atom tag_name
        val open_map' = AtomMap.insert(open_map, tag_atom, open_ctor)
        val close_map' = case close_ctor_opt of
                             NONE => close_map
                           | SOME close_tok => AtomMap.insert(
                                               close_map, tag_atom, close_tok)
    in (open_map', close_map') end

val (strict_open_map, strict_close_map) =
    foldl element_tuple_to_ctor_maps (AtomMap.empty, AtomMap.empty)
          strict_tuple_list

val (loose_open_map, loose_close_map) =
    foldl element_tuple_to_ctor_maps (strict_open_map, strict_close_map)
          loose_tuple_list

val (frameset_open_map, frameset_close_map) =
    foldl element_tuple_to_ctor_maps (strict_open_map, strict_close_map)
          frameset_tuple_list

val open_map_ref = ref strict_open_map

val close_map_ref = ref strict_close_map

fun mkOpenTag payloadStr =
    let val tag_atom = extractTag payloadStr
    in case AtomMap.find(!open_map_ref, tag_atom) of
           NONE => OPENTAG (tag_atom, (payloadStr, extractAttrs payloadStr))
         | SOME ctor => ctor (payloadStr, extractAttrs payloadStr)
    end

fun mkCloseTag payloadStr =
    let val tag_atom = extractTag payloadStr
    in case AtomMap.find(!close_map_ref, tag_atom) of
           NONE => CLOSETAG tag_atom
         | SOME tok => tok
    end

(* ____________________________________________________________ *)
(* Alternative tokToString useful for printing.
 *)

fun payloadToString (_, []) = ""
  | payloadToString (_, attrs as (_ :: _)) =
    " " ^ (HTML4Utils.attrsToStr attrs)

fun tokToString EOF = "EOF"
  | tokToString (OPENTAG (tagname, tagdata)) =
    String.concat ["OPENTAG ", Atom.toString tagname, " ",
                   payloadToString tagdata]
  | tokToString (CLOSETAG tagname) = "CLOSETAG " ^ (Atom.toString tagname)
  | tokToString (DOCTYPE docdata) = docdata
  | tokToString (PCDATA pcdata) = pcdata
  | tokToString (COMMENT comment) = comment
  | tokToString (CHAR_REF refint) = "&#" ^ (IntInf.toString refint) ^ ";"
  | tokToString (ENTITY_REF refatom) = "&" ^ (Atom.toString refatom) ^ ";"
  | tokToString (XML_PROCESSING directive) = "XML DIRECTIVE " ^ directive
  | tokToString (STARTA payload) =
    "<A" ^ (payloadToString payload) ^ ">"
  | tokToString ENDA = "</A>"
  | tokToString (STARTABBR payload) =
    "<ABBR" ^ (payloadToString payload) ^ ">"
  | tokToString ENDABBR = "</ABBR>"
  | tokToString (STARTACRONYM payload) =
    "<ACRONYM" ^ (payloadToString payload) ^ ">"
  | tokToString ENDACRONYM = "</ACRONYM>"
  | tokToString (STARTADDRESS payload) =
    "<ADDRESS" ^ (payloadToString payload) ^ ">"
  | tokToString ENDADDRESS = "</ADDRESS>"
  | tokToString (STARTAREA payload) =
    "<AREA" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTB payload) =
    "<B" ^ (payloadToString payload) ^ ">"
  | tokToString ENDB = "</B>"
  | tokToString (STARTBASE payload) =
    "<BASE" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTBDO payload) =
    "<BDO" ^ (payloadToString payload) ^ ">"
  | tokToString ENDBDO = "</BDO>"
  | tokToString (STARTBIG payload) =
    "<BIG" ^ (payloadToString payload) ^ ">"
  | tokToString ENDBIG = "</BIG>"
  | tokToString (STARTBLOCKQUOTE payload) =
    "<BLOCKQUOTE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDBLOCKQUOTE = "</BLOCKQUOTE>"
  | tokToString (STARTBODY payload) =
    "<BODY" ^ (payloadToString payload) ^ ">"
  | tokToString ENDBODY = "</BODY>"
  | tokToString (STARTBR payload) =
    "<BR" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTBUTTON payload) =
    "<BUTTON" ^ (payloadToString payload) ^ ">"
  | tokToString ENDBUTTON = "</BUTTON>"
  | tokToString (STARTCAPTION payload) =
    "<CAPTION" ^ (payloadToString payload) ^ ">"
  | tokToString ENDCAPTION = "</CAPTION>"
  | tokToString (STARTCITE payload) =
    "<CITE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDCITE = "</CITE>"
  | tokToString (STARTCODE payload) =
    "<CODE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDCODE = "</CODE>"
  | tokToString (STARTCOL payload) =
    "<COL" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTCOLGROUP payload) =
    "<COLGROUP" ^ (payloadToString payload) ^ ">"
  | tokToString ENDCOLGROUP = "</COLGROUP>"
  | tokToString (STARTDD payload) =
    "<DD" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDD = "</DD>"
  | tokToString (STARTDEL payload) =
    "<DEL" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDEL = "</DEL>"
  | tokToString (STARTDFN payload) =
    "<DFN" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDFN = "</DFN>"
  | tokToString (STARTDIV payload) =
    "<DIV" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDIV = "</DIV>"
  | tokToString (STARTDL payload) =
    "<DL" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDL = "</DL>"
  | tokToString (STARTDT payload) =
    "<DT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDT = "</DT>"
  | tokToString (STARTEM payload) =
    "<EM" ^ (payloadToString payload) ^ ">"
  | tokToString ENDEM = "</EM>"
  | tokToString (STARTFIELDSET payload) =
    "<FIELDSET" ^ (payloadToString payload) ^ ">"
  | tokToString ENDFIELDSET = "</FIELDSET>"
  | tokToString (STARTFORM payload) =
    "<FORM" ^ (payloadToString payload) ^ ">"
  | tokToString ENDFORM = "</FORM>"
  | tokToString (STARTH1 payload) =
    "<H1" ^ (payloadToString payload) ^ ">"
  | tokToString ENDH1 = "</H1>"
  | tokToString (STARTH2 payload) =
    "<H2" ^ (payloadToString payload) ^ ">"
  | tokToString ENDH2 = "</H2>"
  | tokToString (STARTH3 payload) =
    "<H3" ^ (payloadToString payload) ^ ">"
  | tokToString ENDH3 = "</H3>"
  | tokToString (STARTH4 payload) =
    "<H4" ^ (payloadToString payload) ^ ">"
  | tokToString ENDH4 = "</H4>"
  | tokToString (STARTH5 payload) =
    "<H5" ^ (payloadToString payload) ^ ">"
  | tokToString ENDH5 = "</H5>"
  | tokToString (STARTH6 payload) =
    "<H6" ^ (payloadToString payload) ^ ">"
  | tokToString ENDH6 = "</H6>"
  | tokToString (STARTHEAD payload) =
    "<HEAD" ^ (payloadToString payload) ^ ">"
  | tokToString ENDHEAD = "</HEAD>"
  | tokToString (STARTHR payload) =
    "<HR" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTHTML payload) =
    "<HTML" ^ (payloadToString payload) ^ ">"
  | tokToString ENDHTML = "</HTML>"
  | tokToString (STARTI payload) =
    "<I" ^ (payloadToString payload) ^ ">"
  | tokToString ENDI = "</I>"
  | tokToString (STARTIMG payload) =
    "<IMG" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTINPUT payload) =
    "<INPUT" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTINS payload) =
    "<INS" ^ (payloadToString payload) ^ ">"
  | tokToString ENDINS = "</INS>"
  | tokToString (STARTKBD payload) =
    "<KBD" ^ (payloadToString payload) ^ ">"
  | tokToString ENDKBD = "</KBD>"
  | tokToString (STARTLABEL payload) =
    "<LABEL" ^ (payloadToString payload) ^ ">"
  | tokToString ENDLABEL = "</LABEL>"
  | tokToString (STARTLEGEND payload) =
    "<LEGEND" ^ (payloadToString payload) ^ ">"
  | tokToString ENDLEGEND = "</LEGEND>"
  | tokToString (STARTLI payload) =
    "<LI" ^ (payloadToString payload) ^ ">"
  | tokToString ENDLI = "</LI>"
  | tokToString (STARTLINK payload) =
    "<LINK" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTMAP payload) =
    "<MAP" ^ (payloadToString payload) ^ ">"
  | tokToString ENDMAP = "</MAP>"
  | tokToString (STARTMETA payload) =
    "<META" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTNOSCRIPT payload) =
    "<NOSCRIPT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDNOSCRIPT = "</NOSCRIPT>"
  | tokToString (STARTOBJECT payload) =
    "<OBJECT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDOBJECT = "</OBJECT>"
  | tokToString (STARTOL payload) =
    "<OL" ^ (payloadToString payload) ^ ">"
  | tokToString ENDOL = "</OL>"
  | tokToString (STARTOPTGROUP payload) =
    "<OPTGROUP" ^ (payloadToString payload) ^ ">"
  | tokToString ENDOPTGROUP = "</OPTGROUP>"
  | tokToString (STARTOPTION payload) =
    "<OPTION" ^ (payloadToString payload) ^ ">"
  | tokToString ENDOPTION = "</OPTION>"
  | tokToString (STARTP payload) =
    "<P" ^ (payloadToString payload) ^ ">"
  | tokToString ENDP = "</P>"
  | tokToString (STARTPARAM payload) =
    "<PARAM" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTPRE payload) =
    "<PRE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDPRE = "</PRE>"
  | tokToString (STARTQ payload) =
    "<Q" ^ (payloadToString payload) ^ ">"
  | tokToString ENDQ = "</Q>"
  | tokToString (STARTSAMP payload) =
    "<SAMP" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSAMP = "</SAMP>"
  | tokToString (STARTSCRIPT payload) =
    "<SCRIPT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSCRIPT = "</SCRIPT>"
  | tokToString (STARTSELECT payload) =
    "<SELECT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSELECT = "</SELECT>"
  | tokToString (STARTSMALL payload) =
    "<SMALL" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSMALL = "</SMALL>"
  | tokToString (STARTSPAN payload) =
    "<SPAN" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSPAN = "</SPAN>"
  | tokToString (STARTSTRONG payload) =
    "<STRONG" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSTRONG = "</STRONG>"
  | tokToString (STARTSTYLE payload) =
    "<STYLE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSTYLE = "</STYLE>"
  | tokToString (STARTSUB payload) =
    "<SUB" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSUB = "</SUB>"
  | tokToString (STARTSUP payload) =
    "<SUP" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSUP = "</SUP>"
  | tokToString (STARTTABLE payload) =
    "<TABLE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTABLE = "</TABLE>"
  | tokToString (STARTTBODY payload) =
    "<TBODY" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTBODY = "</TBODY>"
  | tokToString (STARTTD payload) =
    "<TD" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTD = "</TD>"
  | tokToString (STARTTEXTAREA payload) =
    "<TEXTAREA" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTEXTAREA = "</TEXTAREA>"
  | tokToString (STARTTFOOT payload) =
    "<TFOOT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTFOOT = "</TFOOT>"
  | tokToString (STARTTH payload) =
    "<TH" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTH = "</TH>"
  | tokToString (STARTTHEAD payload) =
    "<THEAD" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTHEAD = "</THEAD>"
  | tokToString (STARTTITLE payload) =
    "<TITLE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTITLE = "</TITLE>"
  | tokToString (STARTTR payload) =
    "<TR" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTR = "</TR>"
  | tokToString (STARTTT payload) =
    "<TT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDTT = "</TT>"
  | tokToString (STARTUL payload) =
    "<UL" ^ (payloadToString payload) ^ ">"
  | tokToString ENDUL = "</UL>"
  | tokToString (STARTVAR payload) =
    "<VAR" ^ (payloadToString payload) ^ ">"
  | tokToString ENDVAR = "</VAR>"
  | tokToString (STARTAPPLET payload) =
    "<APPLET" ^ (payloadToString payload) ^ ">"
  | tokToString ENDAPPLET = "</APPLET>"
  | tokToString (STARTBASEFONT payload) =
    "<BASEFONT" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTCENTER payload) =
    "<CENTER" ^ (payloadToString payload) ^ ">"
  | tokToString ENDCENTER = "</CENTER>"
  | tokToString (STARTDIR payload) =
    "<DIR" ^ (payloadToString payload) ^ ">"
  | tokToString ENDDIR = "</DIR>"
  | tokToString (STARTFONT payload) =
    "<FONT" ^ (payloadToString payload) ^ ">"
  | tokToString ENDFONT = "</FONT>"
  | tokToString (STARTIFRAME payload) =
    "<IFRAME" ^ (payloadToString payload) ^ ">"
  | tokToString ENDIFRAME = "</IFRAME>"
  | tokToString (STARTISINDEX payload) =
    "<ISINDEX" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTMENU payload) =
    "<MENU" ^ (payloadToString payload) ^ ">"
  | tokToString ENDMENU = "</MENU>"
  | tokToString (STARTS payload) =
    "<S" ^ (payloadToString payload) ^ ">"
  | tokToString ENDS = "</S>"
  | tokToString (STARTSTRIKE payload) =
    "<STRIKE" ^ (payloadToString payload) ^ ">"
  | tokToString ENDSTRIKE = "</STRIKE>"
  | tokToString (STARTU payload) =
    "<U" ^ (payloadToString payload) ^ ">"
  | tokToString ENDU = "</U>"
  | tokToString (STARTFRAME payload) =
    "<FRAME" ^ (payloadToString payload) ^ ">"
  | tokToString (STARTFRAMESET payload) =
    "<FRAMESET" ^ (payloadToString payload) ^ ">"
  | tokToString ENDFRAMESET = "</FRAMESET>"
  | tokToString (STARTNOFRAMES payload) =
    "<NOFRAMES" ^ (payloadToString payload) ^ ">"
  | tokToString ENDNOFRAMES = "</NOFRAMES>"

(* ______________________________________________________________________ *)

fun tokGetAttrs (STARTA payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTABBR payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTACRONYM payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTADDRESS payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTAREA payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTB payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBASE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBDO payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBIG payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBLOCKQUOTE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBODY payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBR payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBUTTON payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTCAPTION payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTCITE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTCODE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTCOL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTCOLGROUP payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDD payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDEL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDFN payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDIV payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTEM payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTFIELDSET payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTFORM payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTH1 payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTH2 payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTH3 payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTH4 payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTH5 payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTH6 payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTHEAD payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTHR payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTHTML payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTI payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTIMG payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTINPUT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTINS payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTKBD payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTLABEL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTLEGEND payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTLI payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTLINK payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTMAP payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTMETA payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTNOSCRIPT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTOBJECT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTOL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTOPTGROUP payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTOPTION payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTP payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTPARAM payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTPRE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTQ payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSAMP payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSCRIPT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSELECT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSMALL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSPAN payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSTRONG payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSTYLE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSUB payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSUP payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTABLE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTBODY payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTD payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTEXTAREA payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTFOOT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTH payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTHEAD payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTITLE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTR payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTTT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTUL payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTVAR payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTAPPLET payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTBASEFONT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTCENTER payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTDIR payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTFONT payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTIFRAME payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTISINDEX payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTMENU payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTS payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTSTRIKE payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTU payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTFRAME payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTFRAMESET payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs (STARTNOFRAMES payload) = SOME (H4U.getAttrs payload)
  | tokGetAttrs _ = NONE

end (* HTML4TokenUtils *)

(* ______________________________________________________________________
   End of html4-token-utils.sml
   ______________________________________________________________________ *)
