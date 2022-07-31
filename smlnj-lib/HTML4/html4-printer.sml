(* html4-printer.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HTML4Printer : sig

    val toString : HTML4.html -> string

end = struct

structure H4 = HTML4

structure PP = PrettyPrint

exception NotImplemented

structure HTML4TagStrings = struct
    (* Strict *)
    val A = "A"
    val ABBR = "ABBR"
    val ACRONYM = "ACRONYM"
    val ADDRESS = "ADDRESS"
    val AREA = "AREA"
    val B = "B"
    val BASE = "BASE"
    val BDO = "BDO"
    val BIG = "BIG"
    val BLOCKQUOTE = "BLOCKQUOTE"
    val BODY = "BODY"
    val BR = "BR"
    val BUTTON = "BUTTON"
    val CAPTION = "CAPTION"
    val CITE = "CITE"
    val CODE = "CODE"
    val COL = "COL"
    val COLGROUP = "COLGROUP"
    val DD = "DD"
    val DEL = "DEL"
    val DFN = "DFN"
    val DIV = "DIV"
    val DL = "DL"
    val DT = "DT"
    val EM = "EM"
    val FIELDSET = "FIELDSET"
    val FORM = "FORM"
    val H1 = "H1"
    val H2 = "H2"
    val H3 = "H3"
    val H4 = "H4"
    val H5 = "H5"
    val H6 = "H6"
    val HEAD = "HEAD"
    val HR = "HR"
    val HTML = "HTML"
    val I = "I"
    val IMG = "IMG"
    val INPUT = "INPUT"
    val INS = "INS"
    val KBD = "KBD"
    val LABEL = "LABEL"
    val LEGEND = "LEGEND"
    val LI = "LI"
    val LINK = "LINK"
    val MAP = "MAP"
    val META = "META"
    val NOSCRIPT = "NOSCRIPT"
    val OBJECT = "OBJECT"
    val OL = "OL"
    val OPTGROUP = "OPTGROUP"
    val OPTION = "OPTION"
    val P = "P"
    val PARAM = "PARAM"
    val PRE = "PRE"
    val Q = "Q"
    val SAMP = "SAMP"
    val SCRIPT = "SCRIPT"
    val SELECT = "SELECT"
    val SMALL = "SMALL"
    val SPAN = "SPAN"
    val STRONG = "STRONG"
    val STYLE = "STYLE"
    val SUB = "SUB"
    val SUP = "SUP"
    val TABLE = "TABLE"
    val TBODY = "TBODY"
    val TD = "TD"
    val TEXTAREA = "TEXTAREA"
    val TFOOT = "TFOOT"
    val TH = "TH"
    val THEAD = "THEAD"
    val TITLE = "TITLE"
    val TR = "TR"
    val TT = "TT"
    val UL = "UL"
    val VAR = "VAR"
    (* Frameset *)
    val FRAME = "FRAME"
    val FRAMESET = "FRAMESET"
    val NOFRAMES = "NOFRAMES"
    (* Loose *)
    val APPLET = "APPLET"
    val BASEFONT = "BASEFONT"
    val CENTER = "CENTER"
    val DIR = "DIR"
    val FONT = "FONT"
    val IFRAME = "IFRAME"
    val ISINDEX = "ISINDEX"
    val MENU = "MENU"
    val S = "S"
    val STRIKE = "STRIKE"
    val U = "U"
end

structure S = HTML4TagStrings

val strictStr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"

val looseStr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"

val framesetStr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">"

val xhtmlStr = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

(* ____________________________________________________________ *)

fun isStrict _ = true

fun getVersionStr (doc as H4.HTML {head, content, ...}) =
    case content of
        H4.BodyOrFrameset_BODY (H4.BODY (attrs, children)) =>
        if isStrict children then strictStr else looseStr
      | H4.BodyOrFrameset_FRAMESET _ => framesetStr

(* ____________________________________________________________ *)

fun ppOpenTag ppstrm (tag, attributes) =
    PP.string ppstrm (String.concat
                      ["<", tag,
                       case attributes of
                           attrs as _::_ => " " ^ (HTML4Utils.attrsToStr attrs)
                         | [] => "",
                       ">"])

fun ppCloseTag ppstrm tag =
    PP.string ppstrm (String.concat ["</", tag, ">"])

fun ppChildren _ _ [] = ()
  | ppChildren ppstrm ppChild children = (
    PP.openHVBox ppstrm (PP.Abs 2);
    PP.newline ppstrm;
    app (ppChild ppstrm) children;
    PP.closeBox ppstrm;
    PP.newline ppstrm
    )

fun ppTagAndChildren ppstrm ppChild tag (attrs, children) = (
    ppOpenTag ppstrm (tag, attrs);
    ppChildren ppstrm ppChild children;
    ppCloseTag ppstrm tag
    )

(* ____________________________________________________________ *)

local
    open H4
in
    fun ppCol ppstrm attrs = ppOpenTag ppstrm (S.COL, attrs)

    fun ppCdata ppstrm (CHAR chNum) =
        PP.string ppstrm ("&#" ^ (IntInf.toString chNum) ^ ";")
      | ppCdata ppstrm (COMMENT comment_string) =
        PP.string ppstrm comment_string
      | ppCdata ppstrm (ENTITY ent) =
        PP.string ppstrm ("&" ^ (Atom.toString ent) ^ ";")
      | ppCdata ppstrm (PCDATA string_data) = PP.string ppstrm string_data
    and ppHtml ppstrm (doc as HTML {version, head, content}) = (
        PP.string ppstrm (case version of
                              SOME doctype_str => doctype_str
                            | NONE => getVersionStr doc);
        PP.newline ppstrm;
        ppOpenTag ppstrm (S.HTML, []);
        PP.openHVBox ppstrm (PP.Abs 2);
        PP.newline ppstrm;
        ppOpenTag ppstrm (S.HEAD, []);
        ppChildren ppstrm ppHead_content head;
        ppCloseTag ppstrm S.HEAD;
        PP.newline ppstrm;
        ppBody_or_frameset ppstrm content;
        PP.closeBox ppstrm;
        PP.newline ppstrm;
        ppCloseTag ppstrm S.HTML;
        PP.newline ppstrm
    )
    and ppHead_content ppstrm (Head_BASE attrs) =
        ppOpenTag ppstrm (S.BASE, attrs)
      | ppHead_content ppstrm (Head_LINK attrs) =
        ppOpenTag ppstrm (S.LINK, attrs)
      | ppHead_content ppstrm (Head_META attrs) =
        ppOpenTag ppstrm (S.META, attrs)
      | ppHead_content ppstrm (Head_OBJECT contents) =
        ppTagAndChildren ppstrm ppFlow_or_param S.OBJECT contents
      | ppHead_content ppstrm (Head_SCRIPT child) = ppScript ppstrm child
      | ppHead_content ppstrm (Head_STYLE contents) =
        ppTagAndChildren ppstrm ppCdata S.STYLE contents
      | ppHead_content ppstrm (Head_TITLE contents) =
        ppTagAndChildren ppstrm ppCdata S.TITLE contents
    and ppBody_or_frameset ppstrm (BodyOrFrameset_BODY body) =
        ppBody ppstrm body
      | ppBody_or_frameset ppstrm (BodyOrFrameset_FRAMESET frameset) =
        ppFrameset ppstrm frameset
    and ppBody ppstrm (BODY content) =
        ppTagAndChildren ppstrm ppBlock_or_script S.BODY content
    and ppFrameset ppstrm (FRAMESET (attrs, children, noframesOpt)) = (
        ppOpenTag ppstrm (S.FRAMESET, attrs);
        ppChildren ppstrm ppFrameset_or_frame children;
        case noframesOpt of
            SOME noframes => (PP.newline ppstrm;
                              ppNoframes ppstrm noframes)
          | _ => ();
        ppCloseTag ppstrm S.FRAMESET
    )
    and ppFrameset_or_frame ppstrm (FRAME attrs) =
        ppOpenTag ppstrm (S.FRAME, attrs)
      | ppFrameset_or_frame ppstrm (FramesetOrFrame_FRAMESET frameset) =
        ppFrameset ppstrm frameset
    and ppNoframes ppstrm (NOFRAMES (attrs, body)) =
        (ppOpenTag ppstrm (S.NOFRAMES, attrs);
         ppBody ppstrm body;
         ppCloseTag ppstrm S.NOFRAMES)
    and ppFlow ppstrm (Flow_BLOCK block) = ppBlock ppstrm block
      | ppFlow ppstrm (Flow_INLINE inline) = ppInline ppstrm inline
    and ppBlock ppstrm (ADDRESS content) =
        ppTagAndChildren ppstrm ppInline S.ADDRESS content
      | ppBlock ppstrm (BLOCKQUOTE content) =
        ppTagAndChildren ppstrm ppBlock_or_script S.BLOCKQUOTE content
      | ppBlock ppstrm (CENTER content) =
        ppTagAndChildren ppstrm ppFlow S.CENTER content
      | ppBlock ppstrm (DIR content) =
        ppTagAndChildren ppstrm ppList_item S.DIR content
      | ppBlock ppstrm (DIV content) =
        ppTagAndChildren ppstrm ppFlow S.DIV content
      | ppBlock ppstrm (DL content) =
        ppTagAndChildren ppstrm ppDef_term_or_desc S.DL content
      | ppBlock ppstrm (FIELDSET (attrs, legend_opt, children)) = (
        ppOpenTag ppstrm (S.FIELDSET, attrs);
        case legend_opt of SOME legend => ppLegend ppstrm legend | NONE => ();
        ppChildren ppstrm ppFlow children;
        ppCloseTag ppstrm S.FIELDSET
        )
      | ppBlock ppstrm (FORM content) =
        ppTagAndChildren ppstrm ppBlock_or_script S.FORM content
      | ppBlock ppstrm (H1 content) =
        ppTagAndChildren ppstrm ppInline S.H1 content
      | ppBlock ppstrm (H2 content) =
        ppTagAndChildren ppstrm ppInline S.H2 content
      | ppBlock ppstrm (H3 content) =
        ppTagAndChildren ppstrm ppInline S.H3 content
      | ppBlock ppstrm (H4 content) =
        ppTagAndChildren ppstrm ppInline S.H4 content
      | ppBlock ppstrm (H5 content) =
        ppTagAndChildren ppstrm ppInline S.H5 content
      | ppBlock ppstrm (H6 content) =
        ppTagAndChildren ppstrm ppInline S.H6 content
      | ppBlock ppstrm (HR attrs) = ppOpenTag ppstrm (S.HR, attrs)
      | ppBlock ppstrm (ISINDEX attrs) = ppOpenTag ppstrm (S.ISINDEX, attrs)
      | ppBlock ppstrm (MENU content) =
        ppTagAndChildren ppstrm ppList_item S.MENU content
      | ppBlock ppstrm (NOSCRIPT content) =
        ppTagAndChildren ppstrm ppBlock S.NOSCRIPT content
      | ppBlock ppstrm (OL content) =
        ppTagAndChildren ppstrm ppList_item S.OL content
      | ppBlock ppstrm (P content) =
        ppTagAndChildren ppstrm ppInline S.P content
      | ppBlock ppstrm (PRE content) =
        ppTagAndChildren ppstrm ppInline S.PRE content
      | ppBlock ppstrm (TABLE content) =
        ppTagAndChildren ppstrm ppTable_data S.TABLE content
      | ppBlock ppstrm (UL content) =
        ppTagAndChildren ppstrm ppList_item S.UL content
    and ppInline ppstrm (A content) =
        ppTagAndChildren ppstrm ppInline S.A content
      | ppInline ppstrm (ABBR content) =
        ppTagAndChildren ppstrm ppInline S.ABBR content
      | ppInline ppstrm (ACRONYM content) =
        ppTagAndChildren ppstrm ppInline S.ACRONYM content
      | ppInline ppstrm (APPLET content) =
        ppTagAndChildren ppstrm ppFlow_or_param S.APPLET content
      | ppInline ppstrm (B content) =
        ppTagAndChildren ppstrm ppInline S.B content
      | ppInline ppstrm (BASEFONT attrs) = ppOpenTag ppstrm (S.BASEFONT, attrs)
      | ppInline ppstrm (BDO content) =
        ppTagAndChildren ppstrm ppInline S.BDO content
      | ppInline ppstrm (BIG content) =
        ppTagAndChildren ppstrm ppInline S.BIG content
      | ppInline ppstrm (BR attrs) = ppOpenTag ppstrm (S.BR, attrs)
      | ppInline ppstrm (BUTTON content) =
        ppTagAndChildren ppstrm ppFlow S.BUTTON content
      | ppInline ppstrm (CDATA children) = app (ppCdata ppstrm) children
      | ppInline ppstrm (CITE content) =
        ppTagAndChildren ppstrm ppInline S.CITE content
      | ppInline ppstrm (CODE content) =
        ppTagAndChildren ppstrm ppInline S.CODE content
      | ppInline ppstrm (DFN content) =
        ppTagAndChildren ppstrm ppInline S.DFN content
      | ppInline ppstrm (EM content) =
        ppTagAndChildren ppstrm ppInline S.EM content
      | ppInline ppstrm (FONT content) =
        ppTagAndChildren ppstrm ppInline S.FONT content
      | ppInline ppstrm (I content) =
        ppTagAndChildren ppstrm ppInline S.I content
      | ppInline ppstrm (IFRAME content) =
        ppTagAndChildren ppstrm ppFlow S.IFRAME content
      | ppInline ppstrm (IMG attrs) = ppOpenTag ppstrm (S.IMG, attrs)
      | ppInline ppstrm (INPUT attrs) = ppOpenTag ppstrm (S.INPUT, attrs)
      | ppInline ppstrm (Inline_SCRIPT script) = ppScript ppstrm script
      | ppInline ppstrm (KBD content) =
        ppTagAndChildren ppstrm ppInline S.KBD content
      | ppInline ppstrm (LABEL content) =
        ppTagAndChildren ppstrm ppInline S.LABEL content
      | ppInline ppstrm (MAP content) =
        ppTagAndChildren ppstrm ppBlock_or_area S.MAP content
      | ppInline ppstrm (OBJECT content) =
        ppTagAndChildren ppstrm ppFlow_or_param S.OBJECT content
      | ppInline ppstrm (Q content) =
        ppTagAndChildren ppstrm ppInline S.Q content
      | ppInline ppstrm (S content) =
        ppTagAndChildren ppstrm ppInline S.S content
      | ppInline ppstrm (SAMP content) =
        ppTagAndChildren ppstrm ppInline S.SAMP content
      | ppInline ppstrm (SELECT content) =
        ppTagAndChildren ppstrm ppOptgroup_or_option S.SELECT content
      | ppInline ppstrm (SMALL content) =
        ppTagAndChildren ppstrm ppInline S.SMALL content
      | ppInline ppstrm (SPAN content) =
        ppTagAndChildren ppstrm ppInline S.SPAN content
      | ppInline ppstrm (STRIKE content) =
        ppTagAndChildren ppstrm ppInline S.STRIKE content
      | ppInline ppstrm (STRONG content) =
        ppTagAndChildren ppstrm ppInline S.STRONG content
      | ppInline ppstrm (SUB content) =
        ppTagAndChildren ppstrm ppInline S.SUB content
      | ppInline ppstrm (SUP content) =
        ppTagAndChildren ppstrm ppInline S.SUP content
      | ppInline ppstrm (TEXTAREA content) =
        ppTagAndChildren ppstrm ppCdata S.TEXTAREA content
      | ppInline ppstrm (TT content) =
        ppTagAndChildren ppstrm ppInline S.TT content
      | ppInline ppstrm (U content) =
        ppTagAndChildren ppstrm ppInline S.U content
      | ppInline ppstrm (VAR content) =
        ppTagAndChildren ppstrm ppInline S.VAR content
    and ppList_item ppstrm (LI content) =
        ppTagAndChildren ppstrm ppFlow S.LI content
    and ppScript ppstrm (SCRIPT content) =
        ppTagAndChildren ppstrm ppCdata S.SCRIPT content
    and ppParam ppstrm (PARAM attrs) = ppOpenTag ppstrm (S.PARAM, attrs)
    and ppLegend ppstrm (LEGEND content) =
        ppTagAndChildren ppstrm ppInline S.LEGEND content
    and ppDef_term_or_desc ppstrm (DD content) =
        ppTagAndChildren ppstrm ppFlow S.DD content
      | ppDef_term_or_desc ppstrm (DT content) =
        ppTagAndChildren ppstrm ppInline S.DT content
    and ppTable_data ppstrm (CAPTION content) =
        ppTagAndChildren ppstrm ppInline S.CAPTION content
      | ppTable_data ppstrm (COL col) = ppCol ppstrm col
      | ppTable_data ppstrm (COLGROUP content) =
        ppTagAndChildren ppstrm ppCol S.COLGROUP content
      | ppTable_data ppstrm (TBODY content) =
        ppTagAndChildren ppstrm ppTr S.TBODY content
      | ppTable_data ppstrm (TFOOT content) =
        ppTagAndChildren ppstrm ppTr S.TFOOT content
      | ppTable_data ppstrm (THEAD content) =
        ppTagAndChildren ppstrm ppTr S.THEAD content
    and ppTr ppstrm (TR content) =
        ppTagAndChildren ppstrm ppTh_or_td S.TR content
    and ppTh_or_td ppstrm (TD content) =
        ppTagAndChildren ppstrm ppFlow S.TD content
      | ppTh_or_td ppstrm (TH content) =
        ppTagAndChildren ppstrm ppFlow S.TH content
    and ppOptgroup_or_option ppstrm (OPTGROUP content) =
        ppTagAndChildren ppstrm
	   (fn pstrm => fn opt => ppTagAndChildren ppstrm ppCdata S.OPTION opt)
	   S.OPTGROUP content
      | ppOptgroup_or_option ppstrm (OPTION content) =
	ppTagAndChildren ppstrm ppCdata S.OPTION content
    and ppFlow_or_param ppstrm (FlowOrParam_FLOW flow) = ppFlow ppstrm flow
      | ppFlow_or_param ppstrm (FlowOrParam_PARAM param) = ppParam ppstrm param
    and ppBlock_or_script ppstrm (BlockOrScript_BLOCK block) =
        ppBlock ppstrm block
      | ppBlock_or_script ppstrm (BlockOrScript_SCRIPT script) =
        ppScript ppstrm script
    and ppBlock_or_area ppstrm (AREA attrs) = ppOpenTag ppstrm (S.AREA, attrs)
      | ppBlock_or_area ppstrm (BlockOrArea_BLOCK block) = ppBlock ppstrm block
end

(* ____________________________________________________________ *)

val toString = PP.pp_to_string 80 ppHtml

end (* HTML4Printer *)

(* ______________________________________________________________________
   End of html4-printer.sml
   ______________________________________________________________________ *)
