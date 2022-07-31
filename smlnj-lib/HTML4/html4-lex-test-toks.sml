(* html4-lex-test-toks.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HTML4Tokens = struct

datatype token = EOF
               | OPENTAG of Atom.atom * HTML4Utils.tag_payload
               | CLOSETAG of Atom.atom
               | COMMENT of string
               | PCDATA of string
               | DOCTYPE of string
               | CHAR_REF of IntInf.int
               | ENTITY_REF of Atom.atom
               | XML_PROCESSING of string
               (* HTML 4 element tokens. *)
               | STARTA of HTML4Utils.tag_payload
               | ENDA
               | STARTABBR of HTML4Utils.tag_payload
               | ENDABBR
               | STARTACRONYM of HTML4Utils.tag_payload
               | ENDACRONYM
               | STARTADDRESS of HTML4Utils.tag_payload
               | ENDADDRESS
               | STARTAPPLET of HTML4Utils.tag_payload
               | ENDAPPLET
               | STARTAREA of HTML4Utils.tag_payload
               (* No END tag for AREA element. *)
               | STARTB of HTML4Utils.tag_payload
               | ENDB
               | STARTBASE of HTML4Utils.tag_payload
               (* No END tag for BASE element. *)
               | STARTBASEFONT of HTML4Utils.tag_payload
               (* No END tag for BASEFONT element. *)
               | STARTBDO of HTML4Utils.tag_payload
               | ENDBDO
               | STARTBIG of HTML4Utils.tag_payload
               | ENDBIG
               | STARTBLOCKQUOTE of HTML4Utils.tag_payload
               | ENDBLOCKQUOTE
               | STARTBODY of HTML4Utils.tag_payload
               | ENDBODY
               | STARTBR of HTML4Utils.tag_payload
               (* No END tag for BR element. *)
               | STARTBUTTON of HTML4Utils.tag_payload
               | ENDBUTTON
               | STARTCAPTION of HTML4Utils.tag_payload
               | ENDCAPTION
               | STARTCENTER of HTML4Utils.tag_payload
               | ENDCENTER
               | STARTCITE of HTML4Utils.tag_payload
               | ENDCITE
               | STARTCODE of HTML4Utils.tag_payload
               | ENDCODE
               | STARTCOL of HTML4Utils.tag_payload
               (* No END tag for COL element. *)
               | STARTCOLGROUP of HTML4Utils.tag_payload
               | ENDCOLGROUP
               | STARTDD of HTML4Utils.tag_payload
               | ENDDD
               | STARTDEL of HTML4Utils.tag_payload
               | ENDDEL
               | STARTDFN of HTML4Utils.tag_payload
               | ENDDFN
               | STARTDIR of HTML4Utils.tag_payload
               | ENDDIR
               | STARTDIV of HTML4Utils.tag_payload
               | ENDDIV
               | STARTDL of HTML4Utils.tag_payload
               | ENDDL
               | STARTDT of HTML4Utils.tag_payload
               | ENDDT
               | STARTEM of HTML4Utils.tag_payload
               | ENDEM
               | STARTFIELDSET of HTML4Utils.tag_payload
               | ENDFIELDSET
               | STARTFONT of HTML4Utils.tag_payload
               | ENDFONT
               | STARTFORM of HTML4Utils.tag_payload
               | ENDFORM
               | STARTFRAME of HTML4Utils.tag_payload
               (* No END tag for FRAME element. *)
               | STARTFRAMESET of HTML4Utils.tag_payload
               | ENDFRAMESET
               | STARTH1 of HTML4Utils.tag_payload
               | ENDH1
               | STARTH2 of HTML4Utils.tag_payload
               | ENDH2
               | STARTH3 of HTML4Utils.tag_payload
               | ENDH3
               | STARTH4 of HTML4Utils.tag_payload
               | ENDH4
               | STARTH5 of HTML4Utils.tag_payload
               | ENDH5
               | STARTH6 of HTML4Utils.tag_payload
               | ENDH6
               | STARTHEAD of HTML4Utils.tag_payload
               | ENDHEAD
               | STARTHR of HTML4Utils.tag_payload
               (* No END tag for HR element. *)
               | STARTHTML of HTML4Utils.tag_payload
               | ENDHTML
               | STARTI of HTML4Utils.tag_payload
               | ENDI
               | STARTIFRAME of HTML4Utils.tag_payload
               | ENDIFRAME
               | STARTIMG of HTML4Utils.tag_payload
               (* No END tag for IMG element. *)
               | STARTINPUT of HTML4Utils.tag_payload
               (* No END tag for INPUT element. *)
               | STARTINS of HTML4Utils.tag_payload
               | ENDINS
               | STARTISINDEX of HTML4Utils.tag_payload
               (* No END tag for ISINDEX element. *)
               | STARTKBD of HTML4Utils.tag_payload
               | ENDKBD
               | STARTLABEL of HTML4Utils.tag_payload
               | ENDLABEL
               | STARTLEGEND of HTML4Utils.tag_payload
               | ENDLEGEND
               | STARTLI of HTML4Utils.tag_payload
               | ENDLI
               | STARTLINK of HTML4Utils.tag_payload
               (* No END tag for LINK element. *)
               | STARTMAP of HTML4Utils.tag_payload
               | ENDMAP
               | STARTMENU of HTML4Utils.tag_payload
               | ENDMENU
               | STARTMETA of HTML4Utils.tag_payload
               (* No END tag for META element. *)
               | STARTNOFRAMES of HTML4Utils.tag_payload
               | ENDNOFRAMES
               | STARTNOSCRIPT of HTML4Utils.tag_payload
               | ENDNOSCRIPT
               | STARTOBJECT of HTML4Utils.tag_payload
               | ENDOBJECT
               | STARTOL of HTML4Utils.tag_payload
               | ENDOL
               | STARTOPTGROUP of HTML4Utils.tag_payload
               | ENDOPTGROUP
               | STARTOPTION of HTML4Utils.tag_payload
               | ENDOPTION
               | STARTP of HTML4Utils.tag_payload
               | ENDP
               | STARTPARAM of HTML4Utils.tag_payload
               (* No END tag for PARAM element. *)
               | STARTPRE of HTML4Utils.tag_payload
               | ENDPRE
               | STARTQ of HTML4Utils.tag_payload
               | ENDQ
               | STARTS of HTML4Utils.tag_payload
               | ENDS
               | STARTSAMP of HTML4Utils.tag_payload
               | ENDSAMP
               | STARTSCRIPT of HTML4Utils.tag_payload
               | ENDSCRIPT
               | STARTSELECT of HTML4Utils.tag_payload
               | ENDSELECT
               | STARTSMALL of HTML4Utils.tag_payload
               | ENDSMALL
               | STARTSPAN of HTML4Utils.tag_payload
               | ENDSPAN
               | STARTSTRIKE of HTML4Utils.tag_payload
               | ENDSTRIKE
               | STARTSTRONG of HTML4Utils.tag_payload
               | ENDSTRONG
               | STARTSTYLE of HTML4Utils.tag_payload
               | ENDSTYLE
               | STARTSUB of HTML4Utils.tag_payload
               | ENDSUB
               | STARTSUP of HTML4Utils.tag_payload
               | ENDSUP
               | STARTTABLE of HTML4Utils.tag_payload
               | ENDTABLE
               | STARTTBODY of HTML4Utils.tag_payload
               | ENDTBODY
               | STARTTD of HTML4Utils.tag_payload
               | ENDTD
               | STARTTEXTAREA of HTML4Utils.tag_payload
               | ENDTEXTAREA
               | STARTTFOOT of HTML4Utils.tag_payload
               | ENDTFOOT
               | STARTTH of HTML4Utils.tag_payload
               | ENDTH
               | STARTTHEAD of HTML4Utils.tag_payload
               | ENDTHEAD
               | STARTTITLE of HTML4Utils.tag_payload
               | ENDTITLE
               | STARTTR of HTML4Utils.tag_payload
               | ENDTR
               | STARTTT of HTML4Utils.tag_payload
               | ENDTT
               | STARTU of HTML4Utils.tag_payload
               | ENDU
               | STARTUL of HTML4Utils.tag_payload
               | ENDUL
               | STARTVAR of HTML4Utils.tag_payload
               | ENDVAR

fun tokToString EOF = "EOF"
  | tokToString (OPENTAG (tagname, tagdata)) =
    String.concat ["OPENTAG ", Atom.toString tagname, " ",
                   HTML4Utils.payloadToStr tagdata]
  | tokToString (CLOSETAG tagname) = "CLOSETAG " ^ (Atom.toString tagname)
  | tokToString (DOCTYPE docdata) = "DOCTYPE " ^ docdata
  | tokToString (PCDATA pcdata) = ("PCDATA \"" ^ (String.toString pcdata)
                                   ^ "\"")
  | tokToString (COMMENT comment) = "COMMENT " ^ comment
  | tokToString (CHAR_REF refint) = "CHAR REF " ^ (IntInf.toString refint)
  | tokToString (ENTITY_REF refatom) = "ENTITY REF " ^ (Atom.toString refatom)
  | tokToString (XML_PROCESSING directive) = "XML DIRECTIVE " ^ directive

  (* Automatically generated via helper.py: *)
  | tokToString (STARTA payload) =
    "STARTA " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDA = "ENDA"
  | tokToString (STARTABBR payload) =
    "STARTABBR " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDABBR = "ENDABBR"
  | tokToString (STARTACRONYM payload) =
    "STARTACRONYM " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDACRONYM = "ENDACRONYM"
  | tokToString (STARTADDRESS payload) =
    "STARTADDRESS " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDADDRESS = "ENDADDRESS"
  | tokToString (STARTAREA payload) =
    "STARTAREA " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTB payload) =
    "STARTB " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDB = "ENDB"
  | tokToString (STARTBASE payload) =
    "STARTBASE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTBDO payload) =
    "STARTBDO " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDBDO = "ENDBDO"
  | tokToString (STARTBIG payload) =
    "STARTBIG " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDBIG = "ENDBIG"
  | tokToString (STARTBLOCKQUOTE payload) =
    "STARTBLOCKQUOTE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDBLOCKQUOTE = "ENDBLOCKQUOTE"
  | tokToString (STARTBODY payload) =
    "STARTBODY " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDBODY = "ENDBODY"
  | tokToString (STARTBR payload) =
    "STARTBR " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTBUTTON payload) =
    "STARTBUTTON " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDBUTTON = "ENDBUTTON"
  | tokToString (STARTCAPTION payload) =
    "STARTCAPTION " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDCAPTION = "ENDCAPTION"
  | tokToString (STARTCITE payload) =
    "STARTCITE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDCITE = "ENDCITE"
  | tokToString (STARTCODE payload) =
    "STARTCODE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDCODE = "ENDCODE"
  | tokToString (STARTCOL payload) =
    "STARTCOL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTCOLGROUP payload) =
    "STARTCOLGROUP " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDCOLGROUP = "ENDCOLGROUP"
  | tokToString (STARTDD payload) =
    "STARTDD " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDD = "ENDDD"
  | tokToString (STARTDEL payload) =
    "STARTDEL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDEL = "ENDDEL"
  | tokToString (STARTDFN payload) =
    "STARTDFN " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDFN = "ENDDFN"
  | tokToString (STARTDIV payload) =
    "STARTDIV " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDIV = "ENDDIV"
  | tokToString (STARTDL payload) =
    "STARTDL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDL = "ENDDL"
  | tokToString (STARTDT payload) =
    "STARTDT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDT = "ENDDT"
  | tokToString (STARTEM payload) =
    "STARTEM " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDEM = "ENDEM"
  | tokToString (STARTFIELDSET payload) =
    "STARTFIELDSET " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDFIELDSET = "ENDFIELDSET"
  | tokToString (STARTFORM payload) =
    "STARTFORM " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDFORM = "ENDFORM"
  | tokToString (STARTH1 payload) =
    "STARTH1 " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDH1 = "ENDH1"
  | tokToString (STARTH2 payload) =
    "STARTH2 " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDH2 = "ENDH2"
  | tokToString (STARTH3 payload) =
    "STARTH3 " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDH3 = "ENDH3"
  | tokToString (STARTH4 payload) =
    "STARTH4 " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDH4 = "ENDH4"
  | tokToString (STARTH5 payload) =
    "STARTH5 " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDH5 = "ENDH5"
  | tokToString (STARTH6 payload) =
    "STARTH6 " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDH6 = "ENDH6"
  | tokToString (STARTHEAD payload) =
    "STARTHEAD " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDHEAD = "ENDHEAD"
  | tokToString (STARTHR payload) =
    "STARTHR " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTHTML payload) =
    "STARTHTML " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDHTML = "ENDHTML"
  | tokToString (STARTI payload) =
    "STARTI " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDI = "ENDI"
  | tokToString (STARTIMG payload) =
    "STARTIMG " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTINPUT payload) =
    "STARTINPUT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTINS payload) =
    "STARTINS " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDINS = "ENDINS"
  | tokToString (STARTKBD payload) =
    "STARTKBD " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDKBD = "ENDKBD"
  | tokToString (STARTLABEL payload) =
    "STARTLABEL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDLABEL = "ENDLABEL"
  | tokToString (STARTLEGEND payload) =
    "STARTLEGEND " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDLEGEND = "ENDLEGEND"
  | tokToString (STARTLI payload) =
    "STARTLI " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDLI = "ENDLI"
  | tokToString (STARTLINK payload) =
    "STARTLINK " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTMAP payload) =
    "STARTMAP " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDMAP = "ENDMAP"
  | tokToString (STARTMETA payload) =
    "STARTMETA " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTNOSCRIPT payload) =
    "STARTNOSCRIPT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDNOSCRIPT = "ENDNOSCRIPT"
  | tokToString (STARTOBJECT payload) =
    "STARTOBJECT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDOBJECT = "ENDOBJECT"
  | tokToString (STARTOL payload) =
    "STARTOL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDOL = "ENDOL"
  | tokToString (STARTOPTGROUP payload) =
    "STARTOPTGROUP " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDOPTGROUP = "ENDOPTGROUP"
  | tokToString (STARTOPTION payload) =
    "STARTOPTION " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDOPTION = "ENDOPTION"
  | tokToString (STARTP payload) =
    "STARTP " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDP = "ENDP"
  | tokToString (STARTPARAM payload) =
    "STARTPARAM " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTPRE payload) =
    "STARTPRE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDPRE = "ENDPRE"
  | tokToString (STARTQ payload) =
    "STARTQ " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDQ = "ENDQ"
  | tokToString (STARTSAMP payload) =
    "STARTSAMP " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSAMP = "ENDSAMP"
  | tokToString (STARTSCRIPT payload) =
    "STARTSCRIPT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSCRIPT = "ENDSCRIPT"
  | tokToString (STARTSELECT payload) =
    "STARTSELECT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSELECT = "ENDSELECT"
  | tokToString (STARTSMALL payload) =
    "STARTSMALL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSMALL = "ENDSMALL"
  | tokToString (STARTSPAN payload) =
    "STARTSPAN " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSPAN = "ENDSPAN"
  | tokToString (STARTSTRONG payload) =
    "STARTSTRONG " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSTRONG = "ENDSTRONG"
  | tokToString (STARTSTYLE payload) =
    "STARTSTYLE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSTYLE = "ENDSTYLE"
  | tokToString (STARTSUB payload) =
    "STARTSUB " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSUB = "ENDSUB"
  | tokToString (STARTSUP payload) =
    "STARTSUP " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSUP = "ENDSUP"
  | tokToString (STARTTABLE payload) =
    "STARTTABLE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTABLE = "ENDTABLE"
  | tokToString (STARTTBODY payload) =
    "STARTTBODY " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTBODY = "ENDTBODY"
  | tokToString (STARTTD payload) =
    "STARTTD " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTD = "ENDTD"
  | tokToString (STARTTEXTAREA payload) =
    "STARTTEXTAREA " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTEXTAREA = "ENDTEXTAREA"
  | tokToString (STARTTFOOT payload) =
    "STARTTFOOT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTFOOT = "ENDTFOOT"
  | tokToString (STARTTH payload) =
    "STARTTH " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTH = "ENDTH"
  | tokToString (STARTTHEAD payload) =
    "STARTTHEAD " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTHEAD = "ENDTHEAD"
  | tokToString (STARTTITLE payload) =
    "STARTTITLE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTITLE = "ENDTITLE"
  | tokToString (STARTTR payload) =
    "STARTTR " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTR = "ENDTR"
  | tokToString (STARTTT payload) =
    "STARTTT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDTT = "ENDTT"
  | tokToString (STARTUL payload) =
    "STARTUL " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDUL = "ENDUL"
  | tokToString (STARTVAR payload) =
    "STARTVAR " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDVAR = "ENDVAR"
  | tokToString (STARTAPPLET payload) =
    "STARTAPPLET " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDAPPLET = "ENDAPPLET"
  | tokToString (STARTBASEFONT payload) =
    "STARTBASEFONT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTCENTER payload) =
    "STARTCENTER " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDCENTER = "ENDCENTER"
  | tokToString (STARTDIR payload) =
    "STARTDIR " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDDIR = "ENDDIR"
  | tokToString (STARTFONT payload) =
    "STARTFONT " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDFONT = "ENDFONT"
  | tokToString (STARTIFRAME payload) =
    "STARTIFRAME " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDIFRAME = "ENDIFRAME"
  | tokToString (STARTISINDEX payload) =
    "STARTISINDEX " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTMENU payload) =
    "STARTMENU " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDMENU = "ENDMENU"
  | tokToString (STARTS payload) =
    "STARTS " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDS = "ENDS"
  | tokToString (STARTSTRIKE payload) =
    "STARTSTRIKE " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDSTRIKE = "ENDSTRIKE"
  | tokToString (STARTU payload) =
    "STARTU " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDU = "ENDU"
  | tokToString (STARTFRAME payload) =
    "STARTFRAME " ^ (HTML4Utils.payloadToStr payload)
  | tokToString (STARTFRAMESET payload) =
    "STARTFRAMESET " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDFRAMESET = "ENDFRAMESET"
  | tokToString (STARTNOFRAMES payload) =
    "STARTNOFRAMES " ^ (HTML4Utils.payloadToStr payload)
  | tokToString ENDNOFRAMES = "ENDNOFRAMES"

  (* Should cause a "match redundant" error if code is all in synch: *)
  (* | tokToString _ = "???" *)

end

(* ______________________________________________________________________
   End of html4-lex-test-toks.sml
   ______________________________________________________________________ *)
