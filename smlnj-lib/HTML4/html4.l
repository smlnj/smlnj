(* html4.l
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

%name HTML4Lexer;

%defs (
open HTML4TokenUtils

fun eof() = EOF
type lex_result = token

val buffer = ref ([] : string list)

fun addStr s = (buffer := s :: !buffer)
fun getStr () = (String.concat(List.rev(!buffer)) before (buffer := []))

(* trim an optional ";" from a non-empty substring *)
fun trimSemi ss = if Substring.sub(ss, Substring.size ss - 1) = #";"
      then Substring.trimr 1 ss
      else ss
val scanInt = #1 o valOf o IntInf.scan StringCvt.DEC Substring.getc
val scanHex = #1 o valOf o IntInf.scan StringCvt.HEX Substring.getc
);

%states INITIAL, INTAG, COM1, COM2;

%let alpha=[A-Za-z];
%let digit=[0-9];
%let hexdigit=[0-9a-fA-F];
%let namechar=[-.A-Za-z0-9.];
%let tag={alpha}{namechar}*;
%let ws=[\ \t];
%let nonct=[^>]+;

(* Open tags *)

<INITIAL>"<"{tag} => (addStr yytext; YYBEGIN INTAG; continue());
<INTAG>">" => (addStr yytext; YYBEGIN INITIAL; mkOpenTag(getStr()));
<INTAG>{nonct} => (addStr yytext; continue());

(* Close tags *)

<INITIAL>"</"{tag}{ws}*">" => (mkCloseTag yytext);

(* Comments *)

<INITIAL>"<!--" => (addStr yytext; YYBEGIN COM1; continue());
<COM1>"--" => (addStr yytext; YYBEGIN COM2; continue());
<COM1>. => (addStr yytext; continue());
<COM2>"--" => (addStr yytext; YYBEGIN COM1; continue());
<COM2>">" => (addStr yytext; YYBEGIN INITIAL; COMMENT (getStr()));
<COM2>\n => (addStr yytext; continue());
<COM2>{ws}+ => (addStr yytext; continue());
<COM2>. => (raise (Fail ("Unexpected text in comment: \"" ^ yytext ^ "\"")));

(* Doctype *)

<INITIAL>"<!DOCTYPE"{nonct}">" => (DOCTYPE yytext);

(* XML Processing instructions. *)

<INITIAL>"<?".*"?>" => (XML_PROCESSING yytext);

(* Entities *)

<INITIAL>"&#"{digit}+";"?
		=> (CHAR_REF(scanInt (trimSemi (Substring.slice(yysubstr, 2, NONE)))));
<INITIAL>"&#x"{hexdigit}+";"?
		=> (CHAR_REF(scanHex (trimSemi (Substring.slice(yysubstr, 3, NONE)))));
<INITIAL>"&"{tag}";"?
		=> (ENTITY_REF(Atom.atom (Substring.string (Substring.slice(yysubstr, 1, NONE)))));

(* Character data *)

<INITIAL>"&" => (PCDATA yytext);

<INITIAL>[^<&]+ => (PCDATA yytext);

(* ______________________________________________________________________
   End of html4.l
   ______________________________________________________________________ *)
