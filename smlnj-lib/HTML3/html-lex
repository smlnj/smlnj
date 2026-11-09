(* html-lex
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * A scanner for HTML.
 *
 * TODO:
 *    Recognize the DOCTYPE element
 *	<!DOCTYPE HTML PUBLIC "...">
 *    Clean-up the scanning of start tags (do we need Err?).
 *    Whitespace in PRE elements should be preserved, but how?
 *)

structure T = Tokens
structure Elems = HTMLElementsFn (
  structure Tokens = Tokens
  structure Err = Err
  structure HTMLAttrs = HTMLAttrs)

type pos = int
type svalue = T.svalue
type arg = (((string * int * int) -> unit) * string option)
type ('a, 'b) token = ('a, 'b) T.token
type lexresult= (svalue, pos) token

fun eof _ = Tokens.EOF(0, 0)

(* a buffer for collecting a string piecewise *)
val buffer = ref ([] : string list)
fun addStr s = (buffer := s :: !buffer)
fun getStr () = (String.concat(List.rev(! buffer)) before (buffer := []))

%%

%s COM1 COM2 STAG;

%header (functor HTMLLexFn (
  structure Tokens : HTML_TOKENS
  structure Err : HTML_ERROR
  structure HTMLAttrs : HTML_ATTRS));

%arg (errorFn, file);

%full
%count

alpha=[A-Za-z];
digit=[0-9];
namechar=[-A-Za-z0-9.];
tag=({alpha}{namechar}*);
ws = [\ \t];

%%

<INITIAL>"<"{tag}
	=> (addStr yytext; YYBEGIN STAG; continue());
<STAG>">"
	=> (addStr yytext;
	    YYBEGIN INITIAL;
	    case Elems.startTag file (getStr(), !yylineno, !yylineno)
	     of NONE => continue()
	      | (SOME tag) => tag
	    (* end case *));
<STAG>\n
	=> (addStr " "; continue());
<STAG>{ws}+
	=> (addStr yytext; continue());
<STAG>{namechar}+
	=> (addStr yytext; continue());
<STAG>"="
	=> (addStr yytext; continue());
<STAG>"\""[^\"\n]*"\""
	=> (addStr yytext; continue());
<STAG>"'"[^'\n]*"'"
	=> (addStr yytext; continue());
<STAG>.
	=> (addStr yytext; continue());

<INITIAL>"</"{tag}{ws}*">"
	=> (case Elems.endTag file (yytext, !yylineno, !yylineno)
	     of NONE => continue()
	      | (SOME tag) => tag
	    (* end case *));

<INITIAL>"<!--"
	=> (YYBEGIN COM1; continue());
<COM1>"--"
	=> (YYBEGIN COM2; continue());
<COM1>\n
	=> (continue());
<COM1>.
	=> (continue());
<COM2>"--"
	=> (YYBEGIN COM1; continue());
<COM2>">"
	=> (YYBEGIN INITIAL; continue());
<COM2>\n
	=> (continue());
<COM2>{ws}
	=> (continue());
<COM2>.
	=> (errorFn("bad comment syntax", !yylineno, !yylineno+1);
	    YYBEGIN INITIAL;
	    continue());

<INITIAL>"&#"[A-Za-z]+";"
	=> (
(** At some point, we should support &#SPACE; and &#TAB; **)
	    continue());

<INITIAL>"&#"[0-9]+";"
	=> (T.CHAR_REF(yytext, !yylineno, !yylineno));

<INITIAL>"&"{tag}";"
	=> (T.ENTITY_REF(yytext, !yylineno, !yylineno));

<INITIAL>"\n"
	=> (continue());
<INITIAL>{ws}
	=> (continue());

<INITIAL>[^<]+
	=> (T.PCDATA(yytext, !yylineno, !yylineno));
<INITIAL>.
	=> (errorFn(concat[
		"bogus character #\"", Char.toString(String.sub(yytext, 0)),
		"\" in PCDATA\n"
	      ], !yylineno, !yylineno+1);
	    continue());

