(* spec.lex
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * (With some code borrowed from ml-yacc)
 *)

%defs (

structure Tok = SpecTokens

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

type lex_result = Tok.token

fun err (lineNo, colNo, msg) = Err.errMsg [
	"Lexical error [", Int.toString lineNo, ".", Int.toString colNo, "]: ", msg
      ]

fun eof () = (
      if (!comLvl > 0)
        then ()
(* err(~1, "unclosed comment starting at line " ^ Int.toString(!comStart)) *)
        else ();
      Tok.EOF)

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0			(* nesting depth of parentheses in CODE *)
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)
);

%let eol=("\n"|"\013\n"|"\013");
%let ws=("\009"|"\011"|"\012"|" "|{eol});
%let lc=[a-z];
%let uc=[A-Z];
%let alpha=({lc}|{uc});
%let digit=[0-9];
%let int=digit*;
%let idchars=({alpha}|{digit}|"_"|"'");
%let id={alpha}{idchars}*;
%let qualid ={id}".";
%let tyvar="'"{idchars}*;

(* the PRECODE state is used for scanning the whitespace between the precursor
 * of a parenthesized SML code fragment and the opening "(".  On most tokens it
 * behaves like INITIAL so that the parser's error correction can respond.
 *)
%states STRING COM PRECODE CODE PRECONSTR CONSTR;

%name SpecLex;

<INITIAL,PRECODE>"of"	=> (YYBEGIN CONSTR; Tok.OF);

<INITIAL,PRECODE>{ws}+		=> (skip());
<INITIAL,PRECODE>{id}		=> (Tok.ID yytext);

<INITIAL,PRECODE>"%change"		=> (Tok.KW_change);
<INITIAL,PRECODE>"%defs"		=> (YYBEGIN PRECODE; Tok.KW_defs);
<INITIAL,PRECODE>"%dropping"		=> (Tok.KW_dropping);
<INITIAL,PRECODE>"%entry"		=> (Tok.KW_entry);
<INITIAL,PRECODE>"%header"		=> (YYBEGIN PRECODE; Tok.KW_header);
<INITIAL,PRECODE>"%import"		=> (Tok.KW_import);
<INITIAL,PRECODE>"%keyword"("s")?	=> (Tok.KW_keywords);
<INITIAL,PRECODE>"%name"		=> (Tok.KW_name);
<INITIAL,PRECODE>"%nonterm"("s")?	=> (Tok.KW_nonterms);
<INITIAL,PRECODE>"%prefer"		=> (Tok.KW_prefer);
<INITIAL,PRECODE>"%refcell"		=> (YYBEGIN CONSTR; Tok.KW_refcell);
<INITIAL,PRECODE>"%start"		=> (Tok.KW_start);
<INITIAL,PRECODE>"%token"("s")?		=> (YYBEGIN CONSTR; Tok.KW_tokens);
<INITIAL,PRECODE>"%tokentype"		=> (YYBEGIN CONSTR; Tok.KW_tokentype);
<INITIAL,PRECODE>"%try"			=> (Tok.KW_try);
<INITIAL,PRECODE>"%value"		=> (YYBEGIN PRECODE; Tok.KW_value);
<INITIAL,PRECODE>"%where"       	=> (YYBEGIN PRECODE; Tok.KW_where);

<INITIAL,PRECODE>"|"	=> (Tok.BAR);
<INITIAL,PRECODE>"@"	=> (YYBEGIN PRECODE; Tok.AT);
<INITIAL,PRECODE>"$"	=> (Tok.DOLLAR);
<INITIAL,PRECODE>"+"	=> (Tok.PLUS);
<INITIAL,PRECODE>"*"	=> (Tok.STAR);
<INITIAL,PRECODE>"?"	=> (Tok.QUERY);
<INITIAL,PRECODE>":"	=> (Tok.COLON);
<INITIAL,PRECODE>";"	=> (Tok.SEMI);
<INITIAL,PRECODE>","	=> (Tok.COMMA);
<INITIAL>"("		=> (Tok.LP);
<INITIAL>")"		=> (Tok.RP);
<INITIAL,PRECODE>"["	=> (Tok.LSB);
<INITIAL,PRECODE>"]"	=> (Tok.RSB);
<INITIAL,PRECODE>"/"	=> (Tok.SLASH);
<INITIAL,PRECODE>"="	=> (Tok.EQ);
<INITIAL,PRECODE>"->"	=> (Tok.ARROW);
<INITIAL,PRECODE>"=>"	=> (YYBEGIN PRECODE; Tok.DARROW);
<INITIAL>"\""	=> (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText()));

<INITIAL>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN INITIAL);
	    continue());
<CONSTR>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN CONSTR);
	    continue());
<PRECODE>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN PRECODE);
	    continue());
<CODE>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM;
	    ignore(continue() before YYBEGIN CODE);
	    continue());

<COM>"(*"
	=> (comLvl := !comLvl+1; continue());
<COM>"*)"
	=> (comLvl := !comLvl-1;
	    if (!comLvl = 0)
	      then (Tok.BOGUS)
	      else continue());
<COM>.|{eol}
	=> (continue());

<PRECODE>"("	=> (pcount := 1; YYBEGIN CODE; clrText(); continue());
<PRECODE>"\""	=> (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN PRECODE);
		    Tok.STRING (getText()));

<CODE>"("	=> (addText yytext;  (* NOTE: the initial "(" is consumed in the PRECODE state *)
		    inc pcount; continue());
<CODE>")"	=> (dec pcount;
		    if !pcount = 0
		      then (YYBEGIN INITIAL; Tok.CODE (getText()))
		      else (addText yytext; continue()));
<CODE>"\""	=> (addText yytext; YYBEGIN STRING;
		    ignore(continue() before YYBEGIN CODE);
		    continue());
<CODE>[^()"]+	=> (addText yytext; continue());

<STRING>"\""	=> (addText yytext; Tok.BOGUS);
<STRING>{eol}	=> (addText yytext; err (!yylineno, !yycolno, "unclosed string");
 	            Tok.BOGUS);
<STRING>\\	=> (addText yytext; continue());
<STRING>\\\\	=> (addText yytext; continue());
(* we apply toString to yytext below to ensure that UTF8 characters are
 * printed in a format that is legal SML.
 *)
<STRING>[^"\\\n\013]+
		=> (addText(String.toString yytext); continue());
<STRING>\\\"	=> (addText yytext; continue());

<CONSTR>{ws}	=> (continue());
<CONSTR>"of"	=> (Tok.OF);
<CONSTR>{id}	=> (Tok.ID yytext);
<CONSTR>{tyvar} => (Tok.TYVAR yytext);
<CONSTR>{qualid}=> (Tok.IDDOT yytext);
<CONSTR>{int}	=> (Tok.INT yytext);
<CONSTR>"|"	=> (Tok.BAR);
<CONSTR>"*"	=> (Tok.STAR);
<CONSTR>":"	=> (Tok.COLON);
<CONSTR>","	=> (Tok.COMMA);
<CONSTR>";"	=> (YYBEGIN INITIAL; Tok.SEMI);
<CONSTR>"("	=> (Tok.LP);
<CONSTR>")"	=> (Tok.RP);
<CONSTR>"{"	=> (Tok.LCB);
<CONSTR>"}"	=> (Tok.RCB);
<CONSTR>"->"	=> (Tok.ARROW);
<CONSTR>"\""	=> (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText()));
<CONSTR>"="	=> (YYBEGIN PRECODE; Tok.EQ);

.	=> (err (!yylineno, !yycolno,
		 concat["illegal character '",
			String.toCString yytext, "'"]);
	    continue());
