(* spec.lex
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * (With some code borrowed from ml-yacc)
 *)

val comLvl : int ref = ref 0		(* nesting depth of comments *)
val comStart : int ref = ref 0		(* start line of current comment *)

type pos = int
type svalue = Tok.svalue
type lexresult = (svalue,pos) Tok.token
type lexarg = (int * string) -> unit	(* error reporting function *)
type arg = lexarg
type ('a, 'b) token = ('a,'b) Tok.token

fun eof (err) = (
      if (!comLvl > 0)
        then err(~1, "unclosed comment starting at line " ^ Int.toString(!comStart))
        else ();
      Tok.EOF(~1, ~1))

val text : string list ref = ref []
fun addText s = (text := s::(!text))
fun clrText () = (text := [])
fun getText () = concat (rev (!text))

val pcount = ref 0
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

%%

%header (functor MLYLexFn (structure Tok : MLY_TOKENS));
%arg (err);
%count

eol=("\n"|"\013\n"|"\013");
ws=("\009"|"\011"|"\012"|" "|{eol});
lc=[a-z];
uc=[A-Z];
alpha=({lc}|{uc});
digit=[0-9];
int=digit*;
idchars=({alpha}|{digit}|"_"|"'");
id={alpha}{idchars}*;
qualid ={id}".";
tyvar="'"{idchars}*;

%s STRING COM CODE CONSTR;

%%

<INITIAL>{ws}+	=> (continue());
<INITIAL>{id}		=> (Tok.ID (yytext, !yylineno, !yylineno));

<INITIAL>"%tokens"	=> (YYBEGIN CONSTR; 
			    Tok.KW_tokens	(!yylineno, !yylineno));
<INITIAL>"%defs"	=> (YYBEGIN CODE; clrText();
			    Tok.KW_defs		(!yylineno, !yylineno));
<INITIAL>"%keywords"	=> (Tok.KW_keywords	(!yylineno, !yylineno));
<INITIAL>"%import"	=> (Tok.KW_import	(!yylineno, !yylineno));
<INITIAL>"%name"	=> (Tok.KW_name		(!yylineno, !yylineno));
<INITIAL>"%header"	=> (Tok.KW_header	(!yylineno, !yylineno));
<INITIAL>"%start"	=> (Tok.KW_start	(!yylineno, !yylineno));
<INITIAL>"%entry"	=> (Tok.KW_entry	(!yylineno, !yylineno));
<INITIAL>"%try"		=> (Tok.KW_try		(!yylineno, !yylineno));
<INITIAL>"%where"       => (YYBEGIN CODE; clrText();
			    Tok.KW_where	(!yylineno, !yylineno));
<INITIAL>"%debugactions"=> (Tok.KW_debugact	(!yylineno, !yylineno));
<INITIAL>"%unitactions"	=> (Tok.KW_unitact	(!yylineno, !yylineno));
<INITIAL>"%drop"	=> (Tok.KW_drop 	(!yylineno, !yylineno));
<INITIAL>"%extend"	=> (Tok.KW_extend 	(!yylineno, !yylineno));
<INITIAL>"%replace"	=> (Tok.KW_replace 	(!yylineno, !yylineno));
<INITIAL>"%refcell"	=> (YYBEGIN CONSTR;
			    Tok.KW_refcell 	(!yylineno, !yylineno));

<INITIAL>"|"	=> (Tok.BAR	(!yylineno, !yylineno)); 
<INITIAL>"@"	=> (YYBEGIN CODE; clrText();
		    Tok.AT	(!yylineno, !yylineno)); 
<INITIAL>"$"	=> (Tok.DOLLAR	(!yylineno, !yylineno)); 
<INITIAL>"+"	=> (Tok.PLUS	(!yylineno, !yylineno)); 
<INITIAL>"*"	=> (Tok.STAR	(!yylineno, !yylineno));
<INITIAL>"?"	=> (Tok.QUERY	(!yylineno, !yylineno));
<INITIAL>":"	=> (Tok.COLON	(!yylineno, !yylineno));
<INITIAL>";"	=> (Tok.SEMI	(!yylineno, !yylineno));
<INITIAL>","	=> (Tok.COMMA	(!yylineno, !yylineno));
<INITIAL>"("	=> (Tok.LP	(!yylineno, !yylineno));
<INITIAL>")"	=> (Tok.RP	(!yylineno, !yylineno));
<INITIAL>"["	=> (Tok.LSB	(!yylineno, !yylineno));
<INITIAL>"]"	=> (Tok.RSB	(!yylineno, !yylineno));
<INITIAL>"/"	=> (Tok.SLASH	(!yylineno, !yylineno));
<INITIAL>"="	=> (Tok.EQ	(!yylineno, !yylineno));
<INITIAL>"->"	=> (Tok.ARROW	(!yylineno, !yylineno));
<INITIAL>"=>"	=> (YYBEGIN CODE; clrText();
		    Tok.DARROW	(!yylineno, !yylineno));
<INITIAL>"\""	
	        => (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN INITIAL);
		    Tok.STRING (getText(), !yylineno, !yylineno));

<INITIAL>"(*" 
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN INITIAL);
	    continue());
<CONSTR>"(*"
	=> (comLvl := 1; comStart := !yylineno; YYBEGIN COM; 
	    ignore(continue() before YYBEGIN CONSTR);
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
	      then (Tok.BOGUS(!yylineno, !yylineno)) 
	      else continue());
<COM>.|{eol}
	=> (continue());

<CODE>"("	=> (if !pcount = 0 then () else addText yytext;
		    inc pcount; continue());
<CODE>")"	=> (dec pcount; 
		    if !pcount = 0 then
		      (YYBEGIN INITIAL;
		       Tok.CODE (getText(), !yylineno, !yylineno))
		    else (addText yytext; continue()));
<CODE>"\""	=> (addText yytext; YYBEGIN STRING; 
		    ignore(continue() before YYBEGIN CODE);
		    continue());
<CODE>[^()"]+	=> (addText yytext; continue());

<STRING>"\""	=> (addText yytext; Tok.BOGUS(!yylineno, !yylineno));
<STRING>{eol}	=> (addText yytext; err (!yylineno, "unclosed string");
 	            Tok.BOGUS(!yylineno, !yylineno));
<STRING>\\	=> (addText yytext; continue());
<STRING>\\\\	=> (addText yytext; continue());
<STRING>[^"\\\n\013]+ 
		=> (addText yytext; continue());
<STRING>\\\"	=> (addText yytext; continue());

<CONSTR>{ws}	=> (continue());
<CONSTR>"of"	=> (Tok.OF	(!yylineno, !yylineno));
<CONSTR>{id}	=> (Tok.ID	(yytext, !yylineno, !yylineno));
<CONSTR>{tyvar} => (Tok.TYVAR	(yytext, !yylineno, !yylineno));
<CONSTR>{qualid}=> (Tok.IDDOT	(yytext, !yylineno, !yylineno));
<CONSTR>{int}	=> (Tok.INT	(yytext, !yylineno, !yylineno));
<CONSTR>"|"	=> (Tok.BAR	(!yylineno, !yylineno)); 
<CONSTR>"*"	=> (Tok.STAR	(!yylineno, !yylineno));
<CONSTR>":"	=> (Tok.COLON	(!yylineno, !yylineno));
<CONSTR>";"	=> (YYBEGIN INITIAL; 
		    Tok.SEMI	(!yylineno, !yylineno));
<CONSTR>"("	=> (Tok.LP	(!yylineno, !yylineno));
<CONSTR>")"	=> (Tok.RP	(!yylineno, !yylineno));
<CONSTR>"{"	=> (Tok.LCB	(!yylineno, !yylineno));
<CONSTR>"}"	=> (Tok.RCB	(!yylineno, !yylineno));
<CONSTR>"->"	=> (Tok.ARROW	(!yylineno, !yylineno));
<CONSTR>"\""	=> (YYBEGIN STRING; clrText(); addText yytext;
		    ignore(continue() before YYBEGIN CONSTR);
		    Tok.STRING (getText(), !yylineno, !yylineno));
<CONSTR>":=="	=> (YYBEGIN CODE; clrText();
		    Tok.REFSET	(!yylineno, !yylineno));

.	=> (err (!yylineno, 
		 concat["illegal character '", 
			String.toCString yytext, "'"]);
	    continue());
