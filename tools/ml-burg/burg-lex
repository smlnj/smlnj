(* burg-lex
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * ML-Lex specification for ML-burg.
 *)

structure T 		= Tokens
structure E		= ErrorMsg
type pos 		= int
type svalue		= T.svalue
type ('a,'b) token 	= ('a,'b) T.token
type lexresult		= (svalue,pos) token

val comLevel		= ref 0
val lineNum		= ref 0
val verbatimLevel	= ref 0
val percentCount	= ref 0
val rawLine		= ref ""
val rawNoNewLine	= ref false
val raw:string list ref = ref []
val reachedEop		= ref false

fun resetState()	= (comLevel      := 0;
			   lineNum       := 0;
			   verbatimLevel := 0;
			   percentCount  := 0;
			   rawLine	 := "";
			   rawNoNewLine	 := false;
			   raw		 := [];
			   reachedEop	 := false)
			   
fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

fun incVerbLvl()	= if !verbatimLevel <> 0 
			  then E.impossible "nested verbatim levels"
			  else inc verbatimLevel

fun outputRaw (s:string) = (rawLine := !rawLine^s; rawNoNewLine := true)

fun rawNextLine ()	= (raw := !rawLine^"\n":: (!raw);
			   rawLine := ""; rawNoNewLine := false)

fun rawStop ()		= if !rawNoNewLine then rawNextLine () else ()

fun eof()		= (if !comLevel > 0 then E.complain "unclosed comment"
			   else if !verbatimLevel <> 0 then
				   E.complain "unclosed user input"
			        else ();
			   if !reachedEop 
			   then T.K_EOF(!lineNum,!lineNum)
			   else	(rawStop ();
				 T.PPERCENT(rev(!raw),!lineNum,!lineNum)
				before (raw := [];
				        reachedEop := true)))

%%

%s 			COMMENT DUMP POSTLUDE;
%header			(functor BurgLexFun(structure Tokens : Burg_TOKENS));
idchars			= [A-Za-z_0-9];
id			= [A-Za-z]{idchars}*;
ws			= [\t\ ]*;
num			= [0-9]+;
line			= .*;




%%

<INITIAL> "\n"		=> (inc lineNum; continue());
<INITIAL> "%{"		=> (incVerbLvl(); YYBEGIN DUMP; continue());
<INITIAL> "%%"		=> (inc percentCount; 
			    if !percentCount = 2 
			    then (YYBEGIN POSTLUDE; continue())
			    else T.PPERCENT(rev(!raw),!lineNum,!lineNum)
					before raw := []);
<INITIAL> {ws}		=> (continue());
<INITIAL> \n		=> (inc lineNum; continue());
<INITIAL> "("		=> (T.K_LPAREN(!lineNum,!lineNum));
<INITIAL> ")"		=> (T.K_RPAREN(!lineNum,!lineNum));
<INITIAL> ","		=> (T.K_COMMA(!lineNum,!lineNum));
<INITIAL> ":"		=> (T.K_COLON(!lineNum,!lineNum));
<INITIAL> ";"		=> (T.K_SEMICOLON(!lineNum,!lineNum));
<INITIAL> "="		=> (T.K_EQUAL(!lineNum,!lineNum));
<INITIAL> "|"		=> (T.K_PIPE(!lineNum,!lineNum));
<INITIAL> "%term"	=> (T.K_TERM(!lineNum,!lineNum));
<INITIAL> "%start"	=> (T.K_START(!lineNum,!lineNum));
<INITIAL> "%termprefix"	=> (T.K_TERMPREFIX(!lineNum,!lineNum));
<INITIAL> "%ruleprefix"	=> (T.K_RULEPREFIX(!lineNum,!lineNum));
<INITIAL> "%sig"	=> (T.K_SIG(!lineNum,!lineNum));
<INITIAL> "(*"		=> (YYBEGIN COMMENT; comLevel:=1; continue());
<INITIAL> {num}		=> (T.INT(valOf(Int.fromString yytext),!lineNum,!lineNum));
<INITIAL> {id}		=> (T.ID(yytext,!lineNum,!lineNum));

<COMMENT> "(*"		=> (inc comLevel; continue());
<COMMENT> \n		=> (inc lineNum; continue());
<COMMENT> "*)"		=> (dec comLevel;
			    if !comLevel=0 then YYBEGIN INITIAL else ();
			    continue());
<COMMENT> .		=> (continue());

<DUMP> "%}"		=> (rawStop(); dec verbatimLevel;
			    YYBEGIN INITIAL; continue());
<DUMP> "\n"		=> (rawNextLine (); inc lineNum; continue());
<DUMP> {line}		=> (outputRaw yytext; continue());


<POSTLUDE> "\n"		=> (rawNextLine (); inc lineNum; continue());
<POSTLUDE> {line}	=> (outputRaw yytext; continue());
