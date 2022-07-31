(* -*- sml-lex -*-
 *
 * cm.lex
 *
 * lexical analysis (ML-Lex specification) for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

structure S = CMSemant

type svalue = Tokens.svalue
type pos = int

type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

type lexarg = {
	       enterC: unit -> unit,
	       leaveC: unit -> bool,
	       newS: pos -> unit,
	       addS: char -> unit,
	       addSC: string * int -> unit,
	       addSN: string * pos -> unit,
	       getS: pos * (string * pos * pos -> lexresult) -> lexresult,
	       handleEof: unit -> pos,
	       newline: pos -> unit,
	       obsolete: pos * pos -> unit,
	       error: pos * pos -> string -> unit,
	       sync: pos * string -> unit,
	       in_section2: bool ref
	      }

type arg = lexarg

fun eof (arg: lexarg) = let
    val pos = #handleEof arg ()
in
    Tokens.EOF (pos, pos)
end

fun errorTok (t, p) = let
    fun findGraph i =
	if Char.isGraph (String.sub (t, i)) then i
	else findGraph (i + 1)
    fun findError i =
	if String.sub (t, i) = #"e" then i
	else findError (i + 1)
    val start = findGraph (5 + findError 0)
    val msg = String.extract (t, start, NONE)
in
    Tokens.ERROR (msg, p + 1, p + size t)
end

fun plain t (_: bool ref, arg) = t arg : lexresult
fun is_token (r, arg) = (r := true; Tokens.IS arg) : lexresult

val cm_ids = [("Group", plain Tokens.GROUP),
	      ("GROUP", plain Tokens.GROUP),
	      ("group", plain Tokens.GROUP),
	      ("Library", plain Tokens.LIBRARY),
	      ("LIBRARY", plain Tokens.LIBRARY),
	      ("library", plain Tokens.LIBRARY),
	      ("IS", is_token),
	      ("is", is_token),
	      ("*", plain Tokens.STAR),
	      ("-", plain Tokens.DASH),
	      ("Source", plain Tokens.SOURCE),
	      ("SOURCE", plain Tokens.SOURCE),
	      ("source", plain Tokens.SOURCE)]

val ml_ids = [("structure", Tokens.STRUCTURE),
	      ("signature", Tokens.SIGNATURE),
	      ("functor", Tokens.FUNCTOR),
	      ("funsig", Tokens.FUNSIG)]

val pp_ids = [("defined", plain Tokens.DEFINED),
	      ("div", plain (fn (x, y) => Tokens.MULSYM (S.DIV, x, y))),
	      ("mod", plain (fn (x, y) => Tokens.MULSYM (S.MOD, x, y))),
	      ("andalso", plain Tokens.ANDALSO),
	      ("orelse", plain Tokens.ORELSE),
	      ("not", plain Tokens.NOT),
	      ("true", plain Tokens.TRUE),
	      ("false", plain Tokens.FALSE)]

fun idToken (t, p, idlist, default, chstate, in_section2) =
    case List.find (fn (id, _) => id = t) ml_ids of
	SOME (_, tok) => (chstate (); tok (p, p + size t))
      | NONE =>
	    (case List.find (fn (id, _) => id = t) idlist of
		 SOME (_, tok) => tok (in_section2, (p, p + size t))
	       | NONE => default (t, p, p + size t))

(* states:

     INITIAL -> C
       |
       +------> P -> PC
       |        |
       |        +--> PM -> PMC
       |
       +------> M -> MC
       |
       +------> S -> SS

   "C"  -- COMMENT
   "P"  -- PREPROC
   "M"  -- MLSYMBOL
   "S"  -- STRING
   "SS" -- STRINGSKIP
*)

%%

%s C P PC PM PMC M MC S SS;

%header(functor CMLexFun (structure Tokens: CM_TOKENS));

%arg ({ enterC, leaveC,
        newS, addS, addSC, addSN, getS,
        handleEof,
        newline,
	obsolete,
	error,
	sync,
	in_section2 });

idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
cmextrachars=[.;,!%&$+/<=>?@~|#*]|\-|\^;
cmidchars={idchars}|{cmextrachars};
cmid={cmidchars}+;
ws=("\012"|[\t\ ]);
eol=("\013\010"|"\013"|"\010");
neol=[^\013\010];
sym=[!%&$+/:<=>?@~|#*]|\-|\^|"\\";
digit=[0-9];
sharp="#";
%%

<INITIAL>"(*"           => (enterC (); YYBEGIN C; continue ());
<P>"(*"                 => (enterC (); YYBEGIN PC; continue ());
<PM>"(*"                => (enterC (); YYBEGIN PMC; continue ());
<M>"(*"                 => (enterC (); YYBEGIN MC; continue ());

<C,PC,PMC,MC>"(*"       => (enterC (); continue ());

<C>"*)"                 => (if leaveC () then YYBEGIN INITIAL else ();
			    continue ());
<PC>"*)"                => (if leaveC () then YYBEGIN P else ();
			    continue ());
<PMC>"*)"                => (if leaveC () then YYBEGIN PM else ();
			    continue ());
<MC>"*)"                => (if leaveC () then YYBEGIN M else ();
			    continue ());
<C,PC,PMC,MC>{eol}      => (newline yypos; continue ());
<C,PC,PMC,MC>.          => (continue ());

<INITIAL,P,PM,M>"*)"	=> (error (yypos, yypos+2)
			      "unmatched comment delimiter";
			    continue ());

<INITIAL>"\""		=> (YYBEGIN S; newS yypos; continue ());

<S>"\\a"		=> (addS #"\a"; continue ());
<S>"\\b"		=> (addS #"\b"; continue ());
<S>"\\f"		=> (addS #"\f"; continue ());
<S>"\\n"		=> (addS #"\n"; continue ());
<S>"\\r"		=> (addS #"\r"; continue ());
<S>"\\t"		=> (addS #"\t"; continue ());
<S>"\\v"		=> (addS #"\v"; continue ());

<S>"\\^"@		=> (addS (chr 0); continue ());
<S>"\\^"[a-z]	        => (addSC (yytext, ord #"a"); continue ());
<S>"\\^"[A-Z]	        => (addSC (yytext, ord #"A"); continue ());
<S>"\\^["		=> (addS (chr 27); continue ());
<S>"\\^\\"		=> (addS (chr 28); continue ());
<S>"\\^]"		=> (addS (chr 29); continue ());
<S>"\\^^"		=> (addS (chr 30); continue ());
<S>"\\^_"		=> (addS (chr 31); continue ());

<S>"\\"[0-9][0-9][0-9]	=> (addSN (yytext, yypos); continue ());

<S>"\\\""		=> (addS #"\""; continue ());
<S>"\\\\"		=> (addS #"\\"; continue ());

<S>"\\"{eol}	        => (YYBEGIN SS; newline (yypos + 1); continue ());
<S>"\\"{ws}+	        => (YYBEGIN SS; continue ());

<S>"\\".		=> (error (yypos, yypos+2)
			     ("illegal escape character in string " ^ yytext);
			    continue ());

<S>"\""		        => (YYBEGIN INITIAL; getS (yypos, Tokens.FILE_NATIVE));
<S>{eol}		=> (newline yypos;
			    error (yypos, yypos + size yytext)
			      "illegal linebreak in string";
			    continue ());

<S>.		        => (addS (String.sub (yytext, 0)); continue ());

<SS>{eol}	        => (newline yypos; continue ());
<SS>{ws}+	        => (continue ());
<SS>"\\"	        => (YYBEGIN S; continue ());
<SS>.		        => (error (yypos, yypos+1)
			     ("illegal character in stringskip " ^ yytext);
			    continue ());

<INITIAL,P>"("		=> (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL,P>")"		=> (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL>":"		=> (Tokens.COLON (yypos, yypos + 1));
<P>"+"		        => (Tokens.ADDSYM (S.PLUS, yypos, yypos + 1));
<P>"-"		        => (Tokens.ADDSYM (S.MINUS, yypos, yypos + 1));
<P>"*"		        => (Tokens.MULSYM (S.TIMES, yypos, yypos + 1));
<P>"<>"		        => (Tokens.EQSYM (S.NE, yypos, yypos + 2));
<P>"!="                 => (obsolete (yypos, yypos + 2);
			    Tokens.EQSYM (S.NE, yypos, yypos+2));
<P>"<="		        => (Tokens.INEQSYM (S.LE, yypos, yypos + 2));
<P>"<"		        => (Tokens.INEQSYM (S.LT, yypos, yypos + 1));
<P>">="		        => (Tokens.INEQSYM (S.GE, yypos, yypos + 2));
<P>">"		        => (Tokens.INEQSYM (S.GT, yypos, yypos + 1));
<P>"=="                 => (obsolete (yypos, yypos + 2);
			    Tokens.EQSYM (S.EQ, yypos, yypos + 2));
<P>"="		        => (Tokens.EQSYM (S.EQ, yypos, yypos + 1));
<P>"~"		        => (Tokens.TILDE (yypos, yypos + 1));

<P>{digit}+	        => (Tokens.NUMBER
			     (valOf (Int.fromString yytext)
			      handle _ =>
				  (error (yypos, yypos + size yytext)
				     "number too large";
				   0),
			      yypos, yypos + size yytext));

<P>{id}                 => (idToken (yytext, yypos, pp_ids, Tokens.CM_ID,
				     fn () => YYBEGIN PM, in_section2));
<P>"/"                  => (obsolete (yypos, yypos + 1);
			    Tokens.MULSYM (S.DIV, yypos, yypos + 1));
<P>"%"                  => (obsolete (yypos, yypos + 1);
			    Tokens.MULSYM (S.MOD, yypos, yypos + 1));
<P>"&&"                 => (obsolete (yypos, yypos + 2);
			    Tokens.ANDALSO (yypos, yypos + 2));
<P>"||"                 => (obsolete (yypos, yypos + 2);
			    Tokens.ORELSE (yypos, yypos + 2));
<P>"!"                  => (obsolete (yypos, yypos + 1);
			    Tokens.NOT (yypos, yypos + 1));

<M>({id}|{sym}+)        => (YYBEGIN INITIAL;
			    Tokens.ML_ID (yytext, yypos, yypos + size yytext));
<PM>({id}|{sym}+)       => (YYBEGIN P;
			    Tokens.ML_ID (yytext, yypos, yypos + size yytext));

<INITIAL,P>{eol}{sharp}{ws}*"if" => (YYBEGIN P;
				     newline yypos;
				     Tokens.IF (yypos, yypos + size yytext));
<INITIAL,P>{eol}{sharp}{ws}*"elif" => (YYBEGIN P;
				     newline yypos;
				     Tokens.ELIF (yypos, yypos + size yytext));
<INITIAL,P>{eol}{sharp}{ws}*"else" => (YYBEGIN P;
				     newline yypos;
				     Tokens.ELSE (yypos, yypos + size yytext));
<INITIAL,P>{eol}{sharp}{ws}*"endif" => (YYBEGIN P;
				      newline yypos;
				      Tokens.ENDIF (yypos,
						    yypos + size yytext));
<INITIAL,P>{eol}{sharp}{ws}*"error"{ws}+{neol}* => (newline yypos;
						    errorTok (yytext, yypos));
<INITIAL,M,PM>{eol}     => (newline yypos; continue ());
<P>{eol}                => (YYBEGIN INITIAL; newline yypos; continue ());

<INITIAL,M,PM,P>{ws}+   => (continue ());

<M,PM>.                 => (error (yypos, yypos+1)
			    ("illegal character at start of ML symbol: " ^
			     yytext);
			    continue ());

<INITIAL>{cmid}		=> (idToken (yytext, yypos,
				     if !in_section2 then [] else cm_ids,
				     Tokens.FILE_STANDARD,
				     fn () => YYBEGIN M, in_section2));


<INITIAL>.		=> (error (yypos, yypos+1)
			    ("illegal character: " ^ yytext);
			    continue ());

{eol}{sharp}{ws}*"line"{ws}+{neol}* => (newline yypos;
					sync (yypos, yytext);
					continue ());
