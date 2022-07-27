type pos = int
type svalue = Tok.svalue
type ('a,'b) token = ('a,'b) Tok.token
type lexresult= (svalue,pos) token

open Tok

val eof = fn () => EOF(~1,~1)
val error = (* fn (e,l : int,_) =>
      output(std_out,"line " ^ (makestring l) ^
	     ": " ^ e ^ "\n") *)
     fn _ => ()

(* what to do (i.e. switch start states) after recognizing an action *)
val afterAction = ref (fn () => ())

(* paren counting for actions *)
val pcount = ref 0
val inquote = ref false
fun inc r = if !inquote then () else r := !r + 1
fun dec r = if !inquote then () else r := !r - 1

(* buffer for accumulating test across the rules for actions *)
local
val text = ref ([] : string list)
in
fun clrAction () = (text := ["("])
fun updAction str = if !pcount > 0
      then (text := str :: !text)
      else ()
fun getAction () = String.concat (rev (!text))
end

structure SIS = RegExp.SymSet
fun uniChar s = let
      fun toW32 (c : Char.char) : UTF8.wchar =
	(case c of #"0" => 0w0 | #"1" => 0w1 | #"2" => 0w2 | #"3" => 0w3
	 	 | #"4" => 0w4 | #"5" => 0w5 | #"6" => 0w6 | #"7" => 0w7
	 	 | #"8" => 0w8 | #"9" => 0w9 | #"a" => 0w10 | #"A" => 0w10
		 | #"b" => 0w11 | #"B" => 0w11 | #"c" => 0w12 | #"C" => 0w12
		 | #"d" => 0w13 | #"D" => 0w13 | #"e" => 0w14 | #"E" => 0w14
		 | #"f" => 0w15 | #"F" => 0w15
		 | _ => raise Fail "invalid unicode escape sequence")
      fun iter (#"u"::_, v) = v
        | iter (c::cs,   v) = iter (cs, 0w16*v + (toW32 c))
	| iter _ = raise Fail "invalid unicode escape sequence"
      in iter (List.rev (String.explode s), 0w0)
      end

val highAscii = SIS.interval(0w128, 0w255)

%%

%header (functor MLLexLexFun(structure Tok: MLLex_TOKENS));
%s DEFS RE RECB CHARCLASS LEXSTATES ACTION STRING;
%count

ws	= [\ \n\t\013];
alpha	= [a-zA-Z];
num	= [0-9];
hex	= {num} | [a-fA-F];
id	= {alpha}({alpha} | {num} | "_" | "'")*;

%%

<INITIAL> "%%"	=> (YYBEGIN DEFS; LEXMARK(!yylineno, !yylineno));
<INITIAL> ([^%] | [^%]* % [^%])*
		=> (DECLS(yytext, !yylineno, !yylineno));

<DEFS> {ws}	=> (lex());
<DEFS> "%%"	=> (YYBEGIN RE; LEXMARK(!yylineno, !yylineno));
<DEFS> "%s"	=> (YYBEGIN LEXSTATES; STATES(!yylineno, !yylineno));
<DEFS> "%header" {ws}* "("
		=> (clrAction(); pcount := 1; inquote := false;
	            YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    HEADER(!yylineno, !yylineno));
<DEFS> "%structure"
		=> (STRUCT(!yylineno, !yylineno));
<DEFS> "%arg" {ws}* "("
		=> (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    ARG(!yylineno, !yylineno));
<DEFS> "%count" => (COUNT(!yylineno, !yylineno));
<DEFS> "%reject"=> (REJECTTOK(!yylineno, !yylineno));
<DEFS> "%unicode"
		=> (UNICODE(!yylineno, !yylineno));
<DEFS> "%full"	=> (FULL(!yylineno, !yylineno));
<DEFS> {id}	=> (ID(yytext, !yylineno, !yylineno));
<DEFS> "="	=> (YYBEGIN RE; EQ(!yylineno, !yylineno));

<RE> {ws}	=> (lex());
<RE> "?"	=> (QMARK(!yylineno, !yylineno));
<RE> "*"	=> (STAR(!yylineno, !yylineno));
<RE> "+"	=> (PLUS(!yylineno, !yylineno));
<RE> "|"	=> (BAR(!yylineno, !yylineno));
<RE> "("	=> (LP(!yylineno, !yylineno));
<RE> ")"	=> (RP(!yylineno, !yylineno));
<RE> "$"	=> (DOLLAR(!yylineno, !yylineno));
<RE> "/"	=> (SLASH(!yylineno, !yylineno));
<RE> "."	=> (DOT(!yylineno, !yylineno));
<RE>"^"		=> (CARAT(!yylineno, !yylineno));

<RE> "{"	=> (YYBEGIN RECB; lex());
<RE> "\""       => (YYBEGIN STRING; lex());
<RE> "["	=> (YYBEGIN CHARCLASS; LB(!yylineno, !yylineno));
<RE> "<"	=> (YYBEGIN LEXSTATES; LT(!yylineno, !yylineno));
<RE> ">"	=> (GT(!yylineno, !yylineno));
<RE> "=>" {ws}*	"("
		=> (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN RE);
		    ARROW(!yylineno, !yylineno));
<RE> ";"	=> (YYBEGIN DEFS; SEMI(!yylineno, !yylineno));

<RECB>{ws}	=> (lex());
<RECB>{id}	=> (ID(yytext, !yylineno, !yylineno));
<RECB>{num}+	=> (REPS(valOf (Int.fromString yytext), !yylineno, !yylineno));
<RECB>","	=> (COMMA(!yylineno, !yylineno));
<RECB>"}"	=> (YYBEGIN RE; RCB(!yylineno, !yylineno));

<CHARCLASS>"-]"	=> (YYBEGIN RE; RBD(!yylineno, !yylineno));
<CHARCLASS>"]"	=> (YYBEGIN RE; RB(!yylineno, !yylineno));
<CHARCLASS>"-"	=> (DASH(!yylineno, !yylineno));
<CHARCLASS>"^"	=> (CARAT(!yylineno, !yylineno));

<STRING> "\""	=> (YYBEGIN RE; lex());

<RE,STRING,CHARCLASS>"\\" ({num}{3} | [btnr] | "\\" | "\"")
		=> (CHAR(valOf (String.fromString yytext), !yylineno, !yylineno));
<RE,STRING,CHARCLASS>"\\u"{hex}{4}
		=> (UNICHAR(uniChar yytext, !yylineno, !yylineno));
<RE,STRING,CHARCLASS>"\\h"
		=> (HIGH_CHAR(!yylineno, !yylineno));
<RE,STRING,CHARCLASS>"\\".
		=> (CHAR(String.substring (yytext, 1, 1), !yylineno, !yylineno));
<RE,STRING,CHARCLASS>.
		=> (CHAR(yytext, !yylineno, !yylineno));

<LEXSTATES>{id} => (LEXSTATE(yytext, !yylineno, !yylineno));
<LEXSTATES>{ws}	=> (lex());
<LEXSTATES> "," => (COMMA(!yylineno, !yylineno));
<LEXSTATES> ">" => (YYBEGIN RE; GT(!yylineno, !yylineno));
<LEXSTATES> ";" => (YYBEGIN DEFS; SEMI(!yylineno, !yylineno));

<ACTION> ";"	=> (if !pcount = 0
		    then ((!afterAction)();
			  ACT(getAction(), !yylineno, !yylineno))
		    else (updAction ";"; lex()));
<ACTION> "("	=> (updAction "("; inc pcount; lex());
<ACTION> ")"	=> (updAction ")"; dec pcount; lex());
<ACTION> "\\\"" => (updAction "\\\""; lex());
<ACTION> "\\\\"	=> (updAction "\\\\"; lex());
<ACTION> "\\"	=> (updAction "\\"; lex());
<ACTION> "\""   => (updAction "\""; inquote := not (!inquote); lex());
<ACTION> [^;()\"\\]*
		=> (updAction yytext; lex());

.       => (print (concat[
		"[", Int.toString (!yylineno), "] Illegal character '",
		String.toCString yytext, "'\n"
	      ]);
            continue());
