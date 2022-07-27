%defs (
  open Tokens;
  fun eof() = EOF
  type lex_result = token
);

%let eol=("\n"|"\013\n"|"\013");
%let ws=("\009"|"\011"|"\012"|" "|{eol});
%let alpha=[a-zA-Z];
%let digit=[0-9];
%let int={digit}+;
%let idchars=({alpha}|{digit}|"_");
%let id={alpha}{idchars}*;

{ws}+		=> (skip());
"{" [^}]* "}"	=> (skip());

"program"	=> (KW_program);
"("		=> (LP);
")"		=> (RP);
";"		=> (SEMI);
"."		=> (DOT);
","		=> (COMMA);
"var"		=> (KW_var);
":"		=> (COLON);
"array"		=> (KW_array);
"["		=> (LSB);
"]"		=> (RSB);
"of"		=> (KW_of);
"integer"	=> (KW_integer);
"real"		=> (KW_real);
"function"	=> (KW_function);
"procedure"	=> (KW_procedure);
"begin"		=> (KW_begin);
"end"		=> (KW_end);
":="		=> (ASSIGNOP);
"if"		=> (KW_if);
"then"		=> (KW_then);
"else"		=> (KW_else);
"while"		=> (KW_while);
"do"		=> (KW_do);
"=" | "<>" | "<" | "<=" | ">=" | ">"		
		=> (RELOP yytext);
"+" | "-" | "or"
		=> (ADDOP yytext);
"*" | "/" | "div" | "mod" | "and"
		=> (MULOP yytext);
"not"		=> (KW_not);
"-"		=> (MINUS);

{id}		=> (ID yytext);
{int}		=> (INT (valOf (IntInf.fromString yytext)));

.		=> (print (concat ["Unexpected character: '", yytext,
			           "'\n"]); continue());