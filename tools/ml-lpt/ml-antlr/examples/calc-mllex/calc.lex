(* calc.lex
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

datatype lexresult= DIV | EOF | EOS | ID of string | LPAREN |
                     NUM of int | PLUS | PRINT | RPAREN | SUB | TIMES

val linenum = ref 1
val error = fn x => output(std_out,x ^ "\n")
val eof = fn () => EOF
%%
%structure CalcLex
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (inc linenum; lex());
{ws}+    => (lex());
"/"      => (DIV);
";"      => (EOS);
"("      => (LPAREN);
{digit}+ => (NUM(valOf(Int.fromString yytext)));
")"      => (RPAREN);
"+"      => (PLUS);
{alpha}+ => (if yytext="print" then PRINT else ID yytext);
"-"      => (SUB);
"*"      => (TIMES);
.        => (error ("calc: ignoring bad character "^yytext); lex());
