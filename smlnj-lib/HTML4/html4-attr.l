(* html4-attr.l
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Lexer for parsing HTML 4 attributes.
 *)

%name HTML4AttrLexer;

%defs (
open HTML4AttrTokens

fun eof() = EOF
type lex_result = token
);

%let alpha=[A-Za-z];
%let digit=[0-9];
%let ws=[\ \t\r\n];

(* some XHTML attributes have ":" in their name *)
{alpha}({alpha}|{digit}|[-.:])* => (NAME (Atom.atom yytext));
{digit}+ => (NUMBER yytext);
"=" => (EQUALS);
"." => (DOT);
[\"][^\"]*[\"] => (STRINGLIT yytext);
[\'][^\']*[\'] => (STRINGLIT yytext);

{ws}+ => (continue());

. => ((* error; invalid character *) continue());

(* ______________________________________________________________________
   End of html4-attr.l
   ______________________________________________________________________ *)