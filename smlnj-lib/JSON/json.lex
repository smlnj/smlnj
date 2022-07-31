(* json.lex
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Lexer for JSON files.
 *
 * TODO:
 *	EOF rules for strings
 *	newlines in strings
 *	error messages for unknown characters
 *)

%name JSONLexer;

%defs (
  structure T = JSONTokens
  type lex_result = T.token
  fun eof () = T.EOF
  fun int s = T.INT(valOf(IntInf.fromString s))
  fun float s = T.FLOAT(valOf(LargeReal.fromString s))
(* support for incremental construction of strings *)
  val sbuf : string list ref = ref []
  fun addStr s = sbuf := s :: !sbuf
  fun addUChr lit = let
      (* trim the "\u" prefix *)
	val digits = Substring.triml 2 (Substring.full lit)
	val SOME(w, _) = Word.scan StringCvt.HEX Substring.getc digits
	in
	  addStr(UTF8.encode w)
	end
  fun finishString () = (T.STRING(String.concat(List.rev(!sbuf))) before sbuf := [])
);

%let digit1_9 = [1-9];
%let digit = [0-9];
%let digits = {digit}+;
%let int = "-"?({digit} | {digit1_9}{digits}+);
%let frac = "."{digits};
%let exp = [eE][+-]?{digits};
%let xdigit = {digit}|[a-fA-F];

%states S;

<INITIAL>[\ \t\n\r]+		=> ( skip() );

<INITIAL>"{"			=> ( T.LCB );
<INITIAL>"}"			=> ( T.RCB );
<INITIAL>"["			=> ( T.LB );
<INITIAL>"]"			=> ( T.RB );
<INITIAL>","			=> ( T.COMMA );
<INITIAL>":"			=> ( T.COLON );
<INITIAL>"null"			=> ( T.KW_null );
<INITIAL>"true"			=> ( T.KW_true );
<INITIAL>"false"		=> ( T.KW_false );

<INITIAL>{int}			=> ( T.INT(valOf(IntInf.fromString yytext)) );

<INITIAL>{int}{frac}		=> ( float yytext );
<INITIAL>{int}{exp}		=> ( float yytext );
<INITIAL>{int}{frac}{exp}	=> ( float yytext );

<INITIAL>"\""			=> ( YYBEGIN S; continue() );
<S>"\\\\"			=> ( addStr "\\"; continue() );
<S>"\\\""			=> ( addStr "\""; continue() );
<S>"\\/"			=> ( addStr "/"; continue() );
<S>"\\b"			=> ( addStr "\b"; continue() );
<S>"\\f"			=> ( addStr "\f"; continue() );
<S>"\\n"			=> ( addStr "\n"; continue() );
<S>"\\r"			=> ( addStr "\r"; continue() );
<S>"\\t"			=> ( addStr "\t"; continue() );
<S>"\\u"{xdigit}{4}		=> ( addUChr yytext; continue() );
<S>[^\\"]+			=> ( addStr yytext; continue() );
<S>"\""				=> ( YYBEGIN INITIAL; finishString() );

<INITIAL>"/*"(~(.*"*/".*))"*/"	=> ( skip() );

<INITIAL>. => ( T.ERROR["bad character '", String.toString yytext, "'"]);
