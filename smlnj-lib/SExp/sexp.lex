(* sexp.lex
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
 *
 * Lexer for Sexp files.
 *
 * TODO:
 *	EOF rules for strings
 *	error messages for unknown characters
 *)

%name SExpLexer;

%defs (
  structure T = SExpTokens
  type lex_result = T.token
  fun eof () = T.EOF
  fun int s = T.INT(valOf(IntInf.fromString s))
  fun float s = T.FLOAT(valOf(LargeReal.fromString s))
(* support for incremental construction of strings *)
  val sbuf : string list ref = ref []
  fun addStr s = (sbuf := s :: !sbuf)
  fun addHexEsc lit = let
      (* trim the "\x" prefix and ";" suffix *)
	val digits = Substring.trimr 1 (Substring.triml 2 lit)
	val SOME(d, _) = Int.scan StringCvt.HEX Substring.getc digits
	in
	  addStr(String.str(chr d))
	end
  fun finishString () = (T.STRING(String.concat(List.rev(!sbuf))) before sbuf := [])
);

%let digit1_9 = [1-9];
%let digit = [0-9];
%let digits = {digit}+;
%let int = [+-]?({digit} | {digit1_9}{digits}+);
%let frac = "."{digits};
%let exp = [eE][+-]?{digits};
%let xdigit = {digit}|[a-fA-F];
%let alpha = ([a-zA-Z]);
%let initial = {alpha} | "^" | [-+.@!$%&*/:<=>?_~];
%let subsequent = {initial} | {digit};
%let ident = {initial} {subsequent}*;
%let interlnws = " "|"\t";      (* intraline whitespace *)
%let eol = "\n"|"\r\n"|"\r";

%states S;

<INITIAL>[ \t\n\r]+		=> ( T.WHITE );
<INITIAL>";"[^\n\r]*[\n\r]+	=> ( skip() (* comment *));

<INITIAL>{ident}		=> ( T.SYMBOL (yytext) );

<INITIAL>"'"			=> ( T.QUOTE );
<INITIAL>"("			=> ( T.DELIM (T.PAREN, T.OPEN) );
<INITIAL>")"			=> ( T.DELIM (T.PAREN, T.CLOSE) );
<INITIAL>"["			=> ( T.DELIM (T.BRACKET, T.OPEN) );
<INITIAL>"]"			=> ( T.DELIM (T.BRACKET, T.CLOSE) );
<INITIAL>"{"			=> ( T.DELIM (T.BRACE, T.OPEN) );
<INITIAL>"}"			=> ( T.DELIM (T.BRACE, T.CLOSE) );
<INITIAL>"#t"			=> ( T.KW_true );
<INITIAL>"#f"			=> ( T.KW_false );

(* takes a string of form "0xdeadbeef", strips the leading "0x", and returns
 * an IntInf with hex value deadbeef.  Note that the hex value is unsigned; to
 * get negatives, write "-0xdeadbeef".  This means that the string from C's
 * `printf("%x", -1)` will be parsed as INT_MAX.  TODO is this a good idea?
 *)
<INITIAL>[+-]?"0x"{xdigit}+      => (
    let
    (* TODO Doesn't StringCvt.HEX handle stripping the "0x" prefix? *)
    val digits = if String.isPrefix "+" yytext         (* "+0xdeadbeef" *)
            then String.extract(yytext, 3, NONE)
          else if String.isPrefix "-" yytext            (* "-0xdeadbeef" *)
            then "-" ^ String.extract(yytext, 3, NONE)
            else String.extract(yytext, 2, NONE)        (* "0xdeadbeef" *)
    val SOME(value) = StringCvt.scanString (IntInf.scan StringCvt.HEX) digits
    in
      T.INT(value)
    end);

<INITIAL>{int}                  => ( T.INT(valOf(IntInf.fromString yytext)) );

<INITIAL>{int}{frac}		=> ( float yytext );
<INITIAL>{int}{exp}		=> ( float yytext );
<INITIAL>{int}{frac}{exp}	=> ( float yytext );

(* string values follow the syntax of Scheme as described in
 *
 *      https://www.scheme.com/tspl4/grammar.html#./grammar:strings
 *)
<INITIAL>"\""			=> ( YYBEGIN S; continue() );

<S>"\\\\"                       => ( addStr "\\"; continue() );
<S>"\\\""			=> ( addStr "\""; continue() );
<S>"\\a"			=> ( addStr "\a"; continue() );
<S>"\\b"			=> ( addStr "\b"; continue() );
<S>"\\f"			=> ( addStr "\f"; continue() );
<S>"\\n"			=> ( addStr "\n"; continue() );
<S>"\\r"			=> ( addStr "\r"; continue() );
<S>"\\t"			=> ( addStr "\t"; continue() );
<S>"\\v"			=> ( addStr "\v"; continue() );
<S>"\\x"{xdigit}";"		=> ( addHexEsc yysubstr; continue() );
<S>"\\x"{xdigit}{2}";"		=> ( addHexEsc yysubstr; continue() );
<S>[^\\"]+			=> ( addStr yytext; continue() );
<S>"\\"{interlnws}*{eol}{interlnws}*
                                => ( continue() );
<S>"\""				=> ( YYBEGIN INITIAL; finishString() );
(* FIXME: add some error reporting *)
<S>.                            => ( continue() );

(* FIXME: add some error reporting *)
<INITIAL>.			=> ( skip() );
