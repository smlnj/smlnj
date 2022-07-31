(* sexp-tokens.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Damon Wang (with modifications by John Reppy)
s *
 * The tokens returned by the SExp lexer.
 *)

structure SExpTokens =
  struct

    datatype delim_type = PAREN | BRACKET | BRACE
    datatype delim_open = OPEN | CLOSE

    datatype token
      = EOF		(* end-of-file *)
      | DELIM of (delim_type * delim_open)
      | QUOTE		(* "'" *)
      | KW_true		(* "#t" *)
      | KW_false	(* "#f" *)
      | INT of IntInf.int
      | FLOAT of real
      | STRING of string
      | WHITE       (* whitespace, which separates list items *)
      | SYMBOL of string
      (* TODO: add a HEX constructor for encoding non-printable characters in
        * some human-readable way *)

    fun toString EOF = "<eof>"
      | toString (DELIM(PAREN, OPEN)) = "("
      | toString (DELIM(PAREN, CLOSE)) = ")"
      | toString (DELIM(BRACKET, OPEN)) = "["
      | toString (DELIM(BRACKET, CLOSE)) = "]"
      | toString (DELIM(BRACE, OPEN)) = "{"
      | toString (DELIM(BRACE, CLOSE)) = "}"
      | toString QUOTE = "'"
      | toString KW_true = "#t"
      | toString KW_false = "#f"
      | toString (INT i) =
	  if (i < 0) then "-" ^ IntInf.toString(~i)
	  else IntInf.toString i
      | toString (FLOAT f) =
	  if (f < 0.0) then "-" ^ Real.toString(~f)
	  else Real.toString f
      | toString (STRING s) = let
	  fun f (wchr, l) = UTF8.toString wchr :: l
	  in
	    String.concat("\"" :: (List.foldr f ["\""] (UTF8.explode s)))
	  end
      | toString (SYMBOL str) = str
      | toString WHITE = " "

  end
