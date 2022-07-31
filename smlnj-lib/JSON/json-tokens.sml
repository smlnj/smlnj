(* json-tokens.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The tokens returned by the JSON lexer.
 *)

structure JSONTokens =
  struct

    datatype token
      = EOF		(* end-of-file *)
      | LB | RB		(* "[" "]" *)
      | LCB | RCB	(* "{" "}" *)
      | COMMA		(* "," *)
      | COLON		(* ":" *)
      | KW_null		(* "null" *)
      | KW_true		(* "true" *)
      | KW_false	(* "false" *)
      | INT of IntInf.int
      | FLOAT of real
      | STRING of string
      | ERROR of string list

    fun toString EOF = "<eof>"
      | toString LB = "["
      | toString RB = "]"
      | toString LCB = "{"
      | toString RCB = "}"
      | toString COMMA = ","
      | toString COLON = ":"
      | toString KW_null = "null"
      | toString KW_true = "true"
      | toString KW_false = "false"
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
      | toString (ERROR msg) = "<error>" (* default behavior should be overridden *)

  end
