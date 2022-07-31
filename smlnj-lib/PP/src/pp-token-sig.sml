(* pp-token-sig.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * User-defined pretty-printer tokens.  Tokens pair text with style information
 * for convinence.  For example, one could define tokens for the keywords of
 * a language.
 *)

signature PP_TOKEN =
  sig
    type token
    type style

  (* returns the text of the token *)
    val string : token -> string
  (* returns the associated style of the token *)
    val style  : token -> style
  (* returns the size of the token's text *)
    val size   : token -> int

  end;

