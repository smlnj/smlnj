(* antlr-lexer-sig.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Signature for the lexer argument to parser functors generated
 * by ml-antlr.
 *)

signature ANTLR_LEXER = sig

  type strm
  type pos = AntlrStreamPos.pos

  val getPos : strm -> pos

end