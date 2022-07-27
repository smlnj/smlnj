(* ml-ulex-input-bootstrap.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-ulex input format.
 *)

(***** ADAPTED FROM ML-YACC *****)
structure Streamify =
struct
   datatype str = EVAL of Tok.token * strm | UNEVAL of (unit -> Tok.token)
   and strm = STRM of str ref

   fun lex(STRM (ref(EVAL t))) = SOME t
     | lex(STRM (s as ref(UNEVAL f))) = let
	 val tok = f()
         val t = (tok, STRM(ref(UNEVAL f)))
         in
	   case tok
	    of Tok.EOF => NONE
	     | _ => (s := EVAL t; SOME(t))
         end

   fun streamify f = STRM(ref(UNEVAL f))
   fun cons(a,s) = STRM(ref(EVAL(a,s)))

end

structure MLULexInput : INPUT =
  struct

    structure P = Parser(Streamify)

    fun parseFile fname = let
	  val strm = TextIO.openIn fname
	  val lexer = MLULexLex.makeLexer (fn n => TextIO.inputN (strm, n))
	  val (spec, errors) = P.parse (Streamify.streamify lexer)
			       before TextIO.closeIn strm
	  fun errMsg (pos, err) = print (P.repairToString err ^ "\n")
	  in
	    if (null errors)
	      then SOME spec
	      else (app errMsg errors; NONE)
	    spec
	  end

  end
