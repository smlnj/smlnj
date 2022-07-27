(* ebnf.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * EBNF combinators used for ml-antlr.
 *
 * NOTE: this functor is no longer used; instead we have inlined its code into the
 * body of the generated parser module.  It remains in the ml-lpt library to
 * preserve backward compatibility with parsers that were generated with an older
 * version of the ml-antlr tool.  [JHR; 2015-09-22]
 *)

functor AntlrEBNF (S : sig 
		     type strm
		     val getSpan : strm -> AntlrStreamPos.span
    	           end) = 
struct

  fun optional (pred, parse, strm) = 
        if pred strm
    	then let
	  val (y, span, strm') = parse strm
	  in 
	    (SOME y, span, strm')
	  end
	else (NONE, S.getSpan strm, strm)

  fun closure (pred, parse, strm) = let
        fun iter (strm, (left, right), ys) = 
	      if pred strm
	      then let
		val (y, (_, right'), strm') = parse strm
		in iter (strm', (left, right'), y::ys)
		end
	      else (List.rev ys, (left, right), strm)
        in
          iter (strm, S.getSpan strm, [])
        end

  fun posclos (pred, parse, strm) = let
        val (y, (left, _), strm') = parse strm
	val (ys, (_, right), strm'') = closure (pred, parse, strm')
        in
          (y::ys, (left, right), strm'')
        end

end