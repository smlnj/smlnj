(* test-pp.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Test the pretty printing of S-Expressions.
 *)

structure TestPP =
  struct

    local
      structure S = SExp
      structure PP = TextIOPP

      fun pr wid sexp = let
	    val ppStrm = PP.openOut{dst = TextIO.stdOut, wid=wid}
	    in
	      SExpPP.output (ppStrm, sexp);
	      PP.closeStream ppStrm
	    end

      fun list items = S.LIST(S.SYMBOL(Atom.atom "list") :: items)
    in
  (* a large list *)
    fun prList wid = pr wid (list (List.tabulate(100, fn i => S.INT(IntInf.fromInt i))))
  (*  list of lists *)
    fun prListOfLists wid = let
	  fun mkList n = list (List.tabulate(n, fn i => S.INT(IntInf.fromInt i)))
	  in
	    pr wid (list (List.tabulate(50, mkList)))
	  end
    end (* local *)

  end
