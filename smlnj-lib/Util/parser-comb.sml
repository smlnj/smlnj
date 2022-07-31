(* parser-comb.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Parser combinators over readers.  These are modeled after the Haskell
 * combinators of Hutton and Meijer.  The main difference is that they
 * return a single result, instead of a list of results.  This means that
 * "or" is a committed choice; once one branch succeeds, the others will not
 * be enabled.  While this is somewhat limiting, for many applications it
 * will not be a problem.  For more substantial parsing problems, one should
 * use a parser generator like ML-Antlr, ML-Yacc and/or ML-Lex.
 *)

structure ParserComb : PARSER_COMB =
  struct
    structure SC = StringCvt

    type ('a, 'strm) parser = (char, 'strm) SC.reader -> ('a, 'strm) SC.reader

    fun result v getc strm = SOME(v, strm)

    fun failure getc strm = NONE

    fun wrap (p, f) getc strm = (case (p getc strm)
	   of NONE => NONE
	    | (SOME(x, strm)) => SOME(f x, strm)
	  (* end case *))

    fun seqWith f (p1, p2) getc strm = (case (p1 getc strm)
	   of SOME(t1, strm1) => (case (p2 getc strm1)
		 of SOME(t2, strm2) => SOME(f(t1, t2), strm2)
		  | NONE => NONE
		(* end case *))
	    | NONE => NONE
	  (* end case *))
    fun seq (p1, p2) = seqWith (fn x => x) (p1, p2)

    fun bind (p1, p2') getc strm = (case (p1 getc strm)
	   of SOME(t1, strm1) => p2' t1 getc strm1
	    | NONE => NONE
	  (* end case *))

    fun eatChar pred getc strm = (case getc strm
	   of (res as SOME(c, strm')) => if (pred c) then res else NONE
	    | _ => NONE
	  (* end case *))

    fun char (c: char) = eatChar (fn c' => (c = c'))

    fun string s getc strm = let
	  fun eat (ss, strm) = (case (Substring.getc ss, getc strm)
		 of (SOME(c1, ss'), SOME(c2, strm')) =>
		      if (c1 = c2) then eat(ss', strm') else NONE
		  | (NONE, _) => SOME(s, strm)
		  | _ => NONE
		(* end case *))
	  in
	    eat (Substring.full s, strm)
	  end

    fun skipBefore pred p getc strm = let
	  fun skip' strm = (case getc strm
		 of NONE => NONE
		  | SOME(c, strm') =>
		      if (pred c) then skip' strm' else p getc strm
		(* end case *))
	  in
	    skip' strm
	  end

    fun or (p1, p2) getc strm = (case (p1 getc strm)
	   of NONE => (case (p2 getc strm)
		 of NONE => NONE
		  | res => res
		(* end case *))
	    | res => res
	  (* end case *))

    fun or' l getc strm = let
	  fun tryNext [] = NONE
	    | tryNext (p::r) = (case (p getc strm)
		 of NONE => tryNext r
		  | res => res
		(* end case *))
	  in
	    tryNext l
	  end

    fun zeroOrMore p getc strm = let
	  val p = p getc
	  fun parse (l, strm) = (case (p strm)
		 of (SOME(item, strm)) => parse (item::l, strm)
		  | NONE => SOME(rev l, strm)
		(* end case *))
	  in
	    parse ([], strm)
	  end

    fun oneOrMore p getc strm = (case (zeroOrMore p getc strm)
	   of (res as (SOME(_::_, _))) => res
	    | _ => NONE
	  (* end case *))

    fun option p getc strm = (case (p getc strm)
	   of SOME(x, strm) => SOME(SOME x, strm)
	    | NONE => SOME(NONE, strm)
	  (* end case *))

    fun join p = bind (p, fn (SOME x) => result x | NONE => failure)

  (* parse a token consisting of characters satisfying the predicate.
   * If this succeeds, then the resulting string is guaranteed to be
   * non-empty.
   *)
    fun token pred getc strm = (case (zeroOrMore (eatChar pred) getc strm)
	   of (SOME(res as _::_, strm)) => SOME(implode res, strm)
	    | _ => NONE
	  (* end case *))

  end;
