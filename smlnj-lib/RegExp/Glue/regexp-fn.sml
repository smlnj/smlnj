(* regexp-fn.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Functor that implements a regular expressions matcher by combining
 * a surface syntax and a matching engine.
 *)

functor RegExpFn (
    structure P : REGEXP_PARSER
    structure E : REGEXP_ENGINE
  ) :> REGEXP where type regexp = E.regexp = struct

    structure M = MatchTree

    type regexp = E.regexp

  (* a match specifies the position (as a stream) and the length of the match *)
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree

    exception CannotParse

    fun compile reader s = (case (P.scan reader s)
	   of SOME (syntax, s') => SOME(E.compile syntax, s')
	    | NONE => NONE
	  (* end case *))

    fun compileString str = (case (StringCvt.scanString P.scan str)
	   of SOME syntax => E.compile syntax
	    | NONE => raise CannotParse
	  (* end case *))

    val prefix = E.prefix
    val find = E.find

    fun match l = let
	  fun parse (s, f) = (case (StringCvt.scanString P.scan s)
		 of SOME r => (r, f)
		  | NONE => raise CannotParse
		(* end case *))
	  val m = E.match (map parse l)
	  in
	    fn getc => fn stream => m getc stream
	  end

  end
