(* reg-exp-fn.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Regular expression representation and manipulation.
 *
 * The main points here are to:
 *   (1) make it easy for an RE parser to construct
 *       RE expressions
 *   (2) canonicalize REs for effective comparison
 *   (3) implement the RE derivatives algorithm
 *
 * See the implementation notes for details on the derivatives
 * algorithm and the canonicalization strategy.
 *)

structure RegExp : REG_EXP =
  struct

    structure W = Word

  (* symbols (i.e., words) *)
    structure Sym =
      struct

        type point = W.word

	val compare = W.compare
	val minPt : W.word = 0w0
	val maxPt = W.notb 0w0

	fun succ (w : W.word) =
	      if w = W.notb 0w0 then w
	      else w + 0w1
	fun pred (w : W.word) =
	      if w = 0w0 then w
	      else w - 0w1

	fun isSucc (w1, w2) = (succ w1 = w2)

      end

    structure SymSet = IntervalSetFn(Sym)

    type symbol = Sym.point
    type sym_set = SymSet.set

    structure SIS = SymSet

  (* REs *)
    datatype re
      = Epsilon                 (* matches the empty string *)
      | Any			(* matches any single symbol *)
      | None			(* matches nothing (i.e. the empty language) *)
      | SymSet of sym_set
      | Concat of re list
      | Closure of re
      | Op of (rator * re list)	(* list length <> 1 and in sorted order *)
      | Not of re
    and rator = OR | AND | XOR

  (* we give a total order to REs; this is useful for canonicalization *)
    fun compare (re1, re2) = let
	  fun cmpOp (OR, OR) = EQUAL
	    | cmpOp (OR, _) = LESS
	    | cmpOp (_, OR) = GREATER
	    | cmpOp (AND, AND) = EQUAL
	    | cmpOp (AND, _) = LESS
	    | cmpOp (_, AND) = GREATER
	    | cmpOp (XOR, XOR) = EQUAL
	  fun compareList (res1, res2) = List.collate compare (res1, res2)
	  in
	    case (re1, re2)
	     of (Epsilon, Epsilon) => EQUAL
	      | (Epsilon, _) => LESS
	      | (_, Epsilon) => GREATER
	      | (Any, Any) => EQUAL
	      | (Any, _) => LESS
	      | (_, Any) => GREATER
	      | (None, None) => EQUAL
	      | (None, _) => LESS
	      | (_, None) => GREATER
	      | (SymSet a, SymSet b) => SIS.compare(a, b)
	      | (SymSet a, _) => LESS
	      | (_, SymSet b) => GREATER
	      | (Concat a, Concat b) => compareList(a, b)
	      | (Concat a, _) => LESS
	      | (_, Concat b) => GREATER
	      | (Closure a, Closure b) => compare(a, b)
	      | (Closure a, _) => LESS
	      | (_, Closure b) => GREATER
	      | (Op(op1, res1), Op(op2, res2)) => (case cmpOp (op1, op2)
		   of EQUAL => compareList(res1, res2)
		    | order => order
		  (* end case *))
	      | (Op _, _) => LESS
	      | (_, Op _) => GREATER
	      | (Not a, Not b) => compare(a, b)
	    (* end case *)
	  end
(*  val sort = ListMergeSort.sort (fn (re1, re2) => compare(re1, re2) = LESS) *)

  (* primitive REs *)

    val any = Any
    val none = None
    val epsilon = Epsilon

  (* canonical constructors *)

    fun mkSymSet c =
	  if SIS.isEmpty c then None
	  else if SIS.isUniverse c then Any
	  else SymSet c

    fun mkSym sym = mkSymSet (SIS.singleton sym)

    fun mkConcat (re1, re2) = (case (re1, re2)
	   of (Epsilon, re2) => re2
	    | (re1, Epsilon) => re1
	    | (None, _) => None
	    | (_, None) => None
	    | (Concat res1, Concat res2) => Concat(res1@res2)
	    | (re1, Concat res2) => Concat(re1::res2)
	    | (Concat res1, re2) => Concat(res1@[re2])
	    | _ => Concat[re1, re2]
	  (* end case *))

    fun mkConcatList [] = Epsilon
      | mkConcatList (re::res) = mkConcat(re, mkConcatList res)

    fun mkClosure (Epsilon) = Epsilon
      | mkClosure (None) = Epsilon
      | mkClosure (re as Closure _) = re
      | mkClosure re = Closure re

    fun mergeSIS (inRes, mop) = let
          fun isSIS (SymSet _) = true
	    | isSIS _ = false
	  val (siss, res) = List.partition isSIS inRes
	  in
            case siss
	      of []   => inRes
	       | [re] => inRes
	       | sis::siss' => let
		   fun wrapmop (SymSet s1, SymSet s2) = SymSet (mop (s1, s2))
		     | wrapmop _ = raise Fail "BUG: wrapmop: SymSet expected"
		   val merged = List.foldl wrapmop sis siss'
		   fun reinsert (re1, []) = [re1]
		     | reinsert (re1, re::res) = (case compare (re1, re)
			 of LESS => re1::re::res
			  | EQUAL => raise Fail "BUG: mergeSIS: only one SymSet expected"
			  | GREATER => re::(reinsert (re1, res))
			(* end case *))
		   in
                     reinsert (merged, res)
	           end
            (* end case *)
	  end

    fun mkOr (re1, re2) = let
	  fun merge ([], res2) = res2
	    | merge (res1, []) = res1
	    | merge (re1::r1, re2::r2) = (case compare(re1, re2)
		 of LESS => re1::merge(r1, re2::r2)
		  | EQUAL => merge (re1::r1, r2)
		  | GREATER => re2 :: merge(re1::r1, r2)
		(* end case *))
	  fun mk (a, b) = (case mergeSIS(merge(a, b), SIS.union)
		 of [] => None
		  | [re] => re
		  | res => Op(OR, res)
		(* end case *))
	  in
	    case (re1, re2)
	     of (None, _) => re2
	      | (_, None) => re1
	      | (SymSet s1, SymSet s2) => mkSymSet (SIS.union (s1, s2))
	      | (Op(OR, res1), Op(OR, res2)) => mk(res1, res2)
	      | (Op(OR, res1), _) => mk(res1, [re2])
	      | (_, Op(OR, res2)) => mk([re1], res2)
	      | (re1, re2) => (case compare(re1, re2)
		   of LESS => Op(OR, [re1, re2])
		    | EQUAL => re1
		    | GREATER => Op(OR, [re2, re1])
		  (* end case *))
	    (* end case *)
	  end

    fun mkAnd (re1, re2) = let
	  fun merge ([], res2) = res2
	    | merge (res1, []) = res1
	    | merge (re1::r1, re2::r2) = (case compare(re1, re2)
		 of LESS => re1::merge(r1, re2::r2)
		  | EQUAL => merge (re1::r1, r2)
		  | GREATER => re2 :: merge(re1::r1, r2)
		(* end case *))
	  fun mk (a, b) = (case mergeSIS(merge(a, b), SIS.intersect)
		 of [] => None
		  | [re] => re
		  | res => Op(AND, res)
		(* end case *))
	  in
	    case (re1, re2)
	     of (None, _) => None
	      | (_, None) => None
	      | (SymSet s1, SymSet s2) => mkSymSet (SIS.intersect (s1, s2))
	      | (Op(AND, res1), Op(AND, res2)) => mk(res1, res2)
	      | (Op(AND, res1), _) => mk(res1, [re2])
	      | (_, Op(AND, res2)) => mk([re1], res2)
	      | (re1, re2) => (case compare(re1, re2)
		   of LESS => Op(AND, [re1, re2])
		    | EQUAL => re1
		    | GREATER => Op(AND, [re2, re1])
		  (* end case *))
	    (* end case *)
	  end

    fun mkXor (re1, re2) = let
	  fun merge ([], res2) = res2
	    | merge (res1, []) = res1
	    | merge (re1::r1, re2::r2) = (case compare(re1, re2)
		 of LESS => re1::merge(r1, re2::r2)
		  | EQUAL => merge (r1, r2)
		  | GREATER => re2 :: merge(re1::r1, r2)
		(* end case *))
	  fun mk (a, b) = (case merge(a, b)
		 of [] => None
		  | [re] => re
		  | res => Op(XOR, res)
		(* end case *))
	  in
	    case (re1, re2)
	     of (None, _) => re2
	      | (_, None) => re1
	      | (SymSet s1, SymSet s2) =>
		  mkSymSet (SIS.intersect (
		      SIS.union (s1, s2),
		      SIS.complement (SIS.intersect (s1, s2))
                    ))
	      | (Op(XOR, res1), Op(XOR, res2)) => mk(res1, res2)
	      | (Op(XOR, res1), _) => mk(res1, [re2])
	      | (_, Op(XOR, res2)) => mk([re1], res2)
	      | (re1, re2) => (case compare(re1, re2)
		   of LESS => Op(XOR, [re1, re2])
		    | EQUAL => None (* FIXME is this right? *)
		    | GREATER => Op(XOR, [re2, re1])
		  (* end case *))
	    (* end case *)
	  end

    fun mkOp (OR, re1, re2) = mkOr(re1, re2)
      | mkOp (AND, re1, re2) = mkAnd(re1, re2)
      | mkOp (XOR, re1, re2) = mkXor(re1, re2)

    fun mkNot (Not re) = re
      | mkNot (None) = mkClosure(Any)
      | mkNot re = Not re

    fun mkOpt re = mkOr(Epsilon, re)

    fun mkRep (re, low, high) = let
          fun lowReps 0 = Epsilon
	    | lowReps 1 = re
	    | lowReps n = mkConcat (re, lowReps (n-1))
	  fun highReps 0 = Epsilon
	    | highReps 1 = mkOpt re
	    | highReps n = mkConcat (mkOpt re, highReps (n-1))
          in
            if high < low
              then raise Subscript
              else mkConcat (lowReps low, highReps (high - low))
          end

    fun mkAtLeast (re, 0) = mkClosure re
      | mkAtLeast (re, n) = mkConcat (re, mkAtLeast (re, n-1))

    fun isNone None = true
      | isNone _    = false

    fun symToString w = if !Options.lexCompat
	  then concat["#\"", Char.toString (Char.chr (W.toInt w)), "\""]
	    handle Overflow => raise Fail "(BUG) RegExp: symToString on a nonascii character"
	  else "0wx" ^ W.toString w

    fun SISToString s = let
          fun c2s c = if (c < 0w128)
		then Char.toString (Char.chr (W.toInt c))
		else "\\u" ^ W.toString c
	  fun f (a, b) = if (a = b)
                then c2s a
	        else concat[c2s a, "-", c2s b]
	(* we want to describe the interval set as concisely as possible,
	 * so we compare the number of intervals in the set to the number
	 * of intervals in its complement, and use the smaller of the two.
	 *)
	  val intervals = SIS.intervals s
	  val intervals' = SIS.intervals (SIS.complement s)
	  val (neg, rngs) = if List.length intervals <= List.length intervals'
		then ("", intervals)
		else ("^", intervals')
	  val str = neg ^ (String.concatWithMap "" f rngs)
          in
	    if String.size str <= 1
	      then str
	      else concat["[", str, "]"]
          end

    fun toString re = let
          fun opToString OR = "|"
	    | opToString AND = "&"
	    | opToString XOR = "^"
	  fun opPrec OR = 0
	    | opPrec AND = 2
	    | opPrec XOR = 1
	  fun prec Any = 6
	    | prec None = 6
	    | prec Epsilon = 6
	    | prec (SymSet _) = 6
	    | prec (Concat[]) = 6
	    | prec (Concat _) = 3
	    | prec (Closure _) = 5
	    | prec (Op(_, [])) = 6
	    | prec (Op(_, [re])) = prec re
	    | prec (Op(rator, _)) = opPrec rator
	    | prec (Not _) = 4
	  fun toS (Any, l) = "{any}" :: l
	    | toS (None, l) = "{none}" :: l
	    | toS (Epsilon, l) = "{epsilon}" :: l
	    | toS (SymSet s, l) = SISToString s :: l
	    | toS (Concat[], l) = "" :: l
	    | toS (Concat[re], l) = toS(re, l)
	    | toS (Concat res, l) = toS'(res, 3, "", l)
	    | toS (Closure re, l) = paren(5, re, "*" :: l)
	    | toS (Op(_, []), l) = "{}" :: l
	    | toS (Op(rator, [re]), l) = toS(re, l)
	    | toS (Op(rator, res), l) = toS'(res, opPrec rator, opToString rator, l)
	    | toS (Not re, l) = "!" :: paren(4, re, l)
	  and toS' ([], p, rator, l) = raise Fail "empty"
	    | toS' (re::r, p, rator, l) =
		paren(p, re, List.foldr
		  (fn (re, l) => rator :: paren(p, re, l))
		    l r)
	  and paren (p, re, l) = if (p <= prec re)
		then toS (re, l)
		else "(" :: toS(re, ")" :: l)
	  in
	    String.concat(toS(re, []))
	  end

  (* true iff epsilon is in the language recognized by the RE *)
    fun nullable Any = false
      | nullable None = false
      | nullable Epsilon = true
      | nullable (SymSet _) = false
      | nullable (Closure _) = true
      | nullable (Concat res) = List.all nullable res
      | nullable (Op(OR, res)) = List.exists nullable res
      | nullable (Op(AND, res)) = List.all nullable res
      | nullable (Op(XOR, re::r)) =
	  (nullable re andalso not(List.exists nullable r))
	    orelse nullable(Op(XOR, r))
      | nullable (Op(XOR, [])) = raise Fail "(BUG) RegExp: RE operator has no operands"
      | nullable (Not re) = not(nullable re)

    fun delta re = if (nullable re) then Epsilon else None

  (* compute derivative w.r.t. a symbol *)
    fun derivative a = let
	  fun da Any = Epsilon
	    | da None = None
	    | da Epsilon = None
	    | da (SymSet s) = if SIS.member(s, a) then Epsilon else None
	    | da (re as Closure re') = mkConcat(da re', re)
	    | da (Concat[]) = None
	    | da (Concat[re]) = da re
	    | da (Concat(re::res)) =
		mkOr(
		  mkConcatList((da re)::res),
		  mkConcat(delta re, da(Concat res)))
	    | da (Op(_, [])) = raise Fail "(BUG) RegExp: RE operator has no operands"
	    | da (Op(rator, [re])) = da re
	    | da (Op(rator, re::res)) = mkOp(rator, da re, da(Op(rator, res)))
	    | da (Not re) = mkNot(da re)
	  in
	    da
	  end

    structure Map = RedBlackMapFn (
      struct
	type ord_key = re Vector.vector
	val compare = Vector.collate compare
      end)

    structure SISSet = RedBlackSetFn (
      struct
	type ord_key = SIS.set
	val compare = SIS.compare
      end)

(*
  (* yields the smallest partitioning of the alphabet that
   * "respects" the given sets.  if S is one of the sets
   * returned by compress, then it must be either disjoint
   * with or a subset of each of the sets in the sets
   * parameter.  see the implementation notes for more detail.
   *)
    fun compress sets = let
        (* performs partition of a set againt a list of sets,
         * assuming the list of sets is pairwise disjoint.
         *)
          fun part1 (set, []) =
	        if SIS.isEmpty set then []
		else [set]
            | part1 (set1, set2 :: ss) =
	        if SIS.isEmpty set1 then
		  set2 :: ss
		else let
                  val i = SIS.intersect (set1, set2)
	          in if SIS.isEmpty i then
		       (set2 :: (part1 (set1, ss)))
		     else let
		       val s1 = SIS.difference (set1, i)
                       val s2 = SIS.difference (set2, i)
                       val ss' = if SIS.isEmpty s1 then ss
				 else part1 (s1, ss)
                       in if SIS.isEmpty s2 then
			    (i :: ss')
			  else
			    (i :: s2 :: ss')
                       end
                  end
          in
            List.foldl part1 [] (SIS.universe::sets)
          end
*)

    fun cross (s1, s2) =
	SISSet.foldl (fn (s1elem, accum) =>
          SISSet.foldl (fn (s2elem, accum) =>
	      SISSet.add (accum, SIS.intersect (s1elem, s2elem)))
	    accum s2)
	  SISSet.empty s1

    val trivial = SISSet.singleton (SIS.universe)

    fun derivatives (res : re Vector.vector) = let
        (* compute approximate derivative classes *)
          fun ds Any = trivial
	    | ds None = trivial
	    | ds Epsilon = trivial
	    | ds (SymSet s) = SISSet.fromList [s, SIS.complement s]
	    | ds (Closure re) = ds re
	    | ds (Concat []) = trivial
	    | ds (Concat [re]) = ds re
	    | ds (Concat (re::res)) = if nullable re
                then cross (ds re, ds (Concat res))
		else ds re
	    | ds (Op(rator, res)) = foldl cross trivial (map ds res)
	    | ds (Not re) = ds re
	  val sets = Vector.foldl
		       (fn (re, sets) => cross (ds re, sets))
		         trivial res
(*	  val sets' = compress sets *)
	  fun classes ([], classMap) = Map.listItemsi classMap
	    | classes (set::sets, classMap) = let
	      (* use first element as representative of the equiv class *)
	        val (rep, _) = List.hd (SIS.intervals set)
	        val derivs = Vector.map (derivative rep) res
                in
		  case Map.find (classMap, derivs)
		   of NONE => classes (sets, Map.insert(classMap, derivs, set))
		    | SOME set' => let
			val map' = Map.insert(classMap, derivs, SIS.union (set, set'))
			in
			  classes (sets, map')
			end
		  (* end case *)
		end
	  fun classes ([], ls) = ls
	    | classes (set::sets, ls) = if SIS.isEmpty set
		then classes (sets, ls)
		else let
	          val (rep, _) = List.hd (SIS.intervals set)
	          val derivs = Vector.map (derivative rep) res
	          in
(*		    print (SISToString set ^ "\n"); *)
		    (derivs, set)::classes (sets, ls)
	          end
          in
(*            classes (sets', Map.empty) *)
(*            print "\n"; *)
            classes (SISSet.listItems sets, [])
          end

  end
